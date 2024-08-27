const std = @import("std");
const shared = @import("shared");
const arium = @import("arium");
const test_writer_mod = @import("test_writer.zig");
const config_mod = @import("config.zig");
const test_reporter = @import("test_reporter.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const SharedDiagnostics = shared.Diagnostics;
const Writer = shared.Writer;
const Tokenizer = arium.Tokenizer;
const IoHandler = arium.IoHandler;
const Parser = arium.Parser;
const Sema = arium.Sema;
const ManagedMemory = arium.ManagedMemory;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const error_reporter = arium.error_reporter;
const TestWriter = test_writer_mod.TestWriter;
const Config = config_mod.Config;

pub const Runner = struct {
    const Self = @This();

    const Error = error{
        OutOfMemory,
        TestFailure,
    };

    pub const DiagnosticEntry = struct {
        pub fn Mismatch(T: type) type {
            return struct {
                expected: T,
                actual: T,
            };
        }

        pub const FailureInfo = union(enum) {
            parser: Parser.Diagnostics,
            sema: Sema.Diagnostics,
            compiler: Compiler.Diagnostics,
            vm: Vm.Diagnostics,
            out_mismatch: Mismatch([]const u8),
            err_parser_mismatch: Mismatch(Parser.Diagnostics),
            memory_leak,
        };

        path: []const u8,
        failures: ArrayList(FailureInfo),

        pub fn deinit(self: *DiagnosticEntry, allocator: Allocator) void {
            for (self.failures.items) |*info| {
                switch (info.*) {
                    .parser => |*diags| {
                        diags.deinit();
                    },
                    .sema => |*diags| {
                        diags.deinit();
                    },
                    .compiler => |*diags| {
                        diags.deinit();
                    },
                    .vm => |*diags| {
                        diags.deinit();
                    },
                    .out_mismatch => |mismatch| {
                        allocator.free(mismatch.expected);
                        allocator.free(mismatch.actual);
                    },
                    .err_parser_mismatch => |*mismatch| {
                        mismatch.expected.deinit();
                        mismatch.actual.deinit();
                    },

                    .memory_leak,
                    => {},
                }
            }
        }
    };

    pub const Diagnostics = SharedDiagnostics(DiagnosticEntry);

    allocator: Allocator,
    tests: ArrayList(Config),

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .tests = ArrayList(Config).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.tests.items) |*config| {
            config.deinit();
        }

        self.tests.clearAndFree();
    }

    /// Takes ownership of path.
    /// Takes ownership of source.
    pub fn addTest(
        self: *Self,
        path: []const u8,
        source: []const u8,
        config_diags: *Config.Diagnostics,
    ) !void {
        const config = try Config.initFromOwnedPathAndSource(
            self.allocator,
            path,
            source,
            config_diags,
        );
        try self.tests.append(config);
    }

    pub fn runTests(
        self: *Self,
        allocator: Allocator,
        stdout_writer: *const Writer,
        stderr_writer: *const Writer,
    ) !void {
        var diags = Diagnostics.init(allocator);
        defer diags.deinit();

        var passed: u32 = 0;
        var failed: u32 = 0;

        stdout_writer.print("Running language tests...\n\n");

        for (self.tests.items) |*config| {
            stdout_writer.printf("Running '{s}'... ", .{config.path});

            self.runTest(config, &diags) catch |err| switch (err) {
                error.TestFailure => {
                    stdout_writer.print("FAILED\n");
                    failed += 1;

                    continue;
                },
                else => return err,
            };

            stdout_writer.print("PASSED\n");
            passed += 1;
        }

        stdout_writer.printf(
            "\nTests ran: {}\nTests passed: {}\nTests failed: {}\n",
            .{
                passed + failed,
                passed,
                failed,
            },
        );

        if (failed > 0) {
            test_reporter.reportRunnerDiagnostics(&diags, stderr_writer);
            return error.TestFailure;
        }
    }

    fn runTest(self: *Self, config: *Config, diags: *Diagnostics) Error!void {
        switch (config.kind) {
            .run => try self.runRunKind(config, diags),
        }
    }

    fn runRunKind(self: *Self, config: *Config, diags: *Diagnostics) Error!void {
        assert(config.kind == .run);

        var stdout_configwriter = TestWriter.init(self.allocator);
        defer stdout_configwriter.deinit();

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const allocator = gpa.allocator();

        var diag_entry = DiagnosticEntry{
            .path = config.path,
            .failures = ArrayList(DiagnosticEntry.FailureInfo).init(self.allocator),
        };

        var actual = Config.Expectations.init(self.allocator);
        defer actual.deinit();

        blk: {
            const stdout = stdout_configwriter.writer().any();

            const stdout_writer = Writer.init(&stdout);

            var tokenizer = Tokenizer.init(config.source);
            var parser = Parser.init(allocator);

            // allocate using TestRunner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var parser_diags = Parser.Diagnostics.init(self.allocator);

            const parsed_stmt = parser.parse(&tokenizer, &parser_diags) catch |err| switch (err) {
                error.ParseFailure => {
                    if (config.expectations.err_parser.getLen() > 0) {
                        actual.err_parser = parser_diags;
                    } else {
                        try diag_entry.failures.append(.{
                            .parser = parser_diags,
                        });
                    }
                    break :blk;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            defer parsed_stmt.destroy(allocator);

            var sema = Sema.init(allocator);

            // allocate using TestRunner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var sema_diags = Sema.Diagnostics.init(config.allocator);

            const sema_stmt = sema.analyze(parsed_stmt, &sema_diags) catch |err| switch (err) {
                error.SemaFailure => {
                    try diag_entry.failures.append(.{ .sema = sema_diags });
                    break :blk;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            defer sema_stmt.destroy(allocator);

            var memory = ManagedMemory.init(allocator);
            defer memory.deinit();

            // allocate using TestRunner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var compiler_diags = Compiler.Diagnostics.init(config.allocator);

            Compiler.compile(&memory, sema_stmt, &compiler_diags) catch |err| switch (err) {
                error.CompileFailure => {
                    try diag_entry.failures.append(.{ .compiler = compiler_diags });
                    break :blk;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };

            // allocate using TestRunner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var vm_diags = Vm.Diagnostics.init(self.allocator);

            Vm.interpret(&memory, &stdout_writer, &vm_diags, .{}) catch |err| switch (err) {
                error.Panic => {
                    try diag_entry.failures.append(.{ .vm = vm_diags });
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
        }

        switch (gpa.deinit()) {
            .leak => {
                try diag_entry.failures.append(.memory_leak);
            },
            .ok => {},
        }

        if (diag_entry.failures.items.len == 0) {
            try actual.out.appendSlice(stdout_configwriter.output.items);
            try self.verifyExpectations(&config.expectations, &actual, &diag_entry);
        }

        if (diag_entry.failures.items.len > 0) {
            try diags.add(diag_entry);
            return error.TestFailure;
        }
    }

    fn verifyExpectations(
        self: *Self,
        expectations: *const Config.Expectations,
        actuals: *const Config.Expectations,
        diag_entry: *DiagnosticEntry,
    ) !void {
        if (!verifyOut(&expectations.out, &actuals.out)) {
            try diag_entry.failures.append(.{
                .out_mismatch = .{
                    .expected = try self.allocator.dupe(
                        u8,
                        expectations.out.items,
                    ),
                    .actual = try self.allocator.dupe(
                        u8,
                        actuals.out.items,
                    ),
                },
            });
        }

        if (!verifyErrParser(&expectations.err_parser, &actuals.err_parser)) {
            try diag_entry.failures.append(.{
                .err_parser_mismatch = .{
                    .expected = try expectations.err_parser.clone(),
                    .actual = try actuals.err_parser.clone(),
                },
            });
        }
    }

    fn verifyErrParser(
        expected: *const Parser.Diagnostics,
        actual: *const Parser.Diagnostics,
    ) bool {
        if (expected.getLen() != actual.getLen()) {
            return false;
        }

        for (expected.getEntries(), actual.getEntries()) |expected_entry, actual_entry| {
            if (@intFromEnum(expected_entry.kind) != @intFromEnum(actual_entry.kind)) {
                return false;
            }

            if (expected_entry.position.line != actual_entry.position.line) {
                return false;
            }

            switch (expected_entry.kind) {
                .expected_end_token,
                => |token| if (token != actual_entry.kind.expected_end_token) {
                    return false;
                },

                .invalid_token,
                => |msg| if (!std.mem.eql(u8, msg, actual_entry.kind.invalid_token)) {
                    return false;
                },

                else => {},
            }
        }

        return true;
    }

    fn verifyOut(
        expected: *const ArrayList(u8),
        actual: *const ArrayList(u8),
    ) bool {
        return std.mem.eql(u8, expected.items, actual.items);
    }
};
