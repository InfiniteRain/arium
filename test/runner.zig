const std = @import("std");
const arium = @import("arium");
const shared = @import("shared");
const test_writer_mod = @import("test_writer.zig");
const config_mod = @import("config.zig");
const test_reporter = @import("test_reporter.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const Tokenizer = arium.Tokenizer;
const IoHandler = arium.IoHandler;
const Parser = arium.Parser;
const Sema = arium.Sema;
const SemaExpr = arium.SemaExpr;
const ManagedMemory = arium.ManagedMemory;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const SharedDiagnostics = shared.Diagnostics;
const Writer = shared.Writer;
const meta = shared.meta;
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
            err_sema_mismatch: Mismatch(Sema.Diagnostics),
            err_compiler_mismatch: Mismatch(Compiler.Diagnostics),
            err_vm_mismatch: Mismatch(Vm.Diagnostics),
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
                    .err_sema_mismatch => |*mismatch| {
                        mismatch.expected.deinit();
                        mismatch.actual.deinit();
                    },
                    .err_compiler_mismatch => |*mismatch| {
                        mismatch.expected.deinit();
                        mismatch.actual.deinit();
                    },
                    .err_vm_mismatch => |*mismatch| {
                        mismatch.expected.deinit();
                        mismatch.actual.deinit();
                    },
                    .memory_leak => {},
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
        var stdout_test_writer = TestWriter.init(self.allocator);
        defer stdout_test_writer.deinit();

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const test_allocator = gpa.allocator();

        var diag_entry = DiagnosticEntry{
            .path = config.path,
            .failures = ArrayList(DiagnosticEntry.FailureInfo).init(self.allocator),
        };

        var actual = Config.Expectations.init(self.allocator);
        defer actual.deinit();

        blk: {
            var tokenizer = Tokenizer.init(config.source);
            var parser = Parser.init(test_allocator);

            // allocate using Runner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var parser_diags = Parser.Diagnostics.init(self.allocator);

            const parsed_block = parser.parse(&tokenizer, &parser_diags) catch |err| switch (err) {
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
            defer parsed_block.destroy(test_allocator);

            if (config.kind == .parse) {
                break :blk;
            }

            var sema = Sema.init(test_allocator);

            // allocate using Runner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var sema_diags = Sema.Diagnostics.init(self.allocator);

            const sema_block = sema.analyze(parsed_block, &sema_diags) catch |err| switch (err) {
                error.SemaFailure => {
                    if (config.expectations.err_sema.getLen() > 0) {
                        actual.err_sema = sema_diags;
                    } else {
                        try diag_entry.failures.append(.{ .sema = sema_diags });
                    }
                    break :blk;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            defer sema_block.destroy(test_allocator);

            if (config.kind == .sema) {
                break :blk;
            }

            var memory = ManagedMemory.init(test_allocator);
            defer memory.deinit();

            // allocate using Runner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var compiler_diags = Compiler.Diagnostics.init(self.allocator);

            Compiler.compile(&memory, sema_block, &compiler_diags) catch |err| switch (err) {
                error.CompileFailure => {
                    if (config.expectations.err_compiler.getLen() > 0) {
                        actual.err_compiler = compiler_diags;
                    } else {
                        try diag_entry.failures.append(.{ .compiler = compiler_diags });
                    }
                    break :blk;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };

            if (config.kind == .compile) {
                break :blk;
            }

            const stdout = stdout_test_writer.writer().any();
            const stdout_writer = Writer.init(&stdout);

            // allocate using TestRunner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var vm_diags = Vm.Diagnostics.init(self.allocator);

            Vm.interpret(&memory, &stdout_writer, &vm_diags, .{}) catch |err| switch (err) {
                error.Panic => {
                    if (config.expectations.err_vm.getLen() > 0) {
                        actual.err_vm = vm_diags;
                    } else {
                        try diag_entry.failures.append(.{ .vm = vm_diags });
                    }
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
            try actual.out.appendSlice(stdout_test_writer.output.items);
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
                    .expected = try cloneDiags(&expectations.err_parser),
                    .actual = try cloneDiags(&actuals.err_parser),
                },
            });
        }

        if (!verifyErrSema(&expectations.err_sema, &actuals.err_sema)) {
            try diag_entry.failures.append(.{
                .err_sema_mismatch = .{
                    .expected = try cloneDiags(&expectations.err_sema),
                    .actual = try cloneDiags(&actuals.err_sema),
                },
            });
        }

        if (!verifyErrCompiler(&expectations.err_compiler, &actuals.err_compiler)) {
            try diag_entry.failures.append(.{
                .err_compiler_mismatch = .{
                    .expected = try cloneDiags(&expectations.err_compiler),
                    .actual = try cloneDiags(&actuals.err_compiler),
                },
            });
        }

        if (!verifyErrVm(&expectations.err_vm, &actuals.err_vm)) {
            try diag_entry.failures.append(.{
                .err_vm_mismatch = .{
                    .expected = try cloneDiags(&expectations.err_vm),
                    .actual = try cloneDiags(&actuals.err_vm),
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

        for (
            expected.getEntries(),
            actual.getEntries(),
        ) |expected_entry, actual_entry| {
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

                .expected_expression,
                .expected_left_paren_before_expr,
                .expected_right_paren_after_expr,
                .int_literal_overflows,
                => {},
            }
        }

        return true;
    }

    fn verifyErrSema(
        expected: *const Sema.Diagnostics,
        actual: *const Sema.Diagnostics,
    ) bool {
        if (expected.getLen() != actual.getLen()) {
            return false;
        }

        for (
            expected.getEntries(),
            actual.getEntries(),
        ) |expected_entry, actual_entry| {
            if (@intFromEnum(expected_entry.kind) != @intFromEnum(actual_entry.kind)) {
                return false;
            }

            if (expected_entry.position.line != actual_entry.position.line) {
                return false;
            }

            switch (expected_entry.kind) {
                .expected_expr_type,
                .unexpected_arithmetic_type,
                .unexpected_comparison_type,
                .unexpected_logical_type,
                .unexpected_logical_negation_type,
                .unexpected_arithmetic_negation_type,
                => |eval_type| {
                    const actual_type =
                        meta.getUnionValue(&actual_entry.kind, SemaExpr.EvalType);

                    if (!verifyEvalType(eval_type, actual_type)) {
                        return false;
                    }
                },

                .unexpected_operand_type,
                .unexpected_concat_type,
                .unexpected_equality_type,
                => |eval_type| {
                    const expected_left, const expected_right = eval_type;
                    const actual_left, const actual_right = meta.getUnionValue(
                        &actual_entry.kind,
                        Sema.DiagnosticEntry.EvalTypeTuple,
                    );

                    if (!verifyEvalType(expected_left, actual_left) or
                        !verifyEvalType(expected_right, actual_right))
                    {
                        return false;
                    }
                },
            }
        }

        return true;
    }

    fn verifyErrCompiler(
        expected: *const Compiler.Diagnostics,
        actual: *const Compiler.Diagnostics,
    ) bool {
        if (expected.getLen() != actual.getLen()) {
            return false;
        }

        for (
            expected.getEntries(),
            actual.getEntries(),
        ) |expected_entry, actual_entry| {
            if (expected_entry.kind != actual_entry.kind) {
                return false;
            }

            if (expected_entry.position.line != actual_entry.position.line) {
                return false;
            }
        }

        return true;
    }

    fn verifyErrVm(
        expected: *const Vm.Diagnostics,
        actual: *const Vm.Diagnostics,
    ) bool {
        if (expected.getLen() != actual.getLen()) {
            return false;
        }

        for (
            expected.getEntries(),
            actual.getEntries(),
        ) |expected_entry, actual_entry| {
            if (expected_entry.kind != actual_entry.kind) {
                return false;
            }

            if (expected_entry.position.line != actual_entry.position.line) {
                return false;
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

    fn verifyEvalType(
        expected: SemaExpr.EvalType,
        actual: SemaExpr.EvalType,
    ) bool {
        if (@intFromEnum(expected) != @intFromEnum(actual)) {
            return false;
        }

        switch (expected) {
            .obj => |obj| {
                if (obj != actual.obj) {
                    return false;
                }
            },

            .unit,
            .int,
            .float,
            .bool,
            .invalid,
            => {},
        }

        return true;
    }

    fn cloneDiags(
        diags: anytype,
    ) error{OutOfMemory}!@typeInfo(@TypeOf(diags)).Pointer.child {
        return .{
            .allocator = diags.allocator,
            .entries = try diags.entries.clone(),
        };
    }
};
