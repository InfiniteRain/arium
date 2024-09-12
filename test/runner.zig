const std = @import("std");
const arium = @import("arium");
const shared = @import("shared");
const test_writer_mod = @import("test_writer.zig");
const config_mod = @import("config.zig");
const test_reporter = @import("test_reporter.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const comptimePrint = std.fmt.comptimePrint;
const Tokenizer = arium.Tokenizer;
const Position = arium.Position;
const Token = arium.Token;
const IoHandler = arium.IoHandler;
const Parser = arium.Parser;
const Sema = arium.Sema;
const SemaExpr = arium.SemaExpr;
const SemaType = arium.SemaType;
const ManagedMemory = arium.ManagedMemory;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const SharedDiags = shared.Diags;
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

    pub const DiagEntry = struct {
        pub fn Mismatch(T: type) type {
            return struct {
                expected: T,
                actual: T,
            };
        }

        pub const FailureInfo = union(enum) {
            parser: Parser.Diags,
            sema: Sema.Diags,
            compiler: Compiler.Diags,
            vm: Vm.Diags,
            out_mismatch: Mismatch([]const u8),
            err_parser_mismatch: Mismatch(Parser.Diags),
            err_sema_mismatch: Mismatch(Sema.Diags),
            err_compiler_mismatch: Mismatch(Compiler.Diags),
            err_vm_mismatch: Mismatch(Vm.Diags),
            memory_leak,
        };

        path: []const u8,
        failures: ArrayList(FailureInfo),

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
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

    pub const Diags = SharedDiags(DiagEntry);

    const VerifiableTypes = .{
        Parser.DiagEntry,
        Parser.DiagEntry.Kind,
        Position,
        Token.Kind,
        Sema.DiagEntry,
        Sema.DiagEntry.Kind,
        SemaType,
        Sema.DiagEntry.SemaTypeTuple,
        Compiler.DiagEntry,
        Compiler.DiagEntry.Kind,
        Vm.DiagEntry,
        Vm.DiagEntry.Kind,
    };

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
        config_diags: *Config.Diags,
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
        var diags = Diags.init(allocator);
        defer diags.deinit();

        const start_ms = std.time.milliTimestamp();
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
            "\nTests ran: {}\nTests passed: {}\nTests failed: {}\nTime elapsed (ms): {}\n",
            .{
                passed + failed,
                passed,
                failed,
                std.time.milliTimestamp() - start_ms,
            },
        );

        if (failed > 0) {
            test_reporter.reportRunnerDiags(&diags, stderr_writer);
            return error.TestFailure;
        }
    }

    fn runTest(self: *Self, config: *Config, diags: *Diags) Error!void {
        var stdout_test_writer = TestWriter.init(self.allocator);
        defer stdout_test_writer.deinit();

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const test_allocator = gpa.allocator();

        var diag_entry = DiagEntry{
            .path = config.path,
            .failures = ArrayList(DiagEntry.FailureInfo).init(self.allocator),
        };

        var actual = Config.Expectations.init(self.allocator);
        defer actual.deinit();

        blk: {
            var arena_allocator = ArenaAllocator.init(test_allocator);
            defer arena_allocator.deinit();

            var tokenizer = Tokenizer.init(config.source);
            var parser = Parser.init(&arena_allocator);

            // allocate using Runner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var parser_diags = Parser.Diags.init(self.allocator);

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

            if (config.kind == .parse) {
                break :blk;
            }

            var sema = Sema.init(&arena_allocator);

            // allocate using Runner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var sema_diags = Sema.Diags.init(self.allocator);

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

            if (config.kind == .sema) {
                break :blk;
            }

            var memory = ManagedMemory.init(test_allocator);
            defer memory.deinit();

            // allocate using Runner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var compiler_diags = Compiler.Diags.init(self.allocator);

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
            var vm_diags = Vm.Diags.init(self.allocator);

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
        diag_entry: *DiagEntry,
    ) !void {
        if (!verifyValue(expectations.out, actuals.out)) {
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

        if (!verifyValue(
            expectations.err_parser.entries,
            actuals.err_parser.entries,
        )) {
            try diag_entry.failures.append(.{
                .err_parser_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_parser),
                    .actual = try self.cloneDiags(&actuals.err_parser),
                },
            });
        }

        if (!verifyValue(
            expectations.err_sema.entries,
            actuals.err_sema.entries,
        )) {
            try diag_entry.failures.append(.{
                .err_sema_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_sema),
                    .actual = try self.cloneDiags(&actuals.err_sema),
                },
            });
        }

        if (!verifyValue(
            expectations.err_compiler.entries,
            actuals.err_compiler.entries,
        )) {
            try diag_entry.failures.append(.{
                .err_compiler_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_compiler),
                    .actual = try self.cloneDiags(&actuals.err_compiler),
                },
            });
        }

        if (!verifyValue(
            expectations.err_vm.entries,
            actuals.err_vm.entries,
        )) {
            try diag_entry.failures.append(.{
                .err_vm_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_vm),
                    .actual = try self.cloneDiags(&actuals.err_vm),
                },
            });
        }
    }

    fn verifyValue(expectation: anytype, actual: anytype) bool {
        const Type = @TypeOf(expectation);
        const type_name = @typeName(Type);

        if (Type != @TypeOf(actual)) {
            @compileError("expectation and actual should be of the same type");
        }

        const type_info = @typeInfo(Type);

        if (Type == Position) {
            // special case here as we ignore the column for now.
            return expectation.line == actual.line;
        }

        if (Type == []u8 or Type == []const u8) {
            return verifyString(expectation, actual);
        }

        if (comptime meta.isArrayList(Type)) {
            return verifyArrayList(expectation, actual);
        }

        switch (type_info) {
            .Void,
            => return true,

            .Int,
            => return expectation == actual,

            .Enum,
            => if (comptime meta.typeInTuple(Type, VerifiableTypes)) {
                return expectation == actual;
            } else {
                @compileError(comptimePrint(
                    "enum {s} isn't marked as verifiable",
                    .{type_name},
                ));
            },

            .Union,
            => if (comptime meta.typeInTuple(Type, VerifiableTypes)) {
                return verifyUnion(expectation, actual);
            } else {
                @compileError(comptimePrint(
                    "union {s} isn't marked as verifiable",
                    .{type_name},
                ));
            },

            .Struct,
            => if (comptime meta.typeInTuple(Type, VerifiableTypes)) {
                return verifyStruct(expectation, actual);
            } else {
                @compileError(comptimePrint(
                    "struct {s} isn't marked as verifiable",
                    .{type_name},
                ));
            },

            else => @compileError(comptimePrint(
                "no verification exists for {s} / {s}",
                .{ type_name, @tagName(type_info) },
            )),
        }
    }

    fn verifyStruct(expectation: anytype, actual: anytype) bool {
        inline for (@typeInfo(@TypeOf(expectation)).Struct.fields) |field| {
            if (!verifyValue(@field(expectation, field.name), @field(actual, field.name))) {
                return false;
            }
        }

        return true;
    }

    fn verifyUnion(expectation: anytype, actual: anytype) bool {
        const Type = @TypeOf(expectation);
        const type_info = @typeInfo(Type);
        const Tag = std.meta.Tag(Type);

        if (@as(Tag, expectation) != @as(Tag, actual)) {
            return false;
        }

        inline for (type_info.Union.fields) |field| {
            if (!std.mem.eql(u8, @tagName(expectation), field.name)) {
                comptime continue;
            }

            return verifyValue(
                @field(expectation, field.name),
                @field(actual, field.name),
            );
        }

        unreachable;
    }

    fn verifyArrayList(expectation: anytype, actual: anytype) bool {
        if (expectation.items.len != actual.items.len) {
            return false;
        }

        for (expectation.items, actual.items) |expected_item, actual_item| {
            if (!verifyValue(expected_item, actual_item)) {
                return false;
            }
        }

        return true;
    }

    fn verifyString(expectation: anytype, actual: anytype) bool {
        return std.mem.eql(u8, expectation, actual);
    }

    fn cloneDiags(
        self: *Self,
        diags: anytype,
    ) error{OutOfMemory}!@typeInfo(@TypeOf(diags)).Pointer.child {
        const DiagsType = @typeInfo(@TypeOf(diags)).Pointer.child;
        var clone = DiagsType.init(self.allocator);

        for (diags.getEntries()) |diag| {
            try clone.add(meta.spread(diag, .{
                .kind = try shared.clone.createClone(
                    self.allocator,
                    diag.kind,
                    .{
                        Parser.DiagEntry.Kind,
                        Sema.DiagEntry.Kind,
                        Sema.DiagEntry.SemaTypeTuple,
                    },
                ),
            }));
        }

        return clone;
    }
};
