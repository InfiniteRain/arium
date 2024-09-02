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
const Tokenizer = arium.Tokenizer;
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
                    .expected = try self.cloneDiags(&expectations.err_parser),
                    .actual = try self.cloneDiags(&actuals.err_parser),
                },
            });
        }

        if (!verifyErrSema(&expectations.err_sema, &actuals.err_sema)) {
            try diag_entry.failures.append(.{
                .err_sema_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_sema),
                    .actual = try self.cloneDiags(&actuals.err_sema),
                },
            });
        }

        if (!verifyErrCompiler(&expectations.err_compiler, &actuals.err_compiler)) {
            try diag_entry.failures.append(.{
                .err_compiler_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_compiler),
                    .actual = try self.cloneDiags(&actuals.err_compiler),
                },
            });
        }

        if (!verifyErrVm(&expectations.err_vm, &actuals.err_vm)) {
            try diag_entry.failures.append(.{
                .err_vm_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_vm),
                    .actual = try self.cloneDiags(&actuals.err_vm),
                },
            });
        }
    }

    fn verifyErrParser(
        expected: *const Parser.Diags,
        actual: *const Parser.Diags,
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
                .expected_name,
                .expected_equal_after_name,
                .invalid_assignment_target,
                .expected_type,
                => {},
            }
        }

        return true;
    }

    fn verifyErrSema(
        expected: *const Sema.Diags,
        actual: *const Sema.Diags,
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
                => |sema_type| {
                    const actual_type =
                        meta.getUnionValue(&actual_entry.kind, SemaType);

                    if (!verifySemaType(sema_type, actual_type)) {
                        return false;
                    }
                },

                .unexpected_operand_type,
                .unexpected_concat_type,
                .unexpected_equality_type,
                .unexpected_assignment_type,
                => |sema_type| {
                    const expected_left, const expected_right = sema_type;
                    const actual_left, const actual_right = meta.getUnionValue(
                        &actual_entry.kind,
                        Sema.DiagEntry.SemaTypeTuple,
                    );

                    if (!verifySemaType(expected_left, actual_left) or
                        !verifySemaType(expected_right, actual_right))
                    {
                        return false;
                    }
                },

                .value_not_found,
                .immutable_mutation,
                .type_not_found,
                .value_not_assigned,
                => |name| {
                    const expected_name = name;
                    const actual_name = meta.getUnionValue(
                        &actual_entry.kind,
                        []const u8,
                    );

                    if (!std.mem.eql(u8, expected_name, actual_name)) {
                        return false;
                    }
                },

                .too_many_locals,
                => {},
            }
        }

        return true;
    }

    fn verifyErrCompiler(
        expected: *const Compiler.Diags,
        actual: *const Compiler.Diags,
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
        expected: *const Vm.Diags,
        actual: *const Vm.Diags,
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

    fn verifySemaType(
        expected: SemaType,
        actual: SemaType,
    ) bool {
        if (@intFromEnum(expected) != @intFromEnum(actual)) {
            return false;
        }

        switch (expected) {
            .unit,
            .int,
            .float,
            .bool,
            .string,
            .invalid,
            => {},
        }

        return true;
    }

    fn cloneDiags(
        self: *Self,
        diags: anytype,
    ) error{OutOfMemory}!@typeInfo(@TypeOf(diags)).Pointer.child {
        const DiagsType = @typeInfo(@TypeOf(diags)).Pointer.child;
        var clone = DiagsType.init(self.allocator);

        for (diags.getEntries()) |diag| {
            switch (DiagsType) {
                Parser.Diags => try clone.add(meta.spread(diag, .{
                    .kind = switch (diag.kind) {
                        .invalid_token,
                        => |msg| Parser.DiagEntry.Kind{ .invalid_token = try self.allocator.dupe(u8, msg) },

                        .expected_end_token,
                        .expected_expression,
                        .expected_left_paren_before_expr,
                        .expected_right_paren_after_expr,
                        .int_literal_overflows,
                        .expected_name,
                        .expected_equal_after_name,
                        .invalid_assignment_target,
                        .expected_type,
                        => diag.kind,
                    },
                })),
                Sema.Diags => try clone.add(meta.spread(diag, .{
                    .kind = switch (diag.kind) {
                        .value_not_found,
                        => |name| Sema.DiagEntry.Kind{ .value_not_found = try self.allocator.dupe(u8, name) },

                        .immutable_mutation,
                        => |name| Sema.DiagEntry.Kind{ .immutable_mutation = try self.allocator.dupe(u8, name) },

                        .type_not_found,
                        => |name| Sema.DiagEntry.Kind{ .type_not_found = try self.allocator.dupe(u8, name) },

                        .value_not_assigned,
                        => |name| Sema.DiagEntry.Kind{ .value_not_assigned = try self.allocator.dupe(u8, name) },

                        .expected_expr_type,
                        .unexpected_arithmetic_type,
                        .unexpected_operand_type,
                        .unexpected_concat_type,
                        .unexpected_equality_type,
                        .unexpected_comparison_type,
                        .unexpected_logical_type,
                        .unexpected_logical_negation_type,
                        .unexpected_arithmetic_negation_type,
                        .too_many_locals,
                        .unexpected_assignment_type,
                        => diag.kind,
                    },
                })),
                Compiler.Diags => try clone.add(meta.spread(diag, .{
                    .kind = switch (diag.kind) {
                        .too_many_constants,
                        .too_many_branch_jumps,
                        .jump_too_big,
                        => diag.kind,
                    },
                })),
                Vm.Diags => try clone.add(meta.spread(diag, .{
                    .kind = switch (diag.kind) {
                        .assertion_fail,
                        => diag.kind,
                    },
                })),
                else => unreachable,
            }
        }

        return clone;
    }
};
