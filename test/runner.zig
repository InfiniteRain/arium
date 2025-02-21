const std = @import("std");
const arium = @import("arium");
const shared = @import("shared");
const test_writer_mod = @import("test_writer.zig");
const config_mod = @import("config.zig");
const test_reporter = @import("test_reporter.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const assert = std.debug.assert;
const comptimePrint = std.fmt.comptimePrint;
const Tokenizer = arium.Tokenizer;
const Loc = arium.Loc;
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
                source: []const u8,
            };
        }

        pub const OutMismatch = struct {
            expected: []const u8,
            actual: []const u8,
        };

        pub const Failure = union(enum) {
            parser: struct {
                diags: ArrayListUnmanaged(Parser.Diag),
                source: []const u8,
            },
            sema: struct {
                diags: Sema.Diags,
                source: []const u8,
            },
            compiler: struct {
                diags: Compiler.Diags,
                source: []const u8,
            },
            vm: struct {
                diags: Vm.Diags,
                source: []const u8,
            },
            out_mismatch: OutMismatch,
            err_parser_mismatch: Mismatch(ArrayListUnmanaged(Parser.Diag)),
            err_sema_mismatch: Mismatch(Sema.Diags),
            err_compiler_mismatch: Mismatch(Compiler.Diags),
            err_vm_mismatch: Mismatch(Vm.Diags),
            memory_leak,
        };

        path: []const u8,
        failures: ArrayList(Failure),

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
            for (self.failures.items) |*info| {
                switch (info.*) {
                    .parser => |*parse_failure| {
                        parse_failure.diags.deinit(allocator);
                        allocator.free(parse_failure.source);
                    },
                    .sema => |*sema_failure| {
                        sema_failure.diags.deinit();
                        allocator.free(sema_failure.source);
                    },
                    .compiler => |*compiler_failure| {
                        compiler_failure.diags.deinit();
                        allocator.free(compiler_failure.source);
                    },
                    .vm => |*vm_failure| {
                        vm_failure.diags.deinit();
                        allocator.free(vm_failure.source);
                    },
                    .out_mismatch => |mismatch| {
                        allocator.free(mismatch.expected);
                        allocator.free(mismatch.actual);
                    },
                    .err_parser_mismatch => |*mismatch| {
                        mismatch.expected.deinit(allocator);
                        mismatch.actual.deinit(allocator);
                        allocator.free(mismatch.source);
                    },
                    .err_sema_mismatch => |*mismatch| {
                        mismatch.expected.deinit();
                        mismatch.actual.deinit();
                        allocator.free(mismatch.source);
                    },
                    .err_compiler_mismatch => |*mismatch| {
                        mismatch.expected.deinit();
                        mismatch.actual.deinit();
                        allocator.free(mismatch.source);
                    },
                    .err_vm_mismatch => |*mismatch| {
                        mismatch.expected.deinit();
                        mismatch.actual.deinit();
                        allocator.free(mismatch.source);
                    },
                    .memory_leak => {},
                }
            }
        }
    };

    pub const Diags = SharedDiags(DiagEntry);

    const VerifiableTypes = .{
        Parser.Diag,
        Parser.Diag.Tag,
        Loc,
        Token.Tag,
        Sema.DiagEntry,
        Sema.DiagEntry.Kind,
        Sema.DiagEntry.ArityMismatch,
        Sema.DiagEntry.ArgTypeMismatch,
        SemaType,
        SemaType.Fn,
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
        source: [:0]const u8,
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
            .failures = ArrayList(DiagEntry.Failure).init(self.allocator),
        };

        var actual = Config.Expectations.init(self.allocator);
        defer actual.deinit();

        blk: {
            var arena_allocator = ArenaAllocator.init(test_allocator);
            defer arena_allocator.deinit();

            var tokenizer = Tokenizer.init(config.source);
            var parser = Parser.init(arena_allocator.allocator());

            var parser_diags: ArrayListUnmanaged(Parser.Diag) = .{};

            var ast = parser.parse(&tokenizer, &parser_diags) catch |err| switch (err) {
                error.ParseFailure => {
                    if (config.expectations.err_parser.items.len > 0) {
                        actual.err_parser = try parser_diags.clone(self.allocator);
                    } else {
                        try diag_entry.failures.append(.{
                            .parser = .{
                                .diags = try parser_diags.clone(self.allocator),
                                .source = try self.allocator.dupe(u8, config.source),
                            },
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

            const sema_fn = sema.analyze(&ast, &sema_diags) catch |err| switch (err) {
                error.SemaFailure => {
                    if (config.expectations.err_sema.getLen() > 0) {
                        actual.err_sema = sema_diags;
                    } else {
                        try diag_entry.failures.append(.{
                            .sema = .{
                                .diags = sema_diags,
                                .source = try self.allocator.dupe(u8, config.source),
                            },
                        });
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

            Compiler.compile(
                &memory,
                &arena_allocator,
                sema_fn,
                &compiler_diags,
            ) catch |err| switch (err) {
                error.CompileFailure => {
                    if (config.expectations.err_compiler.getLen() > 0) {
                        actual.err_compiler = compiler_diags;
                    } else {
                        try diag_entry.failures.append(.{
                            .compiler = .{
                                .diags = compiler_diags,
                                .source = try self.allocator.dupe(u8, config.source),
                            },
                        });
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
                        try diag_entry.failures.append(.{
                            .vm = .{
                                .diags = vm_diags,
                                .source = try self.allocator.dupe(u8, config.source),
                            },
                        });
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
            try self.verifyExpectations(
                config.source,
                &config.expectations,
                &actual,
                &diag_entry,
            );
        }

        if (diag_entry.failures.items.len > 0) {
            try diags.add(diag_entry);
            return error.TestFailure;
        }
    }

    fn verifyExpectations(
        self: *Self,
        source: []const u8,
        expectations: *const Config.Expectations,
        actuals: *const Config.Expectations,
        diag_entry: *DiagEntry,
    ) !void {
        if (!verifyValue(expectations.out, actuals.out, source)) {
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
            expectations.err_parser.items,
            actuals.err_parser.items,
            source,
        )) {
            try diag_entry.failures.append(.{
                .err_parser_mismatch = .{
                    .expected = try expectations.err_parser.clone(self.allocator),
                    .actual = try actuals.err_parser.clone(self.allocator),
                    .source = try self.allocator.dupe(u8, source),
                },
            });
        }

        if (!verifyValue(
            expectations.err_sema.entries,
            actuals.err_sema.entries,
            source,
        )) {
            try diag_entry.failures.append(.{
                .err_sema_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_sema),
                    .actual = try self.cloneDiags(&actuals.err_sema),
                    .source = try self.allocator.dupe(u8, source),
                },
            });
        }

        if (!verifyValue(
            expectations.err_compiler.entries,
            actuals.err_compiler.entries,
            source,
        )) {
            try diag_entry.failures.append(.{
                .err_compiler_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_compiler),
                    .actual = try self.cloneDiags(&actuals.err_compiler),
                    .source = try self.allocator.dupe(u8, source),
                },
            });
        }

        if (!verifyValue(
            expectations.err_vm.entries,
            actuals.err_vm.entries,
            source,
        )) {
            try diag_entry.failures.append(.{
                .err_vm_mismatch = .{
                    .expected = try self.cloneDiags(&expectations.err_vm),
                    .actual = try self.cloneDiags(&actuals.err_vm),
                    .source = try self.allocator.dupe(u8, source),
                },
            });
        }
    }

    fn verifyValue(expectation: anytype, actual: anytype, source: []const u8) bool {
        const Type = @TypeOf(expectation);
        const type_name = @typeName(Type);

        if (Type != @TypeOf(actual)) {
            @compileError("expectation and actual should be of the same type");
        }

        const type_info = @typeInfo(Type);

        if (Type == Loc) {
            const line, _ = actual.toLineCol(source);
            return expectation.index == line;
        }

        if (Type == []u8 or Type == []const u8) {
            return verifyString(expectation, actual);
        }

        if (comptime meta.isArrayList(Type)) {
            return verifyArray(expectation.items, actual.items, source);
        }

        if (type_info == .pointer and type_info.pointer.size == .one) {
            return verifyValue(expectation.*, actual.*, source);
        }

        if ((type_info == .array) or
            (type_info == .pointer and type_info.pointer.size == .slice))
        {
            return verifyArray(expectation, actual, source);
        }

        switch (type_info) {
            .void,
            => return true,

            .int,
            => return expectation == actual,

            .optional,
            => return verifyOptional(expectation, actual, source),

            .@"enum",
            => if (comptime meta.valueInTuple(Type, VerifiableTypes)) {
                return expectation == actual;
            } else {
                @compileError(comptimePrint(
                    "enum {s} isn't marked as verifiable",
                    .{type_name},
                ));
            },

            .@"union",
            => if (comptime meta.valueInTuple(Type, VerifiableTypes)) {
                return verifyUnion(expectation, actual, source);
            } else {
                @compileError(comptimePrint(
                    "union {s} isn't marked as verifiable",
                    .{type_name},
                ));
            },

            .@"struct",
            => if (comptime meta.valueInTuple(Type, VerifiableTypes)) {
                return verifyStruct(expectation, actual, source);
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

    fn verifyStruct(
        expectation: anytype,
        actual: anytype,
        source: []const u8,
    ) bool {
        inline for (@typeInfo(@TypeOf(expectation)).@"struct".fields) |field| {
            if (!verifyValue(
                @field(expectation, field.name),
                @field(actual, field.name),
                source,
            )) {
                return false;
            }
        }

        return true;
    }

    fn verifyUnion(
        expectation: anytype,
        actual: anytype,
        source: []const u8,
    ) bool {
        const Type = @TypeOf(expectation);
        const type_info = @typeInfo(Type);
        const Tag = std.meta.Tag(Type);

        if (@as(Tag, expectation) != @as(Tag, actual)) {
            return false;
        }

        inline for (type_info.@"union".fields) |field| {
            if (!std.mem.eql(u8, @tagName(expectation), field.name)) {
                comptime continue;
            }

            return verifyValue(
                @field(expectation, field.name),
                @field(actual, field.name),
                source,
            );
        }

        unreachable;
    }

    fn verifyArray(
        expectation: anytype,
        actual: anytype,
        source: []const u8,
    ) bool {
        if (expectation.len != actual.len) {
            return false;
        }

        for (expectation, actual) |expected_item, actual_item| {
            if (!verifyValue(expected_item, actual_item, source)) {
                return false;
            }
        }

        return true;
    }

    fn verifyString(expectation: anytype, actual: anytype) bool {
        return std.mem.eql(u8, expectation, actual);
    }

    fn verifyOptional(
        expectation: anytype,
        actual: anytype,
        source: []const u8,
    ) bool {
        if (expectation == null and actual == null) {
            return true;
        }

        if (expectation == null or actual == null) {
            return false;
        }

        return verifyValue(expectation.?, actual.?, source);
    }

    fn cloneDiags(
        self: *Self,
        diags: anytype,
    ) error{OutOfMemory}!@typeInfo(@TypeOf(diags)).pointer.child {
        const DiagsType = @typeInfo(@TypeOf(diags)).pointer.child;
        var clone = DiagsType.init(self.allocator);

        for (diags.getEntries()) |diag| {
            try clone.add(meta.spread(diag, .{
                .kind = try shared.clone.createClone(
                    self.allocator,
                    diag.kind,
                    .{
                        Sema.DiagEntry.Kind,
                        Sema.DiagEntry.SemaTypeTuple,
                        Sema.DiagEntry.ArityMismatch,
                        Sema.DiagEntry.ArgTypeMismatch,
                        SemaType,
                        SemaType.Fn,
                    },
                ),
            }));
        }

        return clone;
    }
};
