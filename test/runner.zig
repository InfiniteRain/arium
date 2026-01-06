const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const meta = std.meta;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;

const arium = @import("arium");
const FixedArray = arium.FixedArray;
const Output = arium.Output;
const Span = arium.Span;
const AriumParser = arium.Parser;
const Tokenizer = arium.Tokenizer;
const Parser = arium.Parser;
const InternPool = arium.InternPool;
const Sema = arium.Sema;
const Memory = arium.Memory;
const Compiler = arium.Compiler;
const Vm = arium.Vm;

const checkForFixedArray = @import("util.zig").checkForFixedArray;
const constants = @import("constants.zig");
const test_parser_mod = @import("test_parser.zig");
const TestParser = test_parser_mod.TestParser;
const TestSetup = test_parser_mod.TestSetup;
const TestWriter = @import("test_writer.zig").TestWriter;

pub const Runner = struct {
    allocator: Allocator,
    diags: *Diags,
    scratch: *Scratch,
    test_scratch: *TestParser.Scratch,

    pub const Error = error{RunTestFailure} || Allocator.Error;

    pub const Diags = struct {
        entries: ArrayList(Entry),
        strings: ArrayList(u8),
        expects: ArrayList(TestSetup.Expect),
        actuals: ArrayList(Actual),
        test_parser_diag_entries: ArrayList(TestParser.Diags.Entry),
        ip_items: MultiArrayList(InternPool.Item),
        ip_extra: ArrayList(u32),

        pub const Entry = struct {
            file_path: Span(u8),
            tag: Tag,

            pub const Tag = union(enum) {
                file_read_failure,
                test_parse_failure: TestParseFailure,
                validation_failure: ValidationFailure,

                pub const ValidationFailure = struct {
                    expects: Span(TestSetup.Expect),
                    actuals: Span(Actual),
                    source: Span(u8),
                    ip_items: Span(void),
                    ip_extra: Span(u32),
                    ip_strings: Span(u8),
                };

                pub const TestParseFailure = struct {
                    entries: Span(TestParser.Diags.Entry),
                    source: Span(u8),
                };
            };
        };

        pub const empty: Diags = .{
            .entries = .empty,
            .strings = .empty,
            .expects = .empty,
            .actuals = .empty,
            .test_parser_diag_entries = .empty,
            .ip_items = .empty,
            .ip_extra = .empty,
        };

        pub fn deinit(self: *Diags, allocator: Allocator) void {
            self.entries.deinit(allocator);
            self.strings.deinit(allocator);
            self.expects.deinit(allocator);
            self.actuals.deinit(allocator);
            self.test_parser_diag_entries.deinit(allocator);
            self.ip_items.deinit(allocator);
            self.ip_extra.deinit(allocator);
        }

        fn appendFileReadFailure(
            self: *Diags,
            allocator: Allocator,
            file_path: []const u8,
        ) Allocator.Error!void {
            try self.entries.append(allocator, .{
                .file_path = try self.appendString(allocator, file_path),
                .tag = .file_read_failure,
            });
        }

        fn appendTestParseFailure(
            self: *Diags,
            allocator: Allocator,
            file_path: []const u8,
            test_parser_diag_entries: []const TestParser.Diags.Entry,
            source: [:0]const u8,
        ) Allocator.Error!void {
            const test_parser_diag_entries_top =
                self.test_parser_diag_entries.items.len;

            try self.test_parser_diag_entries.appendSlice(
                allocator,
                test_parser_diag_entries,
            );

            try self.entries.append(allocator, .{
                .file_path = try self.appendString(allocator, file_path),
                .tag = .{ .test_parse_failure = .{
                    .entries = .init(
                        test_parser_diag_entries_top,
                        self.test_parser_diag_entries.items.len,
                    ),
                    .source = try self.appendString(allocator, source),
                } },
            });
        }

        fn appendExpectationMismatch(
            self: *Diags,
            allocator: Allocator,
            file_path: []const u8,
            source: [:0]const u8,
            intern_pool: *const InternPool,
            scratch: *const Scratch,
            expects: []const TestSetup.Expect,
            actuals: []const Actual,
        ) Allocator.Error!void {
            const expects_top = self.expects.items.len;
            const actuals_top = self.actuals.items.len;
            const ip_items_top = self.ip_items.len;
            const ip_extra_top = self.ip_extra.items.len;

            try self.expects.ensureUnusedCapacity(allocator, expects.len);

            for (expects) |expect| {
                switch (expect) {
                    .out => |out| self.expects.appendAssumeCapacity(.{
                        .out = try self.appendString(
                            allocator,
                            out.toSlice(source),
                        ),
                    }),

                    .parse_err,
                    .sema_err,
                    .compile_err,
                    .vm_err,
                    => self.expects.appendAssumeCapacity(expect),
                }
            }

            try self.actuals.ensureUnusedCapacity(allocator, actuals.len);

            for (actuals) |actual| {
                switch (actual) {
                    .out => |out| self.actuals.appendAssumeCapacity(.{
                        .out = try self.appendString(
                            allocator,
                            out.toSlice(scratch.strings.items),
                        ),
                    }),

                    .parse_err,
                    .sema_err,
                    .compile_err,
                    .vm_err,
                    => self.actuals.appendAssumeCapacity(actual),
                }
            }

            try self.ip_items.ensureUnusedCapacity(
                allocator,
                intern_pool.items.len,
            );

            for (0..intern_pool.items.len) |index| {
                self.ip_items.appendAssumeCapacity(
                    intern_pool.items.get(index),
                );
            }

            try self.ip_extra.appendSlice(allocator, intern_pool.extra.items);

            const ip_strings_span = try self.appendString(
                allocator,
                intern_pool.strings.items,
            );

            try self.entries.append(allocator, .{
                .file_path = try self.appendString(allocator, file_path),
                .tag = .{
                    .validation_failure = .{
                        .expects = .init(expects_top, self.expects.items.len),
                        .actuals = .init(actuals_top, self.actuals.items.len),
                        .source = try self.appendString(allocator, source),

                        .ip_items = .init(ip_items_top, self.ip_items.len),
                        .ip_extra = .init(
                            ip_extra_top,
                            self.ip_extra.items.len,
                        ),
                        .ip_strings = ip_strings_span,
                    },
                },
            });
        }

        fn appendString(
            self: *Diags,
            allocator: Allocator,
            string: []const u8,
        ) Allocator.Error!Span(u8) {
            const strings_top = self.strings.items.len;
            try self.strings.appendSlice(allocator, string);
            return .init(strings_top, self.strings.items.len);
        }
    };

    pub const Scratch = struct {
        actuals: ArrayList(Actual),
        strings: ArrayList(u8),

        pub const empty: Scratch = .{
            .actuals = .empty,
            .strings = .empty,
        };

        pub fn deinit(self: *Scratch, allocator: Allocator) void {
            self.actuals.deinit(allocator);
            self.strings.deinit(allocator);
        }

        fn appendOut(
            self: *Scratch,
            allocator: Allocator,
            string: []const u8,
        ) Allocator.Error!void {
            const strings_top = self.strings.items.len;
            try self.strings.appendSlice(allocator, string);
            try self.actuals.append(allocator, .{
                .out = .init(strings_top, self.strings.items.len),
            });
        }
    };

    pub const Actual = union(enum) {
        parse_err: AriumParser.Diags.Entry,
        sema_err: Sema.Diags.Entry,
        compile_err: Compiler.Diags.Entry,
        vm_err: Vm.Diags.Entry,
        out: Span(u8),
    };

    pub fn init(
        allocator: Allocator,
        diags: *Diags,
        scratch: *Scratch,
        test_scratch: *TestParser.Scratch,
    ) Runner {
        return .{
            .allocator = allocator,
            .diags = diags,
            .scratch = scratch,
            .test_scratch = test_scratch,
        };
    }

    pub fn runTest(
        self: *const Runner,
        file_path: []const u8,
    ) Error!void {
        const actuals_top = self.scratch.actuals.items.len;
        defer self.scratch.actuals.shrinkRetainingCapacity(actuals_top);

        const source = self.readFileAlloc(file_path) catch |err|
            switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.FileReadFailure => {
                    try self.diags.appendFileReadFailure(
                        self.allocator,
                        file_path,
                    );
                    return error.RunTestFailure;
                },
            };
        defer self.allocator.free(source);

        var intern_pool = try InternPool.init(self.allocator);
        defer intern_pool.deinit(self.allocator);

        var test_parser_diags: TestParser.Diags = .empty;
        defer test_parser_diags.deinit(self.allocator);

        var setup = TestParser.parse(
            self.allocator,
            source,
            &intern_pool,
            &test_parser_diags,
            self.test_scratch,
        ) catch |err| switch (err) {
            error.TestParseFailure => {
                try self.diags.appendTestParseFailure(
                    self.allocator,
                    file_path,
                    test_parser_diags.entries.items,
                    source,
                );
                return error.RunTestFailure;
            },
            else => |run_test_error| return run_test_error,
        };
        defer setup.deinit(self.allocator);

        try self.run(&setup, source, &intern_pool);

        if (!self.validate(&setup, source)) {
            return self.expectationMismatch(
                &setup,
                file_path,
                source,
                &intern_pool,
            );
        }
    }

    fn run(
        self: *const Runner,
        setup: *const TestSetup,
        source: [:0]const u8,
        intern_pool: *InternPool,
    ) Allocator.Error!void {
        var tokenizer = Tokenizer.init(source);

        var parser_diags: Parser.Diags = .empty;
        defer parser_diags.deinit(self.allocator);

        var parser_scratch: Parser.Scratch = .empty;
        defer parser_scratch.deinit(self.allocator);

        var ast = Parser.parse(
            self.allocator,
            &tokenizer,
            &parser_diags,
            &parser_scratch,
        ) catch |err| switch (err) {
            error.ParseFailure => {
                try self.scratch.actuals.ensureUnusedCapacity(
                    self.allocator,
                    parser_diags.entries.items.len,
                );

                for (parser_diags.entries.items) |entry| {
                    self.scratch.actuals.appendAssumeCapacity(.{
                        .parse_err = .{
                            .tag = entry.tag,
                            .loc = entry.loc,
                        },
                    });
                }

                return;
            },
            else => |run_test_error| return run_test_error,
        };
        defer ast.deinit(self.allocator);

        if (setup.kind == .parse) {
            return;
        }

        var sema_diags: Sema.Diags = .empty;
        defer sema_diags.deinit(self.allocator);

        var sema_scratch: Sema.Scratch = .empty;
        defer sema_scratch.deinit(self.allocator);

        var air = Sema.analyze(
            self.allocator,
            source,
            intern_pool,
            &ast,
            &sema_diags,
            &sema_scratch,
        ) catch |err| switch (err) {
            error.AnalyzeFailure => {
                try self.scratch.actuals.ensureUnusedCapacity(
                    self.allocator,
                    sema_diags.entries.items.len,
                );

                for (sema_diags.entries.items) |entry| {
                    self.scratch.actuals.appendAssumeCapacity(.{
                        .sema_err = entry,
                    });
                }

                return;
            },
            else => |run_test_error| return run_test_error,
        };
        defer air.deinit(self.allocator);

        if (setup.kind == .sema) {
            return;
        }

        var memory = Memory.init(self.allocator);
        defer memory.deinit();

        var compiler_diags: Compiler.Diags = .empty;
        defer compiler_diags.deinit(self.allocator);

        var compiler_scratch: Compiler.Scratch = .empty;
        defer compiler_scratch.deinit(self.allocator);

        var module = Compiler.compile(
            self.allocator,
            &memory,
            intern_pool,
            &air,
            &compiler_diags,
            &compiler_scratch,
            .release,
        ) catch |err| switch (err) {
            error.CompileFailure => {
                try self.scratch.actuals.ensureUnusedCapacity(
                    self.allocator,
                    compiler_diags.entries.items.len,
                );

                for (compiler_diags.entries.items) |entry| {
                    self.scratch.actuals.appendAssumeCapacity(.{
                        .compile_err = entry,
                    });
                }

                return;
            },
            else => |run_test_error| return run_test_error,
        };
        defer module.deinit(self.allocator);

        if (setup.kind == .compile) {
            return;
        }

        var vm_diags: Vm.Diags = .empty;
        defer vm_diags.deinit(self.allocator);

        var output_bytes: ArrayList(u8) = .empty;
        defer output_bytes.deinit(self.allocator);

        var test_writer = TestWriter.init(self.allocator, &output_bytes);
        const output = Output.init(&test_writer.interface);

        Vm.interpret(&memory, &module, &output, &vm_diags, null) catch |err|
            switch (err) {
                error.Panic => {
                    if (vm_diags.entry) |entry| {
                        self.scratch.actuals.appendAssumeCapacity(.{
                            .vm_err = entry,
                        });
                    }

                    return;
                },
                else => |run_test_error| return run_test_error,
            };

        var splitIterator = mem.splitScalar(u8, output_bytes.items, '\n');
        var addedOut = false;

        while (splitIterator.next()) |line| {
            try self.scratch.appendOut(self.allocator, line);
            addedOut = true;
        }

        if (addedOut and self.scratch.actuals.getLast().out.len == 0) {
            // removing last blank line from out expects
            _ = self.scratch.actuals.pop();
        }
    }

    fn validate(
        self: *const Runner,
        setup: *const TestSetup,
        source: [:0]const u8,
    ) bool {
        const expects = setup.expects.items;
        const actuals = self.scratch.actuals.items;

        if (expects.len != actuals.len) {
            return false;
        }

        for (expects, actuals) |expect, actual| {
            if (!mem.eql(u8, @tagName(expect), @tagName(actual))) {
                return false;
            }

            const is_match = switch (expect) {
                .parse_err,
                => |parse_err| validateValue(
                    parse_err.tag,
                    actual.parse_err.tag,
                ) and validateLoc(source, parse_err.loc, actual.parse_err.loc),

                .sema_err,
                => |sema_err| validateValue(
                    sema_err.tag,
                    actual.sema_err.tag,
                ) and validateLoc(source, sema_err.loc, actual.sema_err.loc),

                .compile_err,
                => |compile_err| validateValue(
                    compile_err.tag,
                    actual.compile_err.tag,
                ) and
                    validateLoc(
                        source,
                        compile_err.loc,
                        actual.compile_err.loc,
                    ),

                .vm_err,
                => |vm_err| validateValue(vm_err.tag, actual.vm_err.tag) and
                    validateLoc(source, vm_err.loc, actual.vm_err.loc),

                .out,
                => |out| mem.eql(
                    u8,
                    out.toSlice(source),
                    actual.out.toSlice(self.scratch.strings.items),
                ),
            };

            if (!is_match) {
                return false;
            }
        }

        return true;
    }

    fn expectationMismatch(
        self: *const Runner,
        setup: *const TestSetup,
        file_path: []const u8,
        source: [:0]const u8,
        intern_pool: *const InternPool,
    ) Error {
        const expects = setup.expects.items;
        const actuals = self.scratch.actuals.items;

        try self.diags.appendExpectationMismatch(
            self.allocator,
            file_path,
            source,
            intern_pool,
            self.scratch,
            expects,
            actuals,
        );

        return error.RunTestFailure;
    }

    fn readFileAlloc(
        self: *const Runner,
        file_path: []const u8,
    ) (error{FileReadFailure} || Allocator.Error)![:0]const u8 {
        var tests_dir = fs.cwd().openDir(constants.tests_dir, .{}) catch
            return error.FileReadFailure;
        defer tests_dir.close();

        const file = tests_dir.openFile(
            file_path,
            .{ .mode = .read_only },
        ) catch return error.FileReadFailure;
        defer file.close();

        var file_buffer: [512]u8 = undefined;
        var reader = file.reader(&file_buffer);
        var file_reader = &reader.interface;

        var buffer: ArrayList(u8) = .empty;
        defer buffer.deinit(self.allocator);

        file_reader.appendRemaining(
            self.allocator,
            &buffer,
            .unlimited,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.FileReadFailure,
        };

        return buffer.toOwnedSliceSentinel(self.allocator, 0);
    }

    fn validateValue(expect: anytype, actual: @TypeOf(expect)) bool {
        const Type = @TypeOf(expect);
        const type_info = @typeInfo(Type);

        if (checkForFixedArray(Type)) |fixed_array| {
            return validateFixedArray(
                fixed_array.T,
                fixed_array.capacity,
                expect,
                actual,
            );
        }

        return switch (type_info) {
            .void,
            .int,
            .@"enum",
            => expect == actual,

            .@"struct" => validateStruct(expect, actual),
            .@"union" => validateUnion(expect, actual),
            else => @compileError(
                "validation for " ++ @typeName(Type) ++ " is not supported",
            ),
        };
    }

    fn validateStruct(expect: anytype, actual: @TypeOf(expect)) bool {
        inline for (meta.fields(@TypeOf(expect))) |field| {
            if (!validateValue(
                @field(expect, field.name),
                @field(actual, field.name),
            )) {
                return false;
            }
        }

        return true;
    }

    fn validateUnion(expect: anytype, actual: @TypeOf(expect)) bool {
        if (!mem.eql(u8, @tagName(expect), @tagName(actual))) {
            return false;
        }

        inline for (meta.fields(@TypeOf(expect))) |field| {
            if (mem.eql(u8, field.name, @tagName(expect))) {
                return validateValue(
                    @field(expect, field.name),
                    @field(actual, field.name),
                );
            }
        }

        return false;
    }

    fn validateFixedArray(
        T: type,
        comptime capacity: usize,
        expect: FixedArray(T, capacity),
        actual: FixedArray(T, capacity),
    ) bool {
        if (expect.len != actual.len) {
            return false;
        }

        for (expect.slice(), actual.slice()) |expect_item, actual_item| {
            if (!validateValue(expect_item, actual_item)) {
                return false;
            }
        }

        return true;
    }

    fn validateLoc(
        source: [:0]const u8,
        expect: TestSetup.Expect.Loc,
        actual: Span(u8),
    ) bool {
        switch (expect) {
            .span,
            => |span| {
                return span.index == actual.index and span.len == actual.len;
            },

            .line,
            => |line| {
                const actual_line, _ = actual.toLineCol(source);
                return line == actual_line;
            },
        }
    }
};
