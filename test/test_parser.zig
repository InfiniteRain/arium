const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const testing = std.testing;

const arium = @import("arium");
const FixedArray = arium.FixedArray;
const Output = arium.Output;
const Span = arium.Span;
const AriumParser = arium.Parser;
const AriumTokenizer = arium.Tokenizer;
const InternPool = arium.InternPool;
const Sema = arium.Sema;
const Compiler = arium.Compiler;
const Vm = arium.Vm;

const checkForFixedArray = @import("util.zig").checkForFixedArray;
const TestDiagsPrinter = @import("test_diags_printer.zig").TestDiagsPrinter;
const tokenizer_mod = @import("test_tokenizer.zig");
const TestTokenizer = tokenizer_mod.TestTokenizer;
const TestToken = tokenizer_mod.TestToken;

const special_comment_len = 3;

pub const TestSetup = struct {
    kind: Kind,
    expects: ArrayList(Expect),

    pub const Kind = enum {
        parse,
        sema,
        compile,
        run,
    };

    pub const Expect = union(enum) {
        parse_err: ExpectErr(AriumParser.Diags.Entry.Tag),
        sema_err: ExpectErr(Sema.Diags.Entry.Tag),
        compile_err: ExpectErr(Compiler.Diags.Entry.Tag),
        vm_err: ExpectErr(Vm.Diags.Entry.Tag),
        out: Span(u8),

        pub const Loc = union(enum) {
            line: u32,
            span: Span(u8),
        };

        pub fn ExpectErr(EntryTag: type) type {
            return struct {
                tag: EntryTag,
                loc: Loc,
            };
        }
    };

    pub fn deinit(self: *TestSetup, allocator: Allocator) void {
        self.expects.deinit(allocator);
    }
};

pub const TestParser = struct {
    allocator: Allocator,
    test_tokenizer: TestTokenizer,
    intern_pool: *InternPool,
    diags: *Diags,
    scratch: *Scratch,
    prev_token: TestToken,
    current_token: TestToken,
    arium_token_loc: Span(u8),

    pub const Error = error{TestParseFailure} || Allocator.Error;

    pub const Diags = struct {
        entries: ArrayList(Entry),

        pub const Entry = struct {
            tag: Tag,
            loc: Span(u8),

            pub const Tag = union(enum) {
                expected_directive_identifier,
                unexpected_directive,
                expected_left_paren_before_directive,
                expected_right_paren_after_directive_args,
                expected_dot_before_enum_variant,
                expected_enum_variant_identifier,
                unknown_enum_variant,
                expected_test_directive,
                expected_int,
                expected_float,
                expected_empty_block_as_void,
                overflow: std.builtin.Type.Int,
                expected_left_brace_before_struct,
                expected_right_brace_after_struct_fields,
                expected_dot_before_field_name,
                expected_field_identifier,
                expected_equal_after_field_identifier,
                unknown_struct_field,
                not_all_fields_initialized,
                field_redifinition,
                expected_comma_after_loc,
                expected_dot_before_union_variant,
                expected_dot_or_left_brace,
                union_variant_is_not_void,
                unknown_union_variant,
                expected_union_variant_identifier,
                expected_equal_after_union_variant_identifier,
                expected_right_brace_after_union_variant_value,
                expected_left_brace_before_list_elems,
                expected_right_brace_after_list_elems,
                fixed_array_limit_reached: usize,
                expected_string,
                expected_comma_after_span_index,
            };
        };

        pub const empty: Diags = .{
            .entries = .empty,
        };

        pub fn deinit(self: *Diags, allocator: Allocator) void {
            self.entries.deinit(allocator);
        }
    };

    pub const Scratch = struct {
        // intern pool indexes that are cleaned once per ip directive
        primary_indexes: ArrayList(InternPool.Index),

        // intern pool indexes used to construct primary indexes, cleaned once
        // per index array
        secondary_indexes: ArrayList(InternPool.Index),

        pub const empty: Scratch = .{
            .primary_indexes = .empty,
            .secondary_indexes = .empty,
        };

        pub fn deinit(self: *Scratch, allocator: Allocator) void {
            self.primary_indexes.deinit(allocator);
            self.secondary_indexes.deinit(allocator);
        }
    };

    const Directive = struct {
        tag: Tag,
        data: Data,

        const Tag = enum {
            @"test",
            line,
            span,
            parse_err,
            sema_err,
            compile_err,
            vm_err,
            out,
            ip,
        };

        const Data = union {
            @"test": TestSetup.Kind,
            line: u32,
            span: Span(u8),
            expect: TestSetup.Expect,
            ip: InternPool.Index,
        };
    };

    pub fn parse(
        allocator: Allocator,
        source: [:0]const u8,
        intern_pool: *InternPool,
        diags: *Diags,
        scratch: *Scratch,
    ) Error!TestSetup {
        var arium_tokenizer = AriumTokenizer.init(source);

        var parser: TestParser = .{
            .allocator = allocator,
            .test_tokenizer = undefined,
            .intern_pool = intern_pool,
            .scratch = scratch,
            .diags = diags,
            .prev_token = undefined,
            .current_token = undefined,
            .arium_token_loc = undefined,
        };
        var state: enum { @"test", expects } = .@"test";

        var test_setup: TestSetup = .{
            .kind = undefined,
            .expects = .empty,
        };

        while (true) {
            const token = arium_tokenizer.next();

            if (token.tag == .eof) {
                break;
            }

            if (token.tag != .comment) {
                continue;
            }

            const comment = token.loc.toSlice(source);

            if (!mem.startsWith(u8, comment, "///")) {
                continue;
            }

            parser.arium_token_loc = token.loc;
            parser.test_tokenizer = TestTokenizer.init(
                comment[special_comment_len..],
            );
            parser.prev_token = undefined;
            parser.current_token = parser.nextToken();

            switch (state) {
                .@"test" => {
                    const test_directive = try parser.parseDirective(
                        .{.@"test"},
                    );
                    state = .expects;
                    test_setup.kind = test_directive.data.@"test";
                },
                .expects => {
                    const expect_directive = try parser.parseDirective(
                        .{ .parse_err, .sema_err, .compile_err, .vm_err, .out },
                    );
                    try test_setup.expects.append(
                        allocator,
                        expect_directive.data.expect,
                    );
                },
            }
        }

        if (state == .@"test") {
            return parser.parseError(.expected_test_directive, .zero);
        }

        return test_setup;
    }

    fn parseDirective(
        self: *TestParser,
        allowlist: anytype,
    ) Error!Directive {
        const AllowList = @TypeOf(allowlist);
        const type_info = @typeInfo(AllowList);

        if (type_info != .@"struct" or !type_info.@"struct".is_tuple) {
            @compileError("allowlist should be a tuple");
        }

        const identifier = try self.consume(
            .identifier,
            .expected_directive_identifier,
        );
        const identifier_str = self.locToStr(identifier.loc);
        var tag_opt: ?Directive.Tag = null;

        inline for (meta.fields(AllowList)) |field| {
            if (@typeInfo(field.type) != .enum_literal) {
                @compileError("allow list should only contain enum literals");
            }

            const tag = @field(allowlist, field.name);

            if (mem.eql(u8, identifier_str, @tagName(tag))) {
                tag_opt = tag;
                break;
            }
        }

        const tag = tag_opt orelse
            return self.parseError(.unexpected_directive, identifier.loc);

        _ = try self.consume(
            .left_paren,
            .expected_left_paren_before_directive,
        );

        const directive: Directive = .{
            .tag = tag,
            .data = switch (tag) {
                .@"test" => try self.parseTestDirective(),
                .line => try self.parseLineDirective(),
                .span => try self.parseSpanDirective(),
                .parse_err => try self.parseExpectDirective(.parse_err),
                .sema_err => try self.parseExpectDirective(.sema_err),
                .compile_err => try self.parseExpectDirective(.compile_err),
                .vm_err => try self.parseExpectDirective(.vm_err),
                .out => try self.parseOutDirective(),
                .ip => try self.parseIpDirective(),
            },
        };

        _ = try self.consume(
            .right_paren,
            .expected_right_paren_after_directive_args,
        );

        return directive;
    }

    fn parseTestDirective(self: *TestParser) Error!Directive.Data {
        return .{ .@"test" = try self.parseEnum(TestSetup.Kind) };
    }

    fn parseLineDirective(self: *TestParser) Error!Directive.Data {
        return .{ .line = try self.parseType(u32) };
    }

    fn parseSpanDirective(self: *TestParser) Error!Directive.Data {
        const index = try self.parseType(u32);

        _ = try self.consume(.comma, .expected_comma_after_span_index);

        const len = try self.parseType(u32);

        return .{ .span = .{ .index = index, .len = len } };
    }

    fn parseExpectDirective(
        self: *TestParser,
        tag: Directive.Tag,
    ) Error!Directive.Data {
        const loc_directive = try self.parseDirective(.{ .line, .span });
        const loc: TestSetup.Expect.Loc = switch (loc_directive.tag) {
            .line => .{ .line = loc_directive.data.line },
            .span => .{ .span = loc_directive.data.span },
            else => unreachable,
        };
        _ = try self.consume(.comma, .expected_comma_after_loc);

        return switch (tag) {
            .parse_err => .{ .expect = .{ .parse_err = .{
                .tag = try self.parseType(
                    AriumParser.Diags.Entry.Tag,
                ),
                .loc = loc,
            } } },
            .sema_err => .{ .expect = .{ .sema_err = .{
                .tag = try self.parseType(Sema.Diags.Entry.Tag),
                .loc = loc,
            } } },
            .compile_err => .{ .expect = .{ .compile_err = .{
                .tag = try self.parseType(Compiler.Diags.Entry.Tag),
                .loc = loc,
            } } },
            .vm_err => .{ .expect = .{ .vm_err = .{
                .tag = try self.parseType(Vm.Diags.Entry.Tag),
                .loc = loc,
            } } },
            else => unreachable, // non-err-expect directive
        };
    }

    fn parseOutDirective(self: *TestParser) Error!Directive.Data {
        const string = try self.consume(.string, .expected_string);
        const arium_loc = self.locToAriumLoc(string.loc);
        return .{ .expect = .{
            .out = .{
                .index = arium_loc.index + 1,
                .len = arium_loc.len - 2,
            },
        } };
    }

    fn parseIpDirective(self: *TestParser) Error!Directive.Data {
        const ip_indexes_top = self.scratch.primary_indexes.items.len;
        defer self.scratch.primary_indexes.shrinkRetainingCapacity(
            ip_indexes_top,
        );

        const key = try self.parseType(InternPool.Key);
        const index = try self.intern_pool.get(self.allocator, key);

        return .{ .ip = index };
    }

    fn parseType(self: *TestParser, T: type) Error!T {
        const type_info = @typeInfo(T);

        if (T == []const u8) {
            return try self.parseString();
        }

        if (T == InternPool.Index) {
            return try self.parseIpIndex();
        }

        if (T == []const InternPool.Index) {
            return try self.parseIpIndexArray();
        }

        if (checkForFixedArray(T)) |fixed_array| {
            return try self.parseFixedArray(
                fixed_array.T,
                fixed_array.capacity,
            );
        }

        return switch (type_info) {
            .void => try self.parseVoid(),
            .int => try self.parseInt(T),
            .float => try self.parseFloat(T),
            .@"struct" => try self.parseStruct(T),
            .@"enum" => try self.parseEnum(T),
            .@"union" => try self.parseUnion(T),
            else => @compileError(
                "parsing for " ++ @typeName(T) ++ " is not supported",
            ),
        };
    }

    fn parseString(self: *TestParser) Error![]const u8 {
        const string_token = try self.consume(.string, .expected_string);
        const string_quoted = self.locToStr(string_token.loc);
        const string = string_quoted[1 .. string_quoted.len - 1];
        return string;
    }

    fn parseIpIndex(self: *TestParser) Error!InternPool.Index {
        if (self.check(.dot)) {
            return try self.parseEnum(InternPool.Index);
        }

        const directive = try self.parseDirective(.{.ip});
        return directive.data.ip;
    }

    fn parseIpIndexArray(self: *TestParser) Error![]const InternPool.Index {
        const secondary_top = self.scratch.secondary_indexes.items.len;
        defer self.scratch.secondary_indexes.shrinkRetainingCapacity(
            secondary_top,
        );

        _ = try self.consume(
            .left_brace,
            .expected_left_brace_before_list_elems,
        );

        while (!self.check(.right_brace)) {
            try self.scratch.secondary_indexes.append(
                self.allocator,
                try self.parseIpIndex(),
            );

            if (self.match(.comma) == null) {
                break;
            }
        }

        _ = try self.consume(
            .right_brace,
            .expected_right_brace_after_list_elems,
        );

        const primary_top = self.scratch.primary_indexes.items.len;

        try self.scratch.primary_indexes.ensureUnusedCapacity(
            self.allocator,
            self.scratch.secondary_indexes.items.len - secondary_top,
        );
        self.scratch.primary_indexes.appendSliceAssumeCapacity(
            self.scratch.secondary_indexes.items[secondary_top..],
        );

        return self.scratch.primary_indexes.items[primary_top..];
    }

    fn parseFixedArray(
        self: *TestParser,
        T: type,
        comptime capacity: usize,
    ) Error!FixedArray(T, capacity) {
        var buffer: [capacity]T = undefined;

        _ = try self.consume(
            .left_brace,
            .expected_left_brace_before_list_elems,
        );

        var index: usize = 0;
        var len: usize = 0;

        while (!self.check(.right_brace)) : (index += 1) {
            if (index >= capacity) {
                return self.parseError(
                    .{ .fixed_array_limit_reached = capacity },
                    self.current_token.loc,
                );
            }

            buffer[index] = try self.parseType(T);
            len += 1;

            if (self.match(.comma) == null) {
                break;
            }
        }

        _ = try self.consume(
            .right_brace,
            .expected_right_brace_after_list_elems,
        );

        return .{
            .buffer = buffer,
            .len = len,
        };
    }

    fn parseVoid(self: *TestParser) Error!void {
        _ = try self.consume(.left_brace, .expected_empty_block_as_void);
        _ = try self.consume(.right_brace, .expected_empty_block_as_void);
    }

    fn parseInt(self: *TestParser, Int: type) Error!Int {
        const int_token = try self.consume(.int, .expected_int);
        const int_str = self.locToStr(int_token.loc);
        const type_info = @typeInfo(Int);

        return fmt.parseInt(Int, int_str, 10) catch |err|
            switch (err) {
                error.Overflow => return self.parseError(
                    .{ .overflow = type_info.int },
                    int_token.loc,
                ),
                error.InvalidCharacter => unreachable,
            };
    }

    fn parseFloat(self: *TestParser, Float: type) Error!Float {
        const float_token = try self.consume(.float, .expected_float);
        const float_str = self.locToStr(float_token.loc);

        return fmt.parseFloat(Float, float_str) catch |err|
            switch (err) {
                error.InvalidCharacter => unreachable,
            };
    }

    fn parseStruct(self: *TestParser, Struct: type) Error!Struct {
        const fields = meta.fields(Struct);
        var final: Struct = undefined;

        _ = try self.consume(.left_brace, .expected_left_brace_before_struct);

        var initialized = [_]bool{false} ** fields.len;

        while (!self.check(.right_brace)) {
            _ = try self.consume(.dot, .expected_dot_before_field_name);

            const name_token = try self.consume(
                .identifier,
                .expected_field_identifier,
            );
            const name = self.locToStr(name_token.loc);

            _ = try self.consume(
                .equal,
                .expected_equal_after_field_identifier,
            );

            var is_field_found = false;

            inline for (fields, 0..) |field, index| {
                if (mem.eql(u8, name, field.name)) {
                    is_field_found = true;

                    if (initialized[index]) {
                        return self.parseError(
                            .field_redifinition,
                            name_token.loc,
                        );
                    }

                    @field(final, field.name) = try self.parseType(field.type);
                    initialized[index] = true;
                }
            }

            if (!is_field_found) {
                return self.parseError(.unknown_struct_field, name_token.loc);
            }

            if (self.match(.comma) == null) {
                break;
            }
        }

        const last_brace_token = try self.consume(
            .right_brace,
            .expected_right_brace_after_struct_fields,
        );

        if (mem.containsAtLeast(bool, &initialized, 1, &[_]bool{false})) {
            return self.parseError(
                .not_all_fields_initialized,
                last_brace_token.loc,
            );
        }

        return final;
    }

    fn parseEnum(self: *TestParser, Enum: type) Error!Enum {
        if (@typeInfo(Enum) != .@"enum") {
            @compileError("T is expected to be an enum");
        }

        _ = try self.consume(.dot, .expected_dot_before_enum_variant);

        const identifier = try self.consume(
            .identifier,
            .expected_enum_variant_identifier,
        );
        const enum_variant = self.locToStr(identifier.loc);

        return meta.stringToEnum(Enum, enum_variant) orelse
            self.parseError(.unknown_enum_variant, identifier.loc);
    }

    fn parseUnion(self: *TestParser, Union: type) Error!Union {
        if (@typeInfo(Union) != .@"union") {
            @compileError("T is expected to be an enum");
        }

        if (self.match(.dot)) |_| {
            const identifier = try self.consume(
                .identifier,
                .expected_union_variant_identifier,
            );
            const union_variant = self.locToStr(identifier.loc);

            inline for (meta.fields(Union)) |field| {
                if (mem.eql(u8, union_variant, field.name)) {
                    if (field.type != void) {
                        return self.parseError(
                            .union_variant_is_not_void,
                            identifier.loc,
                        );
                    }

                    return @unionInit(Union, field.name, {});
                }
            }

            return self.parseError(.unknown_union_variant, identifier.loc);
        }

        _ = try self.consume(
            .left_brace,
            .expected_dot_or_left_brace,
        );
        _ = try self.consume(
            .dot,
            .expected_dot_before_union_variant,
        );

        const identifier = try self.consume(
            .identifier,
            .expected_union_variant_identifier,
        );
        const union_variant = self.locToStr(identifier.loc);

        _ = try self.consume(
            .equal,
            .expected_equal_after_union_variant_identifier,
        );

        var final_opt: ?Union = null;

        inline for (meta.fields(Union)) |field| {
            if (mem.eql(u8, union_variant, field.name)) {
                final_opt = @unionInit(
                    Union,
                    field.name,
                    try self.parseType(field.type),
                );
            }
        }

        if (final_opt) |final| {
            _ = try self.consume(
                .right_brace,
                .expected_right_brace_after_union_variant_value,
            );

            return final;
        } else {
            return self.parseError(.unknown_union_variant, identifier.loc);
        }
    }

    fn advance(self: *TestParser) TestToken {
        if (self.current_token.tag == .eof) {
            return self.current_token;
        }

        self.prev_token = self.current_token;
        self.current_token = self.nextToken();

        return self.prev_token;
    }

    fn match(self: *TestParser, token: TestToken.Tag) ?TestToken {
        if (self.check(token)) {
            return self.advance();
        }

        return null;
    }

    fn consume(
        self: *TestParser,
        token: TestToken.Tag,
        diag: Diags.Entry.Tag,
    ) Error!TestToken {
        if (self.check(token)) {
            return self.advance();
        }

        return self.parseError(diag, self.peek().loc);
    }

    fn check(self: *TestParser, token: TestToken.Tag) bool {
        return self.peek().tag == token;
    }

    fn peek(self: *TestParser) TestToken {
        return self.current_token;
    }

    fn nextToken(self: *TestParser) TestToken {
        return self.test_tokenizer.next();
    }

    fn locToStr(self: *TestParser, loc: Span(u8)) []const u8 {
        return loc.toSlice(self.test_tokenizer.source);
    }

    fn locToAriumLoc(self: *TestParser, loc: Span(u8)) Span(u8) {
        return .{
            .index = self.arium_token_loc.index + special_comment_len +
                loc.index,
            .len = loc.len,
        };
    }

    fn parseError(
        self: *TestParser,
        diag: Diags.Entry.Tag,
        loc: Span(u8),
    ) Error {
        try self.diags.entries.append(self.allocator, .{
            .tag = diag,
            .loc = self.locToAriumLoc(loc),
        });
        return error.TestParseFailure;
    }
};

fn parseExpectingSuccess(
    source: [:0]const u8,
) TestParser.Error!struct { TestSetup, InternPool } {
    var intern_pool = try InternPool.init(testing.allocator);
    errdefer intern_pool.deinit(testing.allocator);

    var diags: TestParser.Diags = .empty;
    defer diags.deinit(testing.allocator);

    var scratch: TestParser.Scratch = .empty;
    defer scratch.deinit(testing.allocator);

    return .{
        try TestParser.parse(
            testing.allocator,
            source,
            &intern_pool,
            &diags,
            &scratch,
        ),
        intern_pool,
    };
}

test "test identifier" {
    inline for (.{
        .{ ".parse", .parse },
        .{ ".sema", .sema },
        .{ ".compile", .compile },
        .{ ".run", .run },
    }) |entry| {
        const str, const variant = entry;

        var setup, var ip = try parseExpectingSuccess(
            "/// test(" ++ str ++ ")",
        );
        defer setup.deinit(testing.allocator);
        defer ip.deinit(testing.allocator);

        try testing.expectEqual(variant, setup.kind);
    }
}

test "sema_err with enum ip" {
    var setup, var ip = try parseExpectingSuccess(
        "/// test(.run)\n" ++
            "/// sema_err(line(123), {.unexpected_expr_type = " ++
            "{.expected = {.type_int}, .actual = .type_string}})\n",
    );
    defer setup.deinit(testing.allocator);
    defer ip.deinit(testing.allocator);

    try testing.expectEqual(1, setup.expects.items.len);

    const expect = setup.expects.items[0];

    try testing.expectEqual(
        Sema.Diags.Entry.Tag.ExprTypeMismatch{
            .expected = Sema.TypeArray.from(.type_int),
            .actual = .type_string,
        },
        expect.sema_err.tag.unexpected_expr_type,
    );
}

test "sema_err with key ip" {
    var setup, var ip = try parseExpectingSuccess(
        "/// test(.run)\n" ++
            "/// sema_err(line(123), {.unexpected_expr_type = " ++
            "{.expected = {.type_int}, .actual = ip({.type_fn = " ++
            "{.arg_types = {.type_int, ip({.value_string = \"hello\"})}, " ++
            ".return_type = .type_int}" ++
            "})}})",
    );
    defer setup.deinit(testing.allocator);
    defer ip.deinit(testing.allocator);

    try testing.expectEqual(1, setup.expects.items.len);

    const expect = setup.expects.items[0];

    try testing.expectEqual(
        Sema.Diags.Entry.Tag.ExprTypeMismatch{
            .expected = Sema.TypeArray.from(.type_int),
            .actual = try ip.get(
                testing.allocator,
                .{ .type_fn = .{
                    .arg_types = &[_]InternPool.Index{
                        .type_int,
                        try ip.get(
                            testing.allocator,
                            .{ .value_string = "hello" },
                        ),
                    },
                    .return_type = .type_int,
                } },
            ),
        },
        expect.sema_err.tag.unexpected_expr_type,
    );
}

test "out directive" {
    const source =
        \\/// test(.run)
        \\/// out("hello")
        \\/// out("world")
        \\/// out("")
    ;
    var setup, var ip = try parseExpectingSuccess(source);
    defer setup.deinit(testing.allocator);
    defer ip.deinit(testing.allocator);

    try testing.expectEqual(3, setup.expects.items.len);

    const out1 = setup.expects.items[0].out;
    const out2 = setup.expects.items[1].out;
    const out3 = setup.expects.items[2].out;

    try testing.expectEqualStrings("hello", out1.toSlice(source));
    try testing.expectEqualStrings("world", out2.toSlice(source));
    try testing.expectEqualStrings("", out3.toSlice(source));
}
