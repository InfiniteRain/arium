const std = @import("std");
const shared = @import("shared");
const Loc = @import("tokenizer.zig").Loc;

const Allocator = std.mem.Allocator;
const MultiArrayList = std.MultiArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const assert = std.debug.assert;

pub const Ast = struct {
    source: []const u8,
    nodes: MultiArrayList(Node),
    locs: ArrayListUnmanaged(Loc),
    extra: ArrayListUnmanaged(u32),

    pub const Node = struct {
        tag: Tag,
        a: u32 = undefined,
        b: u32 = undefined,

        pub const Tag = enum(u8) {
            literal_unit,
            literal_int,
            literal_float,
            literal_bool,
            literal_string,
            add,
            sub,
            mul,
            div,
            concat,
            @"and",
            @"or",
            neg_bool,
            neg_num,
            equal,
            not_equal,
            greater_than,
            greater_equal,
            less_than,
            less_equal,
            block,
            block_semicolon,
            identifier,
            assignment,
            @"if",
            if_else,
            if_elseif,
            if_elseif_else,
            @"for",
            for_conditional,
            @"break",
            @"continue",
            @"return",
            return_value,
            call,
            call_simple,

            assert,
            print,
            expr_stmt,
            let,
            let_mut,
            @"fn",
        };
    };

    pub const Key = union(enum) {
        literal_unit,
        literal_int,
        literal_float,
        literal_bool,
        literal_string,
        add: Binary,
        sub: Binary,
        mul: Binary,
        div: Binary,
        concat: Binary,
        @"and": Binary,
        @"or": Binary,
        equal: Binary,
        not_equal: Binary,
        greater_than: Binary,
        greater_equal: Binary,
        less_than: Binary,
        less_equal: Binary,
        neg_bool: Index,
        neg_num: Index,
        block: []const Index,
        block_semicolon: []const Index,
        identifier,
        assignment: Binary,
        @"if": Conditional,
        if_else: IfElse,
        if_elseif: IfElseIf,
        if_elseif_else: IfElseIfElse,
        @"for": Index,
        for_conditional: Conditional,
        @"break",
        @"continue",
        @"return",
        return_value: Index,
        call: Call,
        call_simple: CallSimple,

        assert: Index,
        print: Index,
        expr_stmt: Index,
        let: Let,
        let_mut: Let,
        @"fn": Fn,

        pub const Binary = struct {
            lhs: Index,
            rhs: Index,
        };

        pub const Conditional = extern struct {
            condition: Index,
            body: Index,
        };

        pub const IfElse = struct {
            conditional: Conditional,
            else_block: Index,
        };

        pub const IfElseIf = struct {
            conditionals: []const Conditional,
        };

        pub const IfElseIfElse = struct {
            conditionals: []const Conditional,
            else_block: Index,
        };

        pub const Let = struct {
            identifier: Index,
            type: ?Index,
            expr: ?Index,
        };

        pub const Fn = struct {
            identifier: Index,
            params: []const FnArg,
            return_type: Index,
            body: Index,
        };

        pub const FnArg = extern struct {
            identifier: Index,
            type: Index,
        };

        pub const Call = struct {
            callee: Index,
            args: []const Index,
        };

        pub const CallSimple = struct {
            callee: Index,
            arg: ?Index,
        };
    };

    pub const Index = enum(u32) {
        _,

        pub fn from(int: anytype) Index {
            return @enumFromInt(int);
        }

        pub fn toInt(self: Index) u32 {
            return @intFromEnum(self);
        }

        pub fn toKey(self: Index, ast: *const Ast) Key {
            return ast.get(self);
        }

        pub fn toLoc(self: Index, ast: *const Ast) Loc {
            return ast.locs.items[@intFromEnum(self)];
        }

        pub fn toStr(self: Index, ast: *const Ast) []const u8 {
            const loc = self.toLoc(ast);
            return ast.source[loc.index..][0..loc.len];
        }

        pub fn toTag(self: Index, ast: *const Ast) Node.Tag {
            return ast.nodes.items(.tag)[self.toInt()];
        }
    };

    pub fn init(source: []const u8) Ast {
        return .{
            .source = source,
            .nodes = .empty,
            .locs = .empty,
            .extra = .empty,
        };
    }

    pub fn deinit(self: *Ast, allocator: Allocator) void {
        self.nodes.deinit(allocator);
        self.locs.deinit(allocator);
        self.extra.deinit(allocator);
    }

    fn get(self: *const Ast, index: Index) Key {
        const node = self.nodes.get(index.toInt());
        const a = node.a;
        const b = node.b;

        return switch (node.tag) {
            .literal_unit => .literal_unit,
            .literal_int => .literal_int,
            .literal_float => .literal_float,
            .literal_bool => .literal_bool,
            .literal_string => .literal_string,
            .add => getBinary("add", a, b),
            .sub => getBinary("sub", a, b),
            .mul => getBinary("mul", a, b),
            .div => getBinary("div", a, b),
            .concat => getBinary("concat", a, b),
            .@"and" => getBinary("and", a, b),
            .@"or" => getBinary("or", a, b),
            .equal => getBinary("equal", a, b),
            .not_equal => getBinary("not_equal", a, b),
            .greater_than => getBinary("greater_than", a, b),
            .greater_equal => getBinary("greater_equal", a, b),
            .less_than => getBinary("less_than", a, b),
            .less_equal => getBinary("less_equal", a, b),
            .neg_bool => .{ .neg_bool = Index.from(a) },
            .neg_num => .{ .neg_num = Index.from(a) },
            .block => .{ .block = @ptrCast(self.extra.items[b..][0..a]) },
            .block_semicolon => .{
                .block_semicolon = @ptrCast(self.extra.items[b..][0..a]),
            },
            .identifier => .identifier,
            .assignment => getBinary("assignment", a, b),
            .@"if" => .{ .@"if" = .{
                .condition = Index.from(a),
                .body = Index.from(b),
            } },
            .if_else => .{ .if_else = .{
                .conditional = .{
                    .condition = Index.from(a),
                    .body = Index.from(self.extra.items[b]),
                },
                .else_block = Index.from(self.extra.items[b + 1]),
            } },
            .if_elseif => .{
                .if_elseif = .{
                    .conditionals = @ptrCast(self.extra.items[b..][0 .. a * 2]),
                },
            },
            .if_elseif_else => .{ .if_elseif_else = .{
                .conditionals = @ptrCast(self.extra.items[b..][0 .. a * 2]),
                .else_block = Index.from(self.extra.items[b + a * 2]),
            } },
            .@"for" => .{ .@"for" = Index.from(a) },
            .for_conditional => .{ .for_conditional = .{
                .condition = Index.from(a),
                .body = Index.from(b),
            } },
            .@"break" => .@"break",
            .@"continue" => .@"continue",
            .@"return" => .@"return",
            .return_value => .{ .return_value = Index.from(a) },
            .call => .{ .call = .{
                .callee = Index.from(a),
                .args = @ptrCast(
                    self.extra.items[b + 1 ..][0..self.extra.items[b]],
                ),
            } },
            .call_simple => .{ .call_simple = .{
                .callee = Index.from(a),
                .arg = if (b == 0) null else Index.from(b),
            } },

            .assert => .{ .assert = Index.from(a) },
            .print => .{ .print = Index.from(a) },
            .expr_stmt => .{ .expr_stmt = Index.from(a) },
            .let => self.getLet("let", a, b),
            .let_mut => self.getLet("let_mut", a, b),
            .@"fn" => blk: {
                const arg_count = self.extra.items[b];
                const params = self.extra.items[b + 1 ..][0 .. arg_count * 2];
                const return_type = self.extra.items[b + arg_count * 2 + 1];
                const body = self.extra.items[b + arg_count * 2 + 2];

                break :blk .{ .@"fn" = .{
                    .identifier = Index.from(a),
                    .params = @ptrCast(params),
                    .return_type = Index.from(return_type),
                    .body = Index.from(body),
                } };
            },
        };
    }

    fn getBinary(comptime tag: []const u8, a: u32, b: u32) Key {
        return @unionInit(Key, tag, .{
            .lhs = Index.from(a),
            .rhs = Index.from(b),
        });
    }

    fn getLet(self: *const Ast, comptime tag: []const u8, a: u32, b: u32) Key {
        const extra = self.extra.items[b..][0..2];
        return @unionInit(Key, tag, .{
            .identifier = Index.from(a),
            .type = if (extra[0] == 0) null else Index.from(extra[0]),
            .expr = if (extra[1] == 0) null else Index.from(extra[1]),
        });
    }
};
