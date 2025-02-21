const std = @import("std");
const Loc = @import("tokenizer.zig").Loc;

const Allocator = std.mem.Allocator;
const MultiArrayList = std.MultiArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const meta = std.meta;
const assert = std.debug.assert;

pub const Ast = struct {
    nodes: MultiArrayList(Node) = .{},
    locs: ArrayListUnmanaged(Loc) = .{},
    extra: ArrayListUnmanaged(u32) = .{},
    root: Ast.Index = undefined,

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
            addition,
            subtraction,
            multiplication,
            division,
            negation_bool,
            negation_num,
            block,
            eval_block,

            print,
            expr_stmt,
        };
    };

    pub const Key = union(enum) {
        literal_unit,
        literal_int,
        literal_float,
        literal_bool,
        literal_string,
        addition: Binary,
        subtraction: Binary,
        multiplication: Binary,
        division: Binary,
        negation_bool: Unary,
        negation_num: Unary,
        block: Block,
        eval_block: Block,

        print: Index,
        expr_stmt: Index,

        pub const Unary = struct {
            rhs: Index,
        };

        pub const Binary = struct {
            lhs: Index,
            rhs: Index,
        };

        pub const Block = struct {
            index: u32,
            len: u32,

            pub fn toSlice(self: Block, ast: *Ast) []Index {
                return @ptrCast(
                    ast.extra.items[self.index .. self.index + self.len],
                );
            }
        };
    };

    pub const Index = enum(u32) {
        _,

        pub fn to(self: Index) u32 {
            return @intFromEnum(self);
        }

        pub fn from(int: anytype) Index {
            return @enumFromInt(int);
        }

        pub fn toKey(self: Index, ast: *Ast) Key {
            return ast.get(self);
        }

        pub fn toLoc(self: Index, ast: *Ast) Loc {
            return ast.getLoc(self);
        }
    };

    pub fn deinit(self: *Ast, allocator: Allocator) void {
        self.nodes.deinit(allocator);
        self.locs.deinit(allocator);
        self.extra.deinit(allocator);
    }

    fn get(self: *Ast, index: Index) Key {
        const node = self.nodes.get(index.to());
        const a = node.a;
        const b = node.b;

        return switch (node.tag) {
            .literal_unit => .literal_unit,
            .literal_int => .literal_int,
            .literal_float => .literal_float,
            .literal_bool => .literal_bool,
            .literal_string => .literal_string,
            .addition => .{ .addition = .{
                .lhs = Index.from(a),
                .rhs = Index.from(b),
            } },
            .subtraction => .{ .subtraction = .{
                .lhs = Index.from(a),
                .rhs = Index.from(b),
            } },
            .multiplication => .{ .multiplication = .{
                .lhs = Index.from(a),
                .rhs = Index.from(b),
            } },
            .division => .{ .division = .{
                .lhs = Index.from(a),
                .rhs = Index.from(b),
            } },
            .negation_bool => .{ .negation_bool = .{ .rhs = Index.from(a) } },
            .negation_num => .{ .negation_num = .{ .rhs = Index.from(a) } },
            .block => .{ .block = .{ .index = b, .len = a } },
            .eval_block => .{ .eval_block = .{ .index = b, .len = a } },

            .print => .{ .print = Index.from(a) },
            .expr_stmt => .{ .expr_stmt = Index.from(a) },
        };
    }

    fn getLoc(self: *Ast, index: Index) Loc {
        return self.locs.items[@intFromEnum(index)];
    }

    pub fn add(
        self: *Ast,
        allocator: Allocator,
        key: Key,
        loc: Loc,
    ) Allocator.Error!Index {
        try self.locs.append(allocator, loc);

        switch (key) {
            .literal_unit => {
                try self.nodes.append(allocator, .{ .tag = .literal_unit });
            },
            .literal_int => {
                try self.nodes.append(allocator, .{ .tag = .literal_int });
            },
            .literal_float => {
                try self.nodes.append(allocator, .{ .tag = .literal_float });
            },
            .literal_bool => {
                try self.nodes.append(allocator, .{ .tag = .literal_bool });
            },
            .literal_string => {
                try self.nodes.append(allocator, .{ .tag = .literal_string });
            },
            .addition => |binary| {
                try self.nodes.append(allocator, .{
                    .tag = .addition,
                    .a = binary.lhs.to(),
                    .b = binary.rhs.to(),
                });
            },
            .subtraction => |binary| {
                try self.nodes.append(allocator, .{
                    .tag = .subtraction,
                    .a = binary.lhs.to(),
                    .b = binary.rhs.to(),
                });
            },
            .multiplication => |binary| {
                try self.nodes.append(allocator, .{
                    .tag = .multiplication,
                    .a = binary.lhs.to(),
                    .b = binary.rhs.to(),
                });
            },
            .division => |binary| {
                try self.nodes.append(allocator, .{
                    .tag = .division,
                    .a = binary.lhs.to(),
                    .b = binary.rhs.to(),
                });
            },
            .negation_bool => |unary| {
                try self.nodes.append(allocator, .{
                    .tag = .negation_bool,
                    .a = unary.rhs.to(),
                });
            },
            .negation_num => |unary| {
                try self.nodes.append(allocator, .{
                    .tag = .negation_num,
                    .a = unary.rhs.to(),
                });
            },
            .block => |block| {
                try self.nodes.append(allocator, .{
                    .tag = .block,
                    .a = block.len,
                    .b = block.index,
                });
            },
            .eval_block => |block| {
                try self.nodes.append(allocator, .{
                    .tag = .eval_block,
                    .a = block.len,
                    .b = block.index,
                });
            },

            .print => |expr| {
                try self.nodes.append(allocator, .{
                    .tag = .print,
                    .a = expr.to(),
                });
            },
            .expr_stmt => |expr| {
                try self.nodes.append(allocator, .{
                    .tag = .expr_stmt,
                    .a = expr.to(),
                });
            },
        }

        assert(self.nodes.len == self.locs.items.len);

        return Index.from(self.nodes.len - 1);
    }

    pub fn getExtra(self: *Ast, index: u32) u32 {
        return self.extra.items[index];
    }

    pub fn getExtraLen(self: *Ast) usize {
        return self.extra.items.len;
    }

    pub fn addExtra(
        self: *Ast,
        allocator: Allocator,
        data: u32,
    ) Allocator.Error!void {
        try self.extra.append(allocator, data);
    }
};
