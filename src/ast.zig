const std = @import("std");
const Loc = @import("tokenizer.zig").Loc;

const Allocator = std.mem.Allocator;
const MultiArrayList = std.MultiArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
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
            block_semicolon,
            variable,
            assignment,

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
        negation_bool: Index,
        negation_num: Index,
        block: []const Index,
        block_semicolon: []const Index,
        variable,
        assignment: Binary,

        print: Index,
        expr_stmt: Index,

        pub const Binary = struct {
            lhs: Index,
            rhs: Index,
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

        pub fn toKey(self: Index, ast: *Ast) Key {
            return ast.get(self);
        }

        pub fn toLoc(self: Index, ast: *Ast) Loc {
            return ast.locs.items[@intFromEnum(self)];
        }

        pub fn toTag(self: Index, ast: *Ast) Node.Tag {
            return ast.nodes.items(.tag)[self.toInt()];
        }
    };

    pub fn deinit(self: *Ast, allocator: Allocator) void {
        self.nodes.deinit(allocator);
        self.locs.deinit(allocator);
        self.extra.deinit(allocator);
    }

    fn get(self: *Ast, index: Index) Key {
        const node = self.nodes.get(index.toInt());
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
            .negation_bool => .{ .negation_bool = Index.from(a) },
            .negation_num => .{ .negation_num = Index.from(a) },
            .block => .{
                .block = @ptrCast(self.extra.items[b .. b + a]),
            },
            .block_semicolon => .{
                .block_semicolon = @ptrCast(self.extra.items[b .. b + a]),
            },
            .variable => .variable,
            .assignment => .{ .assignment = .{
                .lhs = Index.from(a),
                .rhs = Index.from(b),
            } },

            .print => .{ .print = Index.from(a) },
            .expr_stmt => .{ .expr_stmt = Index.from(a) },
        };
    }

    pub fn add(
        self: *Ast,
        allocator: Allocator,
        key: Key,
        loc: Loc,
    ) Allocator.Error!Index {
        try self.locs.append(allocator, loc);
        try self.nodes.append(allocator, switch (key) {
            .literal_unit => .{ .tag = .literal_unit },
            .literal_int => .{ .tag = .literal_int },
            .literal_float => .{ .tag = .literal_float },
            .literal_bool => .{ .tag = .literal_bool },
            .literal_string => .{ .tag = .literal_string },
            .addition => |binary| .{
                .tag = .addition,
                .a = binary.lhs.toInt(),
                .b = binary.rhs.toInt(),
            },
            .subtraction => |binary| .{
                .tag = .subtraction,
                .a = binary.lhs.toInt(),
                .b = binary.rhs.toInt(),
            },
            .multiplication => |binary| .{
                .tag = .multiplication,
                .a = binary.lhs.toInt(),
                .b = binary.rhs.toInt(),
            },
            .division => |binary| .{
                .tag = .division,
                .a = binary.lhs.toInt(),
                .b = binary.rhs.toInt(),
            },
            .negation_bool => |index| .{
                .tag = .negation_bool,
                .a = index.toInt(),
            },
            .negation_num => |index| .{
                .tag = .negation_num,
                .a = index.toInt(),
            },
            .block => |indexes| try self.addBlock(allocator, .block, indexes),
            .block_semicolon => |indexes| try self.addBlock(
                allocator,
                .block_semicolon,
                indexes,
            ),
            .variable => .{ .tag = .variable },
            .assignment => |binary| .{
                .tag = .assignment,
                .a = binary.lhs.toInt(),
                .b = binary.rhs.toInt(),
            },

            .print => |expr| .{ .tag = .print, .a = expr.toInt() },
            .expr_stmt => |expr| .{ .tag = .expr_stmt, .a = expr.toInt() },
        });

        assert(self.nodes.len == self.locs.items.len);

        return Index.from(self.nodes.len - 1);
    }

    fn addBlock(
        self: *Ast,
        allocator: Allocator,
        tag: Node.Tag,
        indexes: []const Index,
    ) Allocator.Error!Node {
        const extra_top = self.extra.items.len;

        for (indexes) |index| {
            try self.extra.append(allocator, index.toInt());
        }

        return .{
            .tag = tag,
            .a = @intCast(self.extra.items.len - extra_top),
            .b = @intCast(extra_top),
        };
    }
};
