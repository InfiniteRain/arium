const std = @import("std");
const Allocator = std.mem.Allocator;
const MultiArrayList = std.MultiArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const intern_pool_mod = @import("intern_pool.zig");
const InternPool = intern_pool_mod.InternPool;

pub const Air = struct {
    nodes: MultiArrayList(Node),
    extra: ArrayListUnmanaged(u32),

    pub const Node = struct {
        tag: Tag,
        a: u32 = undefined,
        b: u32 = undefined,

        pub const Tag = enum {
            constant,
            add,
            sub,
            mul,
            div,
            concat,
            equal,
            not_equal,
            greater_than,
            greater_equal,
            less_than,
            less_equal,
            cond,
            neg,
            block,
            variable,
            assignment,

            assert,
            print,
            let,
        };
    };

    pub const Key = union(enum) {
        constant: InternPool.Index,
        add: Binary,
        sub: Binary,
        mul: Binary,
        div: Binary,
        concat: Binary,
        equal: Binary,
        not_equal: Binary,
        greater_than: Binary,
        greater_equal: Binary,
        less_than: Binary,
        less_equal: Binary,
        cond: Cond,
        neg: Index,
        block: []Index,
        variable: Variable,

        assert: Index,
        print: Index,
        let: Let,
        assignment: Assignment,

        pub const Binary = struct {
            lhs: Index,
            rhs: Index,
        };

        pub const Cond = struct {
            cond: Index,
            then_branch: Index,
            else_branch: Index,
        };

        pub const Variable = struct {
            stack_index: u32,
            type: InternPool.Index,
        };

        pub const Let = struct {
            stack_index: u32,
            rhs: ?Index,
        };

        pub const Assignment = struct {
            stack_index: u32,
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

        pub fn toKey(self: Index, air: *const Air) Key {
            return air.get(self);
        }

        pub fn toType(
            self: Index,
            air: *const Air,
            intern_pool: *const InternPool,
        ) InternPool.Index {
            const idx = self.toInt();
            const tag = air.nodes.items(.tag)[idx];

            return switch (tag) {
                .constant,
                .add,
                .sub,
                .mul,
                .div,
                .concat,
                .neg,
                => InternPool.Index
                    .from(air.nodes.items(.a)[idx])
                    .toType(intern_pool),

                .greater_than,
                .greater_equal,
                .less_than,
                .less_equal,
                .equal,
                .not_equal,
                => .type_bool,

                .cond => Air.Index
                    .from(air.extra.items[air.nodes.items(.b)[idx]])
                    .toType(air, intern_pool),

                .block,
                => blk: {
                    const a = air.nodes.items(.a)[idx];
                    const b = air.nodes.items(.b)[idx];
                    const stmts = air.extra.items[b..][0..a];

                    break :blk Index.from(stmts[stmts.len - 1])
                        .toType(air, intern_pool);
                },

                .variable => InternPool.Index
                    .from(air.nodes.items(.b)[idx]),

                .assert,
                .print,
                .let,
                .assignment,
                => .type_unit,
            };
        }
    };

    pub const empty: Air = .{
        .nodes = .empty,
        .extra = .empty,
    };

    pub fn deinit(self: *Air, allocator: Allocator) void {
        self.nodes.deinit(allocator);
        self.extra.deinit(allocator);
    }

    fn get(self: *const Air, index: Index) Key {
        const node = self.nodes.get(index.toInt());
        const a = node.a;
        const b = node.b;

        return switch (node.tag) {
            .constant => .{ .constant = InternPool.Index.from(a) },
            .add => getBinary("add", a, b),
            .sub => getBinary("sub", a, b),
            .mul => getBinary("mul", a, b),
            .div => getBinary("div", a, b),
            .concat => getBinary("concat", a, b),
            .equal => getBinary("equal", a, b),
            .not_equal => getBinary("not_equal", a, b),
            .greater_than => getBinary("greater_than", a, b),
            .greater_equal => getBinary("greater_equal", a, b),
            .less_than => getBinary("less_than", a, b),
            .less_equal => getBinary("less_equal", a, b),
            .cond => .{ .cond = .{
                .cond = Index.from(a),
                .then_branch = Index.from(self.extra.items[b]),
                .else_branch = Index.from(self.extra.items[b + 1]),
            } },
            .neg => .{ .neg = Index.from(a) },
            .block => .{ .block = @ptrCast(self.extra.items[b..][0..a]) },
            .variable => .{ .variable = .{
                .stack_index = a,
                .type = InternPool.Index.from(b),
            } },

            .assert => .{ .assert = Index.from(a) },
            .print => .{ .print = Index.from(a) },
            .let => .{ .let = .{
                .stack_index = a,
                .rhs = if (b == 0) null else Index.from(b),
            } },
            .assignment => .{ .assignment = .{
                .stack_index = a,
                .rhs = Index.from(b),
            } },
        };
    }

    fn getBinary(comptime tag: []const u8, a: u32, b: u32) Key {
        return @unionInit(Key, tag, .{
            .lhs = Index.from(a),
            .rhs = Index.from(b),
        });
    }
};
