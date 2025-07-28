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
            @"for",
            @"break",
            @"continue",

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
        block: []const Index,
        variable: Variable,
        @"for": For,
        @"break",
        @"continue",

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

        pub const For = struct {
            cond: Index,
            body: Index,
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

        pub fn toStr(self: Index, air: *const Air) []const u8 {
            _ = self;
            _ = air;
            return "";
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
                => InternPool.Index
                    .from(air.nodes.items(.a)[idx])
                    .toType(intern_pool),

                .add,
                .sub,
                .mul,
                .div,
                .concat,
                .neg,
                => blk: {
                    const lhs_type = Index
                        .from(air.nodes.items(.a)[idx])
                        .toType(air, intern_pool);
                    const rhs_type = Index
                        .from(air.nodes.items(.b)[idx])
                        .toType(air, intern_pool);

                    break :blk if (lhs_type == .type_never or rhs_type == .type_never)
                        .type_never
                    else
                        lhs_type;
                },

                .greater_than,
                .greater_equal,
                .less_than,
                .less_equal,
                .equal,
                .not_equal,
                => blk: {
                    const lhs_type = Index
                        .from(air.nodes.items(.a)[idx])
                        .toType(air, intern_pool);
                    const rhs_type = Index
                        .from(air.nodes.items(.b)[idx])
                        .toType(air, intern_pool);

                    break :blk if (lhs_type == .type_never or rhs_type == .type_never)
                        .type_never
                    else
                        .type_bool;
                },

                .cond,
                => blk: {
                    const cond = air.nodes.items(.a)[idx];
                    const cond_type = Index.from(cond).toType(air, intern_pool);

                    if (cond_type == .type_never) {
                        break :blk .type_never;
                    }

                    const extra_idx = air.nodes.items(.b)[idx];
                    const then = air.extra.items[extra_idx];
                    const then_type = Index.from(then).toType(air, intern_pool);
                    const @"else" = air.extra.items[extra_idx + 1];
                    const else_type = Index.from(@"else").toType(air, intern_pool);

                    break :blk if (then_type != .type_never)
                        then_type
                    else if (else_type != .type_never)
                        else_type
                    else
                        .type_never;
                },

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
                => blk: {
                    const a = air.nodes.items(.a)[idx];
                    const expr_type = Index.from(a).toType(air, intern_pool);

                    break :blk if (expr_type == .type_never)
                        .type_never
                    else
                        .type_unit;
                },

                .let,
                .assignment,
                => blk: {
                    const b = air.nodes.items(.b)[idx];

                    if (b == 0) {
                        break :blk .type_unit;
                    }

                    const expr_type = Index.from(b).toType(air, intern_pool);

                    break :blk if (expr_type == .type_never)
                        .type_never
                    else
                        .type_unit;
                },

                .@"for",
                => blk: {
                    const cond = air.nodes.items(.a)[idx];
                    const cond_type = Index.from(cond).toType(air, intern_pool);

                    if (cond_type == .type_never) {
                        break :blk .type_never;
                    }

                    const body = air.nodes.items(.b)[idx];
                    const body_type = Index.from(body).toType(air, intern_pool);

                    break :blk if (body_type == .type_never)
                        .type_never
                    else
                        .type_unit;
                },

                .@"break",
                .@"continue",
                => .type_never,
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
            .constant => .{ .constant = .from(a) },
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
                .cond = .from(a),
                .then_branch = .from(self.extra.items[b]),
                .else_branch = .from(self.extra.items[b + 1]),
            } },
            .neg => .{ .neg = .from(a) },
            .block => .{ .block = @ptrCast(self.extra.items[b..][0..a]) },
            .variable => .{ .variable = .{
                .stack_index = a,
                .type = .from(b),
            } },
            .@"for" => .{ .@"for" = .{
                .cond = .from(a),
                .body = .from(b),
            } },
            .@"break" => .@"break",
            .@"continue" => .@"continue",

            .assert => .{ .assert = .from(a) },
            .print => .{ .print = .from(a) },
            .let => .{ .let = .{
                .stack_index = a,
                .rhs = if (b == 0) null else .from(b),
            } },
            .assignment => .{ .assignment = .{
                .stack_index = a,
                .rhs = .from(b),
            } },
        };
    }

    fn getBinary(comptime tag: []const u8, a: u32, b: u32) Key {
        return @unionInit(Key, tag, .{
            .lhs = .from(a),
            .rhs = .from(b),
        });
    }
};
