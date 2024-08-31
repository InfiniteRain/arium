const std = @import("std");
const sema_expr_mod = @import("sema_expr.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const SemaExpr = sema_expr_mod.SemaExpr;
const Position = tokenizer_mod.Position;

pub const SemaStmt = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Assert = struct {
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                expr: *SemaExpr,
                position: Position,
            ) !*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .assert = .{
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        pub const Print = struct {
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                expr: *SemaExpr,
                position: Position,
            ) !*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .print = .{
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        pub const Expr = struct {
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                expr: *SemaExpr,
                position: Position,
            ) !*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .expr = .{
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        pub const Let = struct {
            index: usize,
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                index: usize,
                expr: *SemaExpr,
                position: Position,
            ) !*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .let = .{
                            .index = index,
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        pub const Invalid = struct {
            child_exprs: ArrayList(*SemaExpr),

            pub fn create(
                allocator: Allocator,
                child_exprs_struct: anytype,
            ) !*Self {
                const stmt = try allocator.create(Self);
                var child_exprs = ArrayList(*SemaExpr).init(allocator);

                inline for (child_exprs_struct) |child_expr| {
                    try child_exprs.append(child_expr);
                }

                stmt.* = .{
                    .kind = .{
                        .invalid = .{
                            .child_exprs = child_exprs,
                        },
                    },
                    .position = .{
                        .line = 0,
                        .column = 0,
                    },
                };

                return stmt;
            }
        };

        assert: Assert,
        print: Print,
        expr: Expr,
        let: Let,
        invalid: Invalid,
    };

    kind: Kind,
    position: Position,

    pub fn destroy(self: *Self, allocator: Allocator) void {
        switch (self.kind) {
            .assert => |assert| assert.expr.destroy(allocator),
            .print => |print| print.expr.destroy(allocator),
            .expr => |expr| expr.expr.destroy(allocator),
            .let => |let| let.expr.destroy(allocator),
            .invalid => |*invalid| {
                for (invalid.child_exprs.items) |child_expr| {
                    child_expr.destroy(allocator);
                }

                invalid.child_exprs.clearAndFree();
            },
        }

        allocator.destroy(self);
    }
};
