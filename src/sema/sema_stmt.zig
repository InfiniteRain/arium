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

        assert: Assert,
        print: Print,
        expr: Expr,
        let: Let,
    };

    kind: Kind,
    position: Position,
};
