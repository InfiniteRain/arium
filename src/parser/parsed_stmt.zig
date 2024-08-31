const std = @import("std");
const parsed_expr_mod = @import("parsed_expr.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ParsedExpr = parsed_expr_mod.ParsedExpr;
const Position = tokenizer_mod.Position;

pub const ParsedStmt = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Assert = struct {
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
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
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
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
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
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
            name: []const u8,
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                name: []const u8,
                expr: *ParsedExpr,
                position: Position,
            ) !*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .let = .{
                            .name = try allocator.dupe(u8, name),
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        print: Print,
        assert: Assert,
        expr: Expr,
        let: Let,
    };

    kind: Kind,
    position: Position,
};
