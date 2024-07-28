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
        pub const Block = struct {
            // todo: should be an expression
            stmts: ArrayList(*Self),

            pub fn create(
                allocator: Allocator,
                stmts: ArrayList(*Self),
                position: Position,
            ) !*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .block = .{
                            .stmts = stmts,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

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

        pub const Invalid = struct {
            child_exprs: ArrayList(*SemaExpr),
            child_stmts: ArrayList(*Self),

            pub fn create(
                allocator: Allocator,
                child_exprs_struct: anytype,
                child_stmts_struct: anytype,
            ) !*Self {
                const stmt = try allocator.create(Self);
                var child_exprs = ArrayList(*SemaExpr).init(allocator);
                var child_stmts = ArrayList(*Self).init(allocator);

                inline for (child_exprs_struct) |child_expr| {
                    try child_exprs.append(child_expr);
                }

                inline for (child_stmts_struct) |child_stmt| {
                    try child_stmts.append(child_stmt);
                }

                stmt.* = .{
                    .kind = .{
                        .invalid = .{
                            .child_exprs = child_exprs,
                            .child_stmts = child_stmts,
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

        block: Block,
        assert: Assert,
        print: Print,
        invalid: Invalid,
    };

    kind: Kind,
    position: Position,

    pub fn destroy(self: *Self, allocator: Allocator) void {
        switch (self.kind) {
            .block => |*block| {
                for (block.stmts.items) |stmt| {
                    stmt.destroy(allocator);
                }

                block.stmts.clearAndFree();
            },
            .assert => |assert| assert.expr.destroy(allocator),
            .print => |print| print.expr.destroy(allocator),
            .invalid => |*invalid| {
                for (invalid.child_exprs.items) |child_expr| {
                    child_expr.destroy(allocator);
                }

                for (invalid.child_stmts.items) |child_stmt| {
                    child_stmt.destroy(allocator);
                }

                invalid.child_exprs.clearAndFree();
                invalid.child_stmts.clearAndFree();
            },
        }

        allocator.destroy(self);
    }
};
