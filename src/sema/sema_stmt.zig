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

        block: Block,
        print: Print,
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
            .print => |print| print.expr.destroy(allocator),
        }

        allocator.destroy(self);
    }
};
