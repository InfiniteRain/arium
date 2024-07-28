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
        pub const Block = struct {
            stmts: ArrayList(*ParsedStmt),

            pub fn create(
                allocator: Allocator,
                stmts: ArrayList(*ParsedStmt),
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

        print: Print,
        block: Block,
        assert: Assert,
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
        }

        allocator.destroy(self);
    }
};
