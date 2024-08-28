const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const parsed_stmt_mod = @import("parsed_stmt.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;
const Position = tokenizer.Position;
const ParsedStmt = parsed_stmt_mod.ParsedStmt;

pub const ParsedExpr = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Literal = struct {
            pub const Kind = union(enum) {
                int: i64,
                float: f64,
                bool: bool,
                string: []const u8,
            };

            kind: Literal.Kind,

            pub fn create(
                allocator: Allocator,
                literal_kind: Literal.Kind,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .literal = .{
                            .kind = literal_kind,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const Binary = struct {
            pub const Kind = enum {
                add,
                subtract,
                divide,
                multiply,
                concat,

                equal,
                not_equal,
                greater,
                greater_equal,
                less,
                less_equal,

                and_,
                or_,
            };

            left: *Self,
            kind: Binary.Kind,
            right: *Self,

            pub fn create(
                allocator: Allocator,
                left: *Self,
                kind: Binary.Kind,
                right: *Self,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .binary = .{
                            .left = left,
                            .kind = kind,
                            .right = right,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const Unary = struct {
            pub const Kind = enum {
                negate_bool,
                negate_num,
            };

            kind: Unary.Kind,
            right: *Self,

            pub fn create(
                allocator: Allocator,
                kind: Unary.Kind,
                right: *Self,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .unary = .{
                            .kind = kind,
                            .right = right,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        // todo: try turning all references to const (expr/stmt)
        pub const Block = struct {
            stmts: ArrayList(*ParsedStmt),

            pub fn create(
                allocator: Allocator,
                stmts: ArrayList(*ParsedStmt),
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .block = .{
                            .stmts = stmts,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        literal: Literal,
        binary: Binary,
        unary: Unary,
        block: Block,
    };

    kind: Kind,
    position: Position,

    pub fn destroy(self: *Self, allocator: Allocator) void {
        switch (self.kind) {
            .binary => |binary| {
                binary.left.destroy(allocator);
                binary.right.destroy(allocator);
            },
            .literal => {},
            .unary => |unary| {
                unary.right.destroy(allocator);
            },
            .block => |*block| {
                for (block.stmts.items) |stmt| {
                    stmt.destroy(allocator);
                }

                block.stmts.clearAndFree();
            },
        }

        allocator.destroy(self);
    }
};
