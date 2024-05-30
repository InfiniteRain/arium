const std = @import("std");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const Allocator = std.mem.Allocator;
const Position = tokenizer_mod.Position;

pub const SemaExpr = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Literal = union(enum) {
            int: i64,
            float: f64,
            bool: bool,
            string: []const u8, // not owned by SemaExpr, but by tokenizer

            pub fn create(allocator: Allocator, literal: Literal, position: Position) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{ .literal = literal },
                    .eval_type = switch (literal) {
                        .int => .int,
                        .float => .float,
                        .bool => .bool,
                        .string => .string,
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const Binary = struct {
            pub const Kind = enum {
                add_int,
                add_float,
                subtract_int,
                subtract_float,
                multiply_int,
                multiply_float,
                divide_int,
                divide_float,
                concat,
            };

            kind: Binary.Kind,
            left: *SemaExpr,
            right: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                kind: Binary.Kind,
                eval_type: EvalType,
                position: Position,
                left: *Self,
                right: *Self,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .binary = .{
                            .kind = kind,
                            .left = left,
                            .right = right,
                        },
                    },
                    .eval_type = eval_type,
                    .position = position,
                };

                return expr;
            }
        };

        pub const Unary = struct {
            pub const Kind = enum {
                negate_bool,
                negate_int,
                negate_float,
            };

            kind: Unary.Kind,
            right: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                kind: Unary.Kind,
                eval_type: EvalType,
                position: Position,
                right: *Self,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .unary = .{
                            .kind = kind,
                            .right = right,
                        },
                    },
                    .eval_type = eval_type,
                    .position = position,
                };

                return expr;
            }
        };

        pub const Invalid = struct {
            pub fn create(allocator: Allocator) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .invalid,
                    .eval_type = .invalid,
                    .position = .{
                        .line = 0,
                        .column = 0,
                    },
                };

                return expr;
            }
        };

        literal: Literal,
        binary: Binary,
        unary: Unary,
        invalid: Invalid,
    };

    pub const EvalType = enum {
        int,
        float,
        bool,
        string,
        invalid,

        pub fn stringify(self: EvalType) []const u8 {
            return switch (self) {
                .int => "Int",
                .float => "Float",
                .bool => "Bool",
                .string => "String",
                .invalid => "Invalid",
            };
        }
    };

    kind: Kind,
    eval_type: EvalType,
    position: Position,

    pub fn destroy(self: *Self, allocator: Allocator) void {
        switch (self.kind) {
            .literal => {},
            .binary => |binary| {
                binary.left.destroy(allocator);
                binary.right.destroy(allocator);
            },
            .unary => |unary| {
                unary.right.destroy(allocator);
            },
            .invalid => {},
        }
        allocator.destroy(self);
    }
};
