const std = @import("std");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const Allocator = std.mem.Allocator;
const Position = tokenizer_mod.Position;

pub const SemaExpression = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Literal = union(enum) {
            int: i64,
            float: f64,
            bool: bool,

            pub fn create(allocator: Allocator, literal: Literal, position: Position) !*Self {
                const expression = try allocator.create(Self);

                expression.* = .{
                    .kind = .{ .literal = literal },
                    .eval_type = switch (literal) {
                        .int => .int,
                        .float => .float,
                        .bool => .bool,
                    },
                    .position = position,
                };

                return expression;
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
                invalid,
            };

            kind: Binary.Kind,
            left: *SemaExpression,
            right: *SemaExpression,

            pub fn create(
                allocator: Allocator,
                kind: Binary.Kind,
                eval_type: EvalType,
                position: Position,
                left: *Self,
                right: *Self,
            ) !*Self {
                const expression = try allocator.create(Self);

                expression.* = .{
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

                return expression;
            }
        };

        pub const Unary = struct {
            pub const Kind = enum {
                negate_bool,
                negate_int,
                negate_float,
                invalid,
            };

            kind: Unary.Kind,
            right: *SemaExpression,

            pub fn create(
                allocator: Allocator,
                kind: Unary.Kind,
                eval_type: EvalType,
                position: Position,
                right: *Self,
            ) !*Self {
                const expression = try allocator.create(Self);

                expression.* = .{
                    .kind = .{
                        .unary = .{
                            .kind = kind,
                            .right = right,
                        },
                    },
                    .eval_type = eval_type,
                    .position = position,
                };

                return expression;
            }
        };

        literal: Literal,
        binary: Binary,
        unary: Unary,
    };

    pub const EvalType = enum {
        int,
        float,
        bool,

        pub fn stringify(self: EvalType) []const u8 {
            return switch (self) {
                .int => "Int",
                .float => "Float",
                .bool => "Bool",
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
        }
        allocator.destroy(self);
    }
};
