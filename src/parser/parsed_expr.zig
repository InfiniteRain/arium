const std = @import("std");
const tokenizer = @import("tokenizer.zig");

const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;

pub const ParsedExpr = union(enum) {
    const Self = @This();

    pub const Literal = struct {
        pub const Kind = enum {
            int,
            float,
            bool,
            string,
        };

        token: Token,
        kind: Kind,

        pub fn create(allocator: Allocator, token: Token, literal_kind: Kind) !*Self {
            const expr = try allocator.create(Self);

            expr.* = .{
                .literal = .{
                    .token = token,
                    .kind = literal_kind,
                },
            };

            return expr;
        }
    };

    pub const Binary = struct {
        pub const OperatorKind = enum {
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
        operator_token: Token,
        operator_kind: OperatorKind,
        right: *Self,

        pub fn create(
            allocator: Allocator,
            left: *Self,
            operator_token: Token,
            operator_kind: OperatorKind,
            right: *Self,
        ) !*Self {
            const expr = try allocator.create(Self);

            expr.* = .{
                .binary = .{
                    .left = left,
                    .operator_token = operator_token,
                    .operator_kind = operator_kind,
                    .right = right,
                },
            };

            return expr;
        }
    };

    pub const Unary = struct {
        pub const OperatorKind = enum {
            negate_bool,
            negate_num,
        };

        operator_token: Token,
        operator_kind: OperatorKind,
        right: *Self,

        pub fn create(
            allocator: Allocator,
            operator_token: Token,
            operator_kind: OperatorKind,
            right: *Self,
        ) !*Self {
            const expr = try allocator.create(Self);

            expr.* = .{
                .unary = .{
                    .operator_token = operator_token,
                    .operator_kind = operator_kind,
                    .right = right,
                },
            };

            return expr;
        }
    };

    literal: Literal,
    binary: Binary,
    unary: Unary,

    pub fn destroy(self: *Self, allocator: Allocator) void {
        switch (self.*) {
            .binary => |binary| {
                binary.left.destroy(allocator);
                binary.right.destroy(allocator);
            },
            .literal => {},
            .unary => |unary| {
                unary.right.destroy(allocator);
            },
        }

        allocator.destroy(self);
    }
};
