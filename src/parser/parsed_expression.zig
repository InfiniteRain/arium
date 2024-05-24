const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const io_handler_mod = @import("../io_handler.zig");

const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;
const IoHandler = io_handler_mod.IoHandler;

pub const ParsedExpression = union(enum) {
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
            const expression = try allocator.create(Self);

            expression.* = .{
                .literal = .{
                    .token = token,
                    .kind = literal_kind,
                },
            };

            return expression;
        }
    };

    pub const Binary = struct {
        pub const OperatorKind = enum {
            add,
            subtract,
            divide,
            multiply,
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
            const expression = try allocator.create(Self);

            expression.* = .{
                .binary = .{
                    .left = left,
                    .operator_token = operator_token,
                    .operator_kind = operator_kind,
                    .right = right,
                },
            };

            return expression;
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
            const expression = try allocator.create(Self);

            expression.* = .{
                .unary = .{
                    .operator_token = operator_token,
                    .operator_kind = operator_kind,
                    .right = right,
                },
            };

            return expression;
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

    pub fn print(self: *const Self, io: *IoHandler) void {
        switch (self.*) {
            .binary => |binary| Self.printParenthesized(
                io,
                binary.operator_token.lexeme,
                .{ binary.left, binary.right },
            ),
            .literal => |literal| io.outf("{s}", .{literal.token.lexeme}),
            .unary => |unary| Self.printParenthesized(
                io,
                unary.operator_token.lexeme,
                .{unary.right},
            ),
        }
    }

    fn printParenthesized(
        io: *IoHandler,
        name: []const u8,
        expressions: anytype,
    ) void {
        const ExpressionsType = @TypeOf(expressions);
        const expressions_type_info = @typeInfo(ExpressionsType);

        if (expressions_type_info != .Struct) {
            @compileError("expected tuple, found " ++ @typeName(ExpressionsType));
        }

        const fields = expressions_type_info.Struct.fields;

        io.outf("({s} ", .{name});

        inline for (fields, 0..) |field, i| {
            const union_index = comptime std.fmt.parseInt(usize, field.name, 10) catch unreachable;
            expressions[union_index].print(io);

            if (i != fields.len - 1) {
                io.out(" ");
            }
        }

        io.out(")");
    }
};
