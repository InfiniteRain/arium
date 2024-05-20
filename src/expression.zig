const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const io_handler_mod = @import("io_handler.zig");

const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;
const IoHandler = io_handler_mod.IoHandler;

pub const Expression = union(enum) {
    const Self = @This();

    pub const Literal = struct {
        const Kind = enum {
            int,
            float,
            bool,
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
        left: *Expression,
        operator: Token,
        right: *Expression,

        pub fn create(
            allocator: Allocator,
            left: *Expression,
            operator: Token,
            right: *Expression,
        ) !*Self {
            const expression = try allocator.create(Self);

            expression.* = .{
                .binary = .{
                    .left = left,
                    .operator = operator,
                    .right = right,
                },
            };

            return expression;
        }
    };

    pub const Unary = struct {
        operator: Token,
        right: *Expression,

        pub fn create(
            allocator: Allocator,
            operator: Token,
            right: *Expression,
        ) !*Self {
            const expression = try allocator.create(Self);

            expression.* = .{
                .unary = .{
                    .operator = operator,
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
                binary.operator.lexeme,
                .{ binary.left, binary.right },
            ),
            .literal => |literal| io.outf("{s}", .{literal.token.lexeme}),
            .unary => |unary| Self.printParenthesized(
                io,
                unary.operator.lexeme,
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
