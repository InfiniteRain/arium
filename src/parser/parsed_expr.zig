const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const io_handler_mod = @import("../io_handler.zig");

const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;
const IoHandler = io_handler_mod.IoHandler;

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
        expr: anytype,
    ) void {
        const ExprsType = @TypeOf(expr);
        const exprs_type_info = @typeInfo(ExprsType);

        if (exprs_type_info != .Struct) {
            @compileError("expected tuple, found " ++ @typeName(ExprsType));
        }

        const fields = exprs_type_info.Struct.fields;

        io.outf("({s} ", .{name});

        inline for (fields, 0..) |field, i| {
            const union_index = comptime std.fmt.parseInt(usize, field.name, 10) catch unreachable;
            expr[union_index].print(io);

            if (i != fields.len - 1) {
                io.out(" ");
            }
        }

        io.out(")");
    }
};
