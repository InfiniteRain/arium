const std = @import("std");
const parsed_expression_mod = @import("../parser/parsed_expression.zig");
const sema_expression_mod = @import("sema_expression.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const parser_mod = @import("../parser/parser.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const allocPrint = std.fmt.allocPrint;
const expectError = std.testing.expectError;
const ParsedExpression = parsed_expression_mod.ParsedExpression;
const SemaExpression = sema_expression_mod.SemaExpression;
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const Position = tokenizer_mod.Position;
const Parser = parser_mod.Parser;

pub const SemaError = error{
    OutOfMemory,
    SemaFailure,
};

pub const SemaErrorInfo = struct {
    message: []u8,
    position: Position,
};

pub const Sema = struct {
    const Self = @This();

    allocator: Allocator,
    errs: ArrayList(SemaErrorInfo),

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .errs = ArrayList(SemaErrorInfo).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.deinitErrs();
    }

    pub fn analyze(self: *Self, expression: *ParsedExpression) SemaError!*SemaExpression {
        self.deinitErrs();
        self.errs = ArrayList(SemaErrorInfo).init(self.allocator);

        const sema_expression = try self.analyzeExpression(expression);

        if (self.errs.items.len > 0) {
            sema_expression.destroy(self.allocator);
            return error.SemaFailure;
        }

        return sema_expression;
    }

    fn analyzeExpression(
        self: *Self,
        expression: *ParsedExpression,
    ) SemaError!*SemaExpression {
        switch (expression.*) {
            .literal => |literal| {
                var lexeme = literal.token.lexeme;
                const literal_variant: SemaExpression.Kind.Literal = switch (literal.kind) {
                    .int => .{ .int = std.fmt.parseInt(i64, lexeme, 10) catch unreachable },
                    .float => .{ .float = std.fmt.parseFloat(f64, lexeme) catch unreachable },
                    .bool => .{ .bool = lexeme.len == 4 },
                    .string => .{ .string = lexeme[1 .. lexeme.len - 1] },
                };
                const position = literal.token.position;

                return SemaExpression.Kind.Literal.create(self.allocator, literal_variant, position);
            },
            .binary => |binary| {
                const BinaryKind = SemaExpression.Kind.Binary.Kind;
                const left = try self.analyzeExpression(binary.left);
                const right = try self.analyzeExpression(binary.right);
                var binary_kind: BinaryKind = undefined;

                if (left.eval_type != .int and left.eval_type != .float) {
                    try self.semaError(
                        binary.operator_token.position,
                        "Can't perform arithmetic operation on {s}.",
                        .{left.eval_type.stringify()},
                    );
                    binary_kind = .invalid;
                } else if (left.eval_type != right.eval_type) {
                    try self.semaError(
                        binary.operator_token.position,
                        "Operand is expected to be of type {s}, got {s}.",
                        .{ left.eval_type.stringify(), right.eval_type.stringify() },
                    );
                    binary_kind = .invalid;
                } else if (left.eval_type == .int) {
                    binary_kind = switch (binary.operator_kind) {
                        .add => .add_int,
                        .subtract => .subtract_int,
                        .multiply => .multiply_int,
                        .divide => .divide_int,
                    };
                } else {
                    binary_kind = switch (binary.operator_kind) {
                        .add => .add_float,
                        .subtract => .subtract_float,
                        .multiply => .multiply_float,
                        .divide => .divide_float,
                    };
                }
                const position = binary.operator_token.position;

                return SemaExpression.Kind.Binary.create(
                    self.allocator,
                    binary_kind,
                    left.eval_type,
                    position,
                    left,
                    right,
                );
            },
            .unary => |unary| {
                const UnaryKind = SemaExpression.Kind.Unary.Kind;
                const right = try self.analyzeExpression(unary.right);
                const eval_type = right.eval_type;
                var unary_kind: UnaryKind = undefined;

                if (unary.operator_kind == .negate_bool) switch (right.eval_type) {
                    .bool => {
                        unary_kind = .negate_bool;
                    },
                    else => {
                        try self.semaError(
                            unary.operator_token.position,
                            "Can't perform logical negation on {s}.",
                            .{eval_type.stringify()},
                        );
                        unary_kind = .invalid;
                    },
                } else switch (right.eval_type) {
                    .int => {
                        unary_kind = .negate_int;
                    },
                    .float => {
                        unary_kind = .negate_float;
                    },
                    else => {
                        try self.semaError(
                            unary.operator_token.position,
                            "Can't perform arithmetic negation on {s}.",
                            .{eval_type.stringify()},
                        );
                        unary_kind = .invalid;
                    },
                }
                const position = unary.operator_token.position;

                return SemaExpression.Kind.Unary.create(
                    self.allocator,
                    unary_kind,
                    eval_type,
                    position,
                    right,
                );
            },
        }
    }

    fn semaError(
        self: *Self,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
    ) SemaError!void {
        const message = try allocPrint(self.allocator, fmt, args);

        try self.errs.append(.{
            .message = message,
            .position = position,
        });
    }

    fn deinitErrs(self: *Self) void {
        for (self.errs.items) |semaErr| {
            self.allocator.free(semaErr.message);
        }

        self.errs.deinit();
    }
};

test "should free all memory on successful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "(2 + 2) * -2";
    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(allocator);

    const parsed_expr = try parser.parse(&tokenizer);
    defer parsed_expr.destroy(allocator);

    // WHEN - THEN
    var sema = Sema.init(allocator);
    defer sema.deinit();

    const sema_expr = try sema.analyze(parsed_expr);
    defer sema_expr.destroy(allocator);
}

test "should free all memory on unsuccessful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "(2 + 2) * -false";
    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(allocator);

    const parsed_expr = try parser.parse(&tokenizer);
    defer parsed_expr.destroy(allocator);

    // WHEN - THEN
    var sema = Sema.init(allocator);
    defer sema.deinit();

    const result = sema.analyze(parsed_expr);
    try expectError(SemaError.SemaFailure, result);
}
