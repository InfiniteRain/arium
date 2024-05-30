const std = @import("std");
const parsed_expr_mod = @import("../parser/parsed_expr.zig");
const sema_expr_mod = @import("sema_expr.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const parser_mod = @import("../parser/parser.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const allocPrint = std.fmt.allocPrint;
const expectError = std.testing.expectError;
const ParsedExpr = parsed_expr_mod.ParsedExpr;
const SemaExpr = sema_expr_mod.SemaExpr;
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

    pub fn analyze(self: *Self, expr: *ParsedExpr) SemaError!*SemaExpr {
        self.deinitErrs();
        self.errs = ArrayList(SemaErrorInfo).init(self.allocator);

        const sema_expr = try self.analyzeExpr(expr);

        if (self.errs.items.len > 0) {
            sema_expr.destroy(self.allocator);
            return error.SemaFailure;
        }

        return sema_expr;
    }

    fn analyzeExpr(
        self: *Self,
        expr: *ParsedExpr,
    ) SemaError!*SemaExpr {
        switch (expr.*) {
            .literal => |literal| {
                var lexeme = literal.token.lexeme;
                const literal_variant: SemaExpr.Kind.Literal = switch (literal.kind) {
                    .int => .{ .int = std.fmt.parseInt(i64, lexeme, 10) catch unreachable },
                    .float => .{ .float = std.fmt.parseFloat(f64, lexeme) catch unreachable },
                    .bool => .{ .bool = lexeme.len == 4 },
                    .string => .{ .string = lexeme[1 .. lexeme.len - 1] },
                };
                const position = literal.token.position;

                return SemaExpr.Kind.Literal.create(self.allocator, literal_variant, position);
            },
            .binary => |binary| {
                const BinaryKind = SemaExpr.Kind.Binary.Kind;
                const left = try self.analyzeExpr(binary.left);
                const right = try self.analyzeExpr(binary.right);
                var binary_kind: BinaryKind = undefined;

                if (left.kind == .invalid or right.kind == .invalid) {
                    return self.createInvalidExpr();
                }

                switch (binary.operator_kind) {
                    .add, .subtract, .divide, .multiply => {
                        if (left.eval_type != .int and left.eval_type != .float) {
                            return try self.semaErrorWithInvalidExpr(
                                binary.operator_token.position,
                                "Can't perform arithmetic operation on {s}.",
                                .{left.eval_type.stringify()},
                            );
                        }

                        if (left.eval_type != right.eval_type) {
                            return try self.semaErrorWithInvalidExpr(
                                binary.operator_token.position,
                                "Operand is expected to be of type {s}, got {s}.",
                                .{ left.eval_type.stringify(), right.eval_type.stringify() },
                            );
                        }

                        binary_kind = if (left.eval_type == .int) switch (binary.operator_kind) {
                            .add => .add_int,
                            .subtract => .subtract_int,
                            .multiply => .multiply_int,
                            .divide => .divide_int,
                            else => unreachable,
                        } else switch (binary.operator_kind) {
                            .add => .add_float,
                            .subtract => .subtract_float,
                            .multiply => .multiply_float,
                            .divide => .divide_float,
                            else => unreachable,
                        };
                    },
                    .concat => {
                        if (left.eval_type != .string or right.eval_type != .string) {
                            return try self.semaErrorWithInvalidExpr(
                                binary.operator_token.position,
                                "Can't perform a concatenation on types {s} and {s}.",
                                .{ left.eval_type.stringify(), right.eval_type.stringify() },
                            );
                        }

                        binary_kind = .concat;
                    },
                }

                const position = binary.operator_token.position;

                return SemaExpr.Kind.Binary.create(
                    self.allocator,
                    binary_kind,
                    left.eval_type,
                    position,
                    left,
                    right,
                );
            },
            .unary => |unary| {
                const UnaryKind = SemaExpr.Kind.Unary.Kind;
                const right = try self.analyzeExpr(unary.right);

                if (right.kind == .invalid) {
                    return self.createInvalidExpr();
                }

                const eval_type = right.eval_type;
                const unary_kind: UnaryKind = if (unary.operator_kind == .negate_bool)
                    switch (right.eval_type) {
                        .bool => .negate_bool,
                        else => return try self.semaErrorWithInvalidExpr(
                            unary.operator_token.position,
                            "Can't perform logical negation on {s}.",
                            .{eval_type.stringify()},
                        ),
                    }
                else switch (right.eval_type) {
                    .int => .negate_int,
                    .float => .negate_float,
                    else => return try self.semaErrorWithInvalidExpr(
                        unary.operator_token.position,
                        "Can't perform arithmetic negation on {s}.",
                        .{eval_type.stringify()},
                    ),
                };
                const position = unary.operator_token.position;

                return SemaExpr.Kind.Unary.create(
                    self.allocator,
                    unary_kind,
                    eval_type,
                    position,
                    right,
                );
            },
        }
    }

    fn createInvalidExpr(self: *const Self) SemaError!*SemaExpr {
        return try SemaExpr.Kind.Invalid.create(self.allocator);
    }

    fn semaErrorWithInvalidExpr(
        self: *Self,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
    ) SemaError!*SemaExpr {
        try self.semaError(position, fmt, args);
        return try self.createInvalidExpr();
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
