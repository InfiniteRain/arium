const std = @import("std");
const tokenizer_mod = @import("tokenizer.zig");
const parsed_expression_mod = @import("parsed_expression.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const expectError = std.testing.expectError;
const allocPrint = std.fmt.allocPrint;
const Token = tokenizer_mod.Token;
const Tokenizer = tokenizer_mod.Tokenizer;
const ParsedExpression = parsed_expression_mod.ParsedExpression;

pub const ParserError = error{
    OutOfMemory,
    ParseFailure,
};

pub const ParserErrorInfo = struct {
    token: Token,
    message: []u8,
};

pub const Parser = struct {
    const Self = @This();

    allocator: Allocator,
    tokenizer: *Tokenizer = undefined,
    previous_token: Token = undefined,
    current_token: Token = undefined,
    errs: ArrayList(ParserErrorInfo),

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .errs = ArrayList(ParserErrorInfo).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.deinitErrs();
    }

    pub fn parse(self: *Self, tokenizer: *Tokenizer) ParserError!*ParsedExpression {
        self.deinitErrs();
        self.tokenizer = tokenizer;
        self.current_token = tokenizer.scanToken();
        self.errs = ArrayList(ParserErrorInfo).init(self.allocator);

        return try self.expression();
    }

    fn expression(self: *Self) ParserError!*ParsedExpression {
        return try self.term();
    }

    fn term(self: *Self) ParserError!*ParsedExpression {
        const OperatorKind = ParsedExpression.Binary.OperatorKind;
        var expr = try self.factor();

        while (self.match(.minus) or self.match(.plus)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .subtract
            else
                .add;
            const right = try self.factor();

            expr = try ParsedExpression.Binary.create(
                self.allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn factor(self: *Self) ParserError!*ParsedExpression {
        const OperatorKind = ParsedExpression.Binary.OperatorKind;
        var expr = try self.unary();

        while (self.match(.slash) or self.match(.star)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .slash)
                .divide
            else
                .multiply;
            const right = try self.unary();

            expr =
                try ParsedExpression.Binary.create(
                self.allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn unary(self: *Self) ParserError!*ParsedExpression {
        if (self.match(.minus) or self.match(.bang)) {
            const OperatorKind = ParsedExpression.Unary.OperatorKind;
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .negate_num
            else
                .negate_bool;
            const right = try self.unary();

            return try ParsedExpression.Unary.create(
                self.allocator,
                operator_token,
                operator_kind,
                right,
            );
        }

        return try self.primary();
    }

    fn primary(self: *Self) ParserError!*ParsedExpression {
        if (self.match(.true_) or self.match(.false_)) {
            return try ParsedExpression.Literal.create(self.allocator, self.previous(), .bool);
        }

        if (self.match(.int)) {
            return try ParsedExpression.Literal.create(self.allocator, self.previous(), .int);
        }

        if (self.match(.float)) {
            return try ParsedExpression.Literal.create(self.allocator, self.previous(), .float);
        }

        if (self.match(.string)) {
            return try ParsedExpression.Literal.create(self.allocator, self.previous(), .string);
        }

        if (self.match(.left_paren)) {
            const expr = try self.expression();
            errdefer expr.destroy(self.allocator);

            _ = try self.consume(.right_paren, "Expected ')' after expression.");
            return expr;
        }

        try self.parserError(self.peek(), "Expected expression.", .{});

        return error.ParseFailure;
    }

    fn match(self: *Self, kind: Token.Kind) bool {
        if (self.check(kind)) {
            _ = self.advance();
            return true;
        }

        return false;
    }

    fn consume(
        self: *Self,
        kind: Token.Kind,
        comptime error_message: []const u8,
    ) ParserError!Token {
        if (self.check(kind)) {
            return self.advance();
        }

        try self.parserError(self.peek(), error_message, .{});

        return error.ParseFailure;
    }

    fn check(self: *const Self, kind: Token.Kind) bool {
        if (self.isAtEnd()) {
            return false;
        }

        return self.peek().kind == kind;
    }

    fn isAtEnd(self: *const Self) bool {
        return self.peek().kind == .eof;
    }

    fn peek(self: *const Self) Token {
        return self.current_token;
    }

    fn previous(self: *const Self) Token {
        return self.previous_token;
    }

    fn advance(self: *Self) Token {
        assert(self.current_token.kind != .eof);

        self.previous_token = self.current_token;
        self.current_token = self.tokenizer.scanToken();

        return self.previous_token;
    }

    fn parserError(
        self: *Self,
        token: Token,
        comptime fmt: []const u8,
        args: anytype,
    ) ParserError!void {
        const message = try allocPrint(self.allocator, fmt, args);

        try self.errs.append(.{
            .token = token,
            .message = message,
        });
    }

    fn deinitErrs(self: *Self) void {
        for (self.errs.items) |parserErr| {
            self.allocator.free(parserErr.message);
        }

        self.errs.deinit();
    }
};

test "should free all memory on successful parse" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "(2 + 2) * -2";
    var tokenizer = Tokenizer.init(source);

    // WHEN - THEN
    var parser = Parser.init(allocator);
    defer parser.deinit();

    const expr = try parser.parse(&tokenizer);
    defer expr.destroy(allocator);
}

test "should free all memory on unsuccessful parse" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "(2 + 2 * (-2 + 2)";
    var tokenizer = Tokenizer.init(source);

    // WHEN - THEN
    var parser = Parser.init(allocator);
    defer parser.deinit();

    const result = parser.parse(&tokenizer);
    try expectError(ParserError.ParseFailure, result);
}
