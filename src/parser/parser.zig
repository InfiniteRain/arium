const std = @import("std");
const tokenizer_mod = @import("tokenizer.zig");
const expression_mod = @import("parsed_expression.zig");

const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Token = tokenizer_mod.Token;
const Tokenizer = tokenizer_mod.Tokenizer;
const ParsedExpression = expression_mod.ParsedExpression;

pub const ParserError = error{
    OutOfMemory,
    ParseFailure,
};

pub const ParserErrorInfo = struct {
    token: ?Token,
    message: []const u8,
};

pub const Parser = struct {
    const Self = @This();

    tokenizer: *Tokenizer,
    previous_token: Token,
    current_token: Token,
    err: ?ParserErrorInfo,

    pub fn init(tokenizer: *Tokenizer) Self {
        return .{
            .tokenizer = tokenizer,
            .previous_token = undefined,
            .current_token = tokenizer.scanToken(),
            .err = null,
        };
    }

    pub fn parse(self: *Self, allocator: Allocator) ParserError!*ParsedExpression {
        return try self.expression(allocator);
    }

    fn expression(self: *Self, allocator: Allocator) ParserError!*ParsedExpression {
        return try self.term(allocator);
    }

    fn term(self: *Self, allocator: Allocator) ParserError!*ParsedExpression {
        const OperatorKind = ParsedExpression.Binary.OperatorKind;
        var expr = try self.factor(allocator);

        while (self.match(.minus) or self.match(.plus)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .subtract
            else
                .add;
            const right = try self.factor(allocator);

            expr = try ParsedExpression.Binary.create(
                allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn factor(self: *Self, allocator: Allocator) ParserError!*ParsedExpression {
        const OperatorKind = ParsedExpression.Binary.OperatorKind;
        var expr = try self.unary(allocator);

        while (self.match(.slash) or self.match(.star)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .slash)
                .divide
            else
                .multiply;
            const right = try self.unary(allocator);

            expr =
                try ParsedExpression.Binary.create(
                allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn unary(self: *Self, allocator: Allocator) ParserError!*ParsedExpression {
        if (self.match(.minus)) {
            const operator_token = self.previous();
            const operator_kind = .negate;
            const right = try self.unary(allocator);

            return try ParsedExpression.Unary.create(
                allocator,
                operator_token,
                operator_kind,
                right,
            );
        }

        return try self.primary(allocator);
    }

    fn primary(self: *Self, allocator: Allocator) ParserError!*ParsedExpression {
        if (self.match(.true_) or self.match(.false_)) {
            return try ParsedExpression.Literal.create(allocator, self.previous(), .bool);
        }

        if (self.match(.number)) {
            return try ParsedExpression.Literal.create(allocator, self.previous(), .int);
        }

        if (self.match(.left_paren)) {
            const expr = try self.expression(allocator);
            _ = try self.consume(.right_paren, "Expect ')' after expression.");
            return expr;
        }

        return self.parserError(self.peek(), "Expect expression.");
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
        error_message: []const u8,
    ) ParserError!Token {
        if (self.check(kind)) {
            return self.advance();
        }

        return self.parserError(self.peek(), error_message);
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

    fn parserError(self: *Self, token: Token, message: []const u8) ParserError {
        self.err = .{
            .token = token,
            .message = message,
        };

        return error.ParseFailure;
    }
};

test "should free all memory" {
    const allocator = std.testing.allocator;

    const source = "(2 + 2) * -2";
    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(&tokenizer);

    const expr = try parser.parse(allocator);
    defer expr.destroy(allocator);
}
