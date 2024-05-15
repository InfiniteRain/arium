const std = @import("std");
const tokenizer_pkg = @import("tokenizer.zig");
const expression_pkg = @import("expression.zig");

const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Token = tokenizer_pkg.Token;
const Tokenizer = tokenizer_pkg.Tokenizer;
const Expression = expression_pkg.Expression;

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

    pub fn parse(self: *Self, allocator: Allocator) ParserError!*Expression {
        return try self.expression(allocator);
    }

    fn expression(self: *Self, allocator: Allocator) ParserError!*Expression {
        return try self.term(allocator);
    }

    fn term(self: *Self, allocator: Allocator) ParserError!*Expression {
        var expr = try self.factor(allocator);

        while (self.match(.minus) or self.match(.plus)) {
            const operator = self.previous();
            const right = try self.factor(allocator);

            expr = try Expression.Binary.create(
                allocator,
                expr,
                operator,
                right,
            );
        }

        return expr;
    }

    fn factor(self: *Self, allocator: Allocator) ParserError!*Expression {
        var expr = try self.unary(allocator);

        while (self.match(.slash) or self.match(.star)) {
            const operator = self.previous();
            const right = try self.unary(allocator);

            expr =
                try Expression.Binary.create(
                allocator,
                expr,
                operator,
                right,
            );
        }

        return expr;
    }

    fn unary(self: *Self, allocator: Allocator) ParserError!*Expression {
        if (self.match(.minus)) {
            const operator = self.previous();
            const right = try self.unary(allocator);

            return try Expression.Unary.create(
                allocator,
                operator,
                right,
            );
        }

        return try self.primary(allocator);
    }

    fn primary(self: *Self, allocator: Allocator) ParserError!*Expression {
        if (self.match(.number)) {
            return try Expression.Literal.create(
                allocator,
                self.previous(),
            );
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
