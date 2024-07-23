const std = @import("std");
const tokenizer_mod = @import("tokenizer.zig");
const parsed_expr_mod = @import("parsed_expr.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const expectError = std.testing.expectError;
const allocPrint = std.fmt.allocPrint;
const Token = tokenizer_mod.Token;
const Tokenizer = tokenizer_mod.Tokenizer;
const ParsedExpr = parsed_expr_mod.ParsedExpr;

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

    pub fn parse(self: *Self, tokenizer: *Tokenizer) ParserError!*ParsedExpr {
        self.deinitErrs();
        self.tokenizer = tokenizer;
        self.current_token = tokenizer.scanToken();
        self.errs = ArrayList(ParserErrorInfo).init(self.allocator);

        return try self.parseExpr();
    }

    fn parseExpr(self: *Self) ParserError!*ParsedExpr {
        return try self.parseOr();
    }

    fn parseOr(self: *Self) ParserError!*ParsedExpr {
        var expr = try self.parseAnd();

        while (self.match(.or_)) {
            const operator_token = self.previous();
            const right = try self.parseAnd();

            expr = try ParsedExpr.Binary.create(
                self.allocator,
                expr,
                operator_token,
                .or_,
                right,
            );
        }

        return expr;
    }

    fn parseAnd(self: *Self) ParserError!*ParsedExpr {
        var expr = try self.parseEquality();

        while (self.match(.and_)) {
            const operator_token = self.previous();
            const right = try self.parseEquality();

            expr = try ParsedExpr.Binary.create(
                self.allocator,
                expr,
                operator_token,
                .and_,
                right,
            );
        }

        return expr;
    }

    fn parseEquality(self: *Self) ParserError!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseComparison();

        while (self.match(.bang_equal) or self.match(.equal_equal)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .bang_equal)
                .not_equal
            else
                .equal;
            const right = try self.parseComparison();

            expr = try ParsedExpr.Binary.create(
                self.allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn parseComparison(self: *Self) ParserError!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseTerm();

        while (self.match(.greater) or
            self.match(.greater_equal) or
            self.match(.less) or
            self.match(.less_equal))
        {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = switch (operator_token.kind) {
                .greater => .greater,
                .greater_equal => .greater_equal,
                .less => .less,
                .less_equal => .less_equal,
                else => unreachable,
            };
            const right = try self.parseTerm();

            expr = try ParsedExpr.Binary.create(
                self.allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn parseTerm(self: *Self) ParserError!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseFactor();

        while (self.match(.minus) or self.match(.plus)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .subtract
            else
                .add;
            const right = try self.parseFactor();

            expr = try ParsedExpr.Binary.create(
                self.allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn parseFactor(self: *Self) ParserError!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseConcat();

        while (self.match(.slash) or self.match(.star)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .slash)
                .divide
            else
                .multiply;
            const right = try self.parseConcat();

            expr = try ParsedExpr.Binary.create(
                self.allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn parseConcat(self: *Self) ParserError!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseUnary();

        while (self.match(.plus_plus)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = .concat;
            const right = try self.parseUnary();

            expr = try ParsedExpr.Binary.create(
                self.allocator,
                expr,
                operator_token,
                operator_kind,
                right,
            );
        }

        return expr;
    }

    fn parseUnary(self: *Self) ParserError!*ParsedExpr {
        if (self.match(.minus) or self.match(.bang)) {
            const OperatorKind = ParsedExpr.Unary.OperatorKind;
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .negate_num
            else
                .negate_bool;
            const right = try self.parseUnary();

            return try ParsedExpr.Unary.create(
                self.allocator,
                operator_token,
                operator_kind,
                right,
            );
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Self) ParserError!*ParsedExpr {
        if (self.match(.true_) or self.match(.false_)) {
            return try ParsedExpr.Literal.create(self.allocator, self.previous(), .bool);
        }

        if (self.match(.int)) {
            return try ParsedExpr.Literal.create(self.allocator, self.previous(), .int);
        }

        if (self.match(.float)) {
            return try ParsedExpr.Literal.create(self.allocator, self.previous(), .float);
        }

        if (self.match(.string)) {
            return try ParsedExpr.Literal.create(self.allocator, self.previous(), .string);
        }

        if (self.match(.left_paren)) {
            const expr = try self.parseExpr();
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
    var tokenizer = try Tokenizer.init(allocator, source);
    defer tokenizer.deinit();

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
    var tokenizer = try Tokenizer.init(allocator, source);
    defer tokenizer.deinit();

    // WHEN - THEN
    var parser = Parser.init(allocator);
    defer parser.deinit();

    const result = parser.parse(&tokenizer);
    try expectError(ParserError.ParseFailure, result);
}
