const std = @import("std");
const shared = @import("shared");
const tokenizer_mod = @import("tokenizer.zig");
const parsed_expr_mod = @import("parsed_expr.zig");
const parsed_stmt_mod = @import("parsed_stmt.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const expectError = std.testing.expectError;
const allocPrint = std.fmt.allocPrint;
const SharedDiagnostics = shared.Diagnostics;
const Token = tokenizer_mod.Token;
const Tokenizer = tokenizer_mod.Tokenizer;
const Position = tokenizer_mod.Position;
const ParsedExpr = parsed_expr_mod.ParsedExpr;
const ParsedStmt = parsed_stmt_mod.ParsedStmt;

pub const Parser = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        ParseFailure,
    };

    pub const DiagnosticEntry = struct {
        message: []u8,
        token: Token,

        pub fn deinit(self: *DiagnosticEntry, allocator: Allocator) void {
            allocator.free(self.message);
        }
    };

    pub const Diagnostics = SharedDiagnostics(DiagnosticEntry);

    allocator: Allocator,
    tokenizer: *Tokenizer = undefined,
    previous_token: Token = undefined,
    current_token: Token = undefined,
    diagnostics: ?*Diagnostics = null,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn parse(
        self: *Self,
        tokenizer: *Tokenizer,
        diagnostics: ?*Diagnostics,
    ) Error!*ParsedStmt {
        self.tokenizer = tokenizer;
        self.current_token = tokenizer.scanNonCommentToken();
        self.diagnostics = diagnostics;

        return try self.parseBlock(.eof, .{ .line = 1, .column = 1 });
    }

    fn parseBlock(
        self: *Self,
        end_token_kind: Token.Kind,
        position: Position,
    ) Error!*ParsedStmt {
        var stmts = ArrayList(*ParsedStmt).init(self.allocator);
        errdefer stmts.clearAndFree();

        var block = try ParsedStmt.Kind.Block.create(
            self.allocator,
            stmts,
            position,
        );
        errdefer block.destroy(self.allocator);

        if (self.match(end_token_kind, true)) {
            return block;
        }

        try block.kind.block.stmts.append(try self.parseStmt());

        while (self.matchStatementTerminator()) {
            if (self.match(end_token_kind, false)) {
                return block;
            }

            try block.kind.block.stmts.append(try self.parseStmt());
        }

        _ = try self.consume(end_token_kind, "Unexpected token (use ';' to separate statements on the same line).");

        return block;
    }

    fn parseStmt(self: *Self) Error!*ParsedStmt {
        errdefer self.synchronize();

        if (self.match(.assert, false)) {
            return try self.parseAssertStmt(self.previous().position);
        }

        if (self.match(.print, false)) {
            return try self.parsePrintStmt(self.previous().position);
        }

        if (self.match(.invalid, false)) {
            try self.parserError(self.previous(), "{s}", .{self.previous().lexeme});
        } else {
            try self.parserError(self.peek(), "Expected statement.", .{});
        }

        return error.ParseFailure;
    }

    fn parseAssertStmt(self: *Self, position: Position) Error!*ParsedStmt {
        _ = try self.consume(.left_paren, "Expected '(' before expression.");

        const expr = try self.parseExpr(false);
        errdefer expr.destroy(self.allocator);

        _ = try self.consume(.right_paren, "Expected ')' after expression.");

        return try ParsedStmt.Kind.Assert.create(self.allocator, expr, position);
    }

    fn parsePrintStmt(self: *Self, position: Position) Error!*ParsedStmt {
        const expr = try self.parseExpr(false);
        errdefer expr.destroy(self.allocator);
        return try ParsedStmt.Kind.Print.create(self.allocator, expr, position);
    }

    fn parseExpr(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        return try self.parseOr(ignore_new_line);
    }

    fn parseOr(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        var expr = try self.parseAnd(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.or_, ignore_new_line)) {
            self.skipNewLines();

            const operator_token = self.previous();
            const right = try self.parseAnd(ignore_new_line);
            errdefer right.destroy(self.allocator);

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

    fn parseAnd(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        var expr = try self.parseEquality(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.and_, ignore_new_line)) {
            self.skipNewLines();

            const operator_token = self.previous();
            const right = try self.parseEquality(ignore_new_line);
            errdefer right.destroy(self.allocator);

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

    fn parseEquality(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseComparison(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.bang_equal, ignore_new_line) or
            self.match(.equal_equal, ignore_new_line))
        {
            self.skipNewLines();

            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .bang_equal)
                .not_equal
            else
                .equal;
            const right = try self.parseComparison(ignore_new_line);
            errdefer right.destroy(self.allocator);

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

    fn parseComparison(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseTerm(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.greater, ignore_new_line) or
            self.match(.greater_equal, ignore_new_line) or
            self.match(.less, ignore_new_line) or
            self.match(.less_equal, ignore_new_line))
        {
            self.skipNewLines();

            const operator_token = self.previous();
            const operator_kind: OperatorKind = switch (operator_token.kind) {
                .greater => .greater,
                .greater_equal => .greater_equal,
                .less => .less,
                .less_equal => .less_equal,
                else => unreachable,
            };
            const right = try self.parseTerm(ignore_new_line);
            errdefer right.destroy(self.allocator);

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

    fn parseTerm(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseFactor(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.minus, ignore_new_line) or
            self.match(.plus, ignore_new_line))
        {
            self.skipNewLines();

            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .subtract
            else
                .add;
            const right = try self.parseFactor(ignore_new_line);
            errdefer right.destroy(self.allocator);

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

    fn parseFactor(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseConcat(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.slash, ignore_new_line) or
            self.match(.star, ignore_new_line))
        {
            self.skipNewLines();

            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .slash)
                .divide
            else
                .multiply;
            const right = try self.parseConcat(ignore_new_line);
            errdefer right.destroy(self.allocator);

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

    fn parseConcat(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseUnary(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.plus_plus, ignore_new_line)) {
            self.skipNewLines();

            const operator_token = self.previous();
            const operator_kind: OperatorKind = .concat;
            const right = try self.parseUnary(ignore_new_line);
            errdefer right.destroy(self.allocator);

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

    fn parseUnary(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        if (self.match(.minus, ignore_new_line) or
            self.match(.not, ignore_new_line))
        {
            self.skipNewLines();

            const OperatorKind = ParsedExpr.Unary.OperatorKind;
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .negate_num
            else
                .negate_bool;
            const right = try self.parseUnary(ignore_new_line);
            errdefer right.destroy(self.allocator);

            return try ParsedExpr.Unary.create(
                self.allocator,
                operator_token,
                operator_kind,
                right,
            );
        }

        return try self.parsePrimary(ignore_new_line);
    }

    fn parsePrimary(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        if (self.match(.true_, ignore_new_line) or self.match(.false_, ignore_new_line)) {
            return try ParsedExpr.Literal.create(self.allocator, self.previous(), .bool);
        }

        if (self.match(.int, ignore_new_line)) {
            return try ParsedExpr.Literal.create(self.allocator, self.previous(), .int);
        }

        if (self.match(.float, ignore_new_line)) {
            return try ParsedExpr.Literal.create(self.allocator, self.previous(), .float);
        }

        if (self.match(.string, ignore_new_line)) {
            return try ParsedExpr.Literal.create(self.allocator, self.previous(), .string);
        }

        if (self.match(.left_paren, ignore_new_line)) {
            const expr = try self.parseExpr(true);
            errdefer expr.destroy(self.allocator);

            _ = try self.consume(.right_paren, "Expected ')' after expression.");
            return expr;
        }

        if (self.match(.invalid, ignore_new_line)) {
            try self.parserError(self.previous(), "{s}", .{self.previous().lexeme});
        } else {
            try self.parserError(self.peek(), "Expected expression.", .{});
        }

        return error.ParseFailure;
    }

    fn match(self: *Self, kind: Token.Kind, ignore_new_line: bool) bool {
        if (ignore_new_line) {
            self.skipNewLines();
        }

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
    ) Error!Token {
        if (self.check(kind)) {
            return self.advance();
        }

        try self.parserError(self.peek(), error_message, .{});
        return error.ParseFailure;
    }

    fn matchStatementTerminator(self: *Self) bool {
        if (self.check(.new_line)) {
            self.skipNewLines();
            return true;
        }

        if (self.check(.semicolon)) {
            _ = self.advance();
            self.skipNewLines();
            return true;
        }

        return false;
    }

    fn check(self: *Self, kind: Token.Kind) bool {
        return self.peek().kind == kind;
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().kind == .eof;
    }

    fn peek(self: *Self) Token {
        return self.current_token;
    }

    fn previous(self: *Self) Token {
        return self.previous_token;
    }

    fn advance(self: *Self) Token {
        if (self.current_token.kind == .eof) {
            return self.current_token;
        }

        self.previous_token = self.current_token;
        self.current_token = self.tokenizer.scanNonCommentToken();

        return self.previous_token;
    }

    fn skipNewLines(self: *Self) void {
        while (self.check(.new_line)) {
            _ = self.advance();
        }
    }

    fn synchronize(self: *Self) void {
        while (self.peek().kind != .eof) {
            if (self.previous().kind == .semicolon) {
                self.skipNewLines();
                return;
            }

            switch (self.peek().kind) {
                .print,
                .assert,
                => return,

                else => _ = self.advance(),
            }
        }
    }

    fn parserError(
        self: *Self,
        token: Token,
        comptime fmt: []const u8,
        args: anytype,
    ) Error!void {
        if (self.diagnostics) |diagnostics| {
            // allocating with diagnostic's allocator, for an edge case found
            // in lang-tests, where new allocator is created for each test
            // to detect memory leaks; this line makes it so that diagnostics
            // could be created with the base allocator instead of an allocator
            // local to the test.
            const message = try allocPrint(diagnostics.allocator, fmt, args);

            try diagnostics.add(.{
                .token = token,
                .message = message,
            });
        }
    }
};

test "should free all memory on successful parse" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "print (2 + 2) * -2";
    var tokenizer = Tokenizer.init(source);

    // WHEN - THEN
    var parser = Parser.init(allocator);

    const expr = try parser.parse(&tokenizer, null);
    defer expr.destroy(allocator);
}

test "should free all memory on unsuccessful parse" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "print (2 + 2) print -2 + 2)";
    var tokenizer = Tokenizer.init(source);

    // WHEN - THEN
    var parser = Parser.init(allocator);

    const result = parser.parse(&tokenizer, null);
    try expectError(Parser.Error.ParseFailure, result);
}
