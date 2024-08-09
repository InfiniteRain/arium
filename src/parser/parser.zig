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

        var stmts = ArrayList(*ParsedStmt).init(self.allocator);
        var had_error = false;

        while (!self.isAtEnd()) {
            if (self.parseStmt()) |stmt| {
                if (had_error) {
                    stmt.destroy(self.allocator);
                } else {
                    try stmts.append(stmt);
                }
            } else |err| switch (err) {
                error.ParseFailure => {
                    had_error = true;

                    for (stmts.items) |stmt| {
                        stmt.destroy(self.allocator);
                    }

                    stmts.clearAndFree();
                    self.synchronize();
                },
                else => return err,
            }
        }

        if (had_error) {
            return error.ParseFailure;
        }

        return try ParsedStmt.Kind.Block.create(self.allocator, stmts, .{
            .line = 1,
            .column = 1,
        });
    }

    fn parseStmt(self: *Self) Error!*ParsedStmt {
        if (self.match(.assert)) {
            return try self.assertStatement(self.previous().position);
        }

        if (self.match(.print)) {
            return try self.printStatement(self.previous().position);
        }

        try self.parserError(self.peek(), "Expected statement.", .{});

        return error.ParseFailure;
    }

    fn assertStatement(self: *Self, position: Position) Error!*ParsedStmt {
        _ = try self.consume(.left_paren, "Expected '(' before expression.");

        const expr = try self.parseExpr();
        errdefer expr.destroy(self.allocator);

        _ = try self.consume(.right_paren, "Expected ')' after expression.");

        return try ParsedStmt.Kind.Assert.create(self.allocator, expr, position);
    }

    fn printStatement(self: *Self, position: Position) Error!*ParsedStmt {
        const expr = try self.parseExpr();
        errdefer expr.destroy(self.allocator);

        return try ParsedStmt.Kind.Print.create(self.allocator, expr, position);
    }

    fn parseExpr(self: *Self) Error!*ParsedExpr {
        return try self.parseOr();
    }

    fn parseOr(self: *Self) Error!*ParsedExpr {
        var expr = try self.parseAnd();
        errdefer expr.destroy(self.allocator);

        while (self.match(.or_)) {
            const operator_token = self.previous();
            const right = try self.parseAnd();
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

    fn parseAnd(self: *Self) Error!*ParsedExpr {
        var expr = try self.parseEquality();
        errdefer expr.destroy(self.allocator);

        while (self.match(.and_)) {
            const operator_token = self.previous();
            const right = try self.parseEquality();
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

    fn parseEquality(self: *Self) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseComparison();
        errdefer expr.destroy(self.allocator);

        while (self.match(.bang_equal) or self.match(.equal_equal)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .bang_equal)
                .not_equal
            else
                .equal;
            const right = try self.parseComparison();
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

    fn parseComparison(self: *Self) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseTerm();
        errdefer expr.destroy(self.allocator);

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

    fn parseTerm(self: *Self) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseFactor();
        errdefer expr.destroy(self.allocator);

        while (self.match(.minus) or self.match(.plus)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .subtract
            else
                .add;
            const right = try self.parseFactor();
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

    fn parseFactor(self: *Self) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseConcat();
        errdefer expr.destroy(self.allocator);

        while (self.match(.slash) or self.match(.star)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .slash)
                .divide
            else
                .multiply;
            const right = try self.parseConcat();
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

    fn parseConcat(self: *Self) Error!*ParsedExpr {
        const OperatorKind = ParsedExpr.Binary.OperatorKind;
        var expr = try self.parseUnary();
        errdefer expr.destroy(self.allocator);

        while (self.match(.plus_plus)) {
            const operator_token = self.previous();
            const operator_kind: OperatorKind = .concat;
            const right = try self.parseUnary();
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

    fn parseUnary(self: *Self) Error!*ParsedExpr {
        if (self.match(.minus) or self.match(.not)) {
            const OperatorKind = ParsedExpr.Unary.OperatorKind;
            const operator_token = self.previous();
            const operator_kind: OperatorKind = if (operator_token.kind == .minus)
                .negate_num
            else
                .negate_bool;
            const right = try self.parseUnary();
            errdefer right.destroy(self.allocator);

            return try ParsedExpr.Unary.create(
                self.allocator,
                operator_token,
                operator_kind,
                right,
            );
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Self) Error!*ParsedExpr {
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
    ) Error!Token {
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
        self.current_token = self.tokenizer.scanNonCommentToken();

        if (self.current_token.kind == .comment) {
            _ = self.advance();
        }

        return self.previous_token;
    }

    fn synchronize(self: *Self) void {
        while (self.current_token.kind != .eof) {
            if (self.previous_token.kind == .semicolon) {
                return;
            }

            switch (self.current_token.kind) {
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
            const message = try allocPrint(self.allocator, fmt, args);

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
