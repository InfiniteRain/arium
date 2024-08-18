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
const Writer = shared.Writer;
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
        pub const Kind = union(enum) {
            expected_end_token: Token.Kind,
            invalid_token: []const u8,
            expected_statement,
            expected_expression,
            expected_left_paren_before_expr,
            expected_right_paren_after_expr,
            int_literal_overflows,
        };

        kind: Kind,
        token: Token,

        pub fn deinit(self: *DiagnosticEntry, allocator: Allocator) void {
            _ = self; // autofix
            _ = allocator; // autofix
        }

        pub fn printMessage(self: *const DiagnosticEntry, writer: *const Writer) void {
            return switch (self.kind) {
                .expected_end_token,
                => |token_kind| {
                    writer.print("Expected ");
                    token_kind.printQuoted(writer);
                    writer.print(", line break or ';'.");
                },

                .invalid_token,
                => |message| writer.print(message),

                .expected_statement,
                => writer.print("Expected statement."),

                .expected_expression,
                => writer.print("Expected expression."),

                .expected_left_paren_before_expr,
                => writer.print("Expected '(' before expression."),

                .expected_right_paren_after_expr,
                => writer.print("Expected ')' after expression."),

                .int_literal_overflows,
                => writer.print("Integer literal value overflows."),
            };
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

        const old_num_diags = if (self.diagnostics) |diags| diags.getLen() else 0;

        while (true) {
            const stmt = self.parseStmt() catch |err| switch (err) {
                error.ParseFailure => {
                    // todo: perhaps add semicolon error message here

                    if (self.check(.eof)) {
                        break;
                    }

                    continue;
                },
                else => return err,
            };

            try block.kind.block.stmts.append(stmt);

            if (!self.matchStmtTerminator() or self.check(end_token_kind)) {
                break;
            }
        }

        _ = try self.consume(
            end_token_kind,
            .{ .expected_end_token = end_token_kind },
        );

        if (self.diagnostics != null and self.diagnostics.?.getLen() > old_num_diags) {
            return error.ParseFailure;
        }

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
            try self.parserError(self.previous(), .{
                .invalid_token = self.previous().lexeme,
            });
        } else {
            try self.parserError(self.peek(), .expected_statement);
        }

        return error.ParseFailure;
    }

    fn parseAssertStmt(self: *Self, position: Position) Error!*ParsedStmt {
        _ = try self.consume(.left_paren, .expected_left_paren_before_expr);

        const expr = try self.parseExpr(false);
        errdefer expr.destroy(self.allocator);

        _ = try self.consume(.right_paren, .expected_right_paren_after_expr);

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
        const position = self.peek().position;
        var expr = try self.parseAnd(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.or_, ignore_new_line)) {
            self.skipNewLines();

            const right = try self.parseAnd(ignore_new_line);
            errdefer right.destroy(self.allocator);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                .or_,
                right,
                position,
            );
        }

        return expr;
    }

    fn parseAnd(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        var expr = try self.parseEquality(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.and_, ignore_new_line)) {
            self.skipNewLines();

            const right = try self.parseEquality(ignore_new_line);
            errdefer right.destroy(self.allocator);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                .and_,
                right,
                position,
            );
        }

        return expr;
    }

    fn parseEquality(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        const BinaryKind = ParsedExpr.Kind.Binary.Kind;
        var expr = try self.parseComparison(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.bang_equal, ignore_new_line) or
            self.match(.equal_equal, ignore_new_line))
        {
            self.skipNewLines();

            const kind: BinaryKind = if (self.previous().kind == .bang_equal)
                .not_equal
            else
                .equal;
            const right = try self.parseComparison(ignore_new_line);
            errdefer right.destroy(self.allocator);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                kind,
                right,
                position,
            );
        }

        return expr;
    }

    fn parseComparison(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        const BinaryKind = ParsedExpr.Kind.Binary.Kind;
        var expr = try self.parseTerm(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.greater, ignore_new_line) or
            self.match(.greater_equal, ignore_new_line) or
            self.match(.less, ignore_new_line) or
            self.match(.less_equal, ignore_new_line))
        {
            self.skipNewLines();

            const kind: BinaryKind = switch (self.previous().kind) {
                .greater => .greater,
                .greater_equal => .greater_equal,
                .less => .less,
                .less_equal => .less_equal,
                else => unreachable,
            };
            const right = try self.parseTerm(ignore_new_line);
            errdefer right.destroy(self.allocator);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                kind,
                right,
                position,
            );
        }

        return expr;
    }

    fn parseTerm(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        const BinaryKind = ParsedExpr.Kind.Binary.Kind;
        var expr = try self.parseFactor(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.minus, ignore_new_line) or
            self.match(.plus, ignore_new_line))
        {
            self.skipNewLines();

            const kind: BinaryKind = if (self.previous().kind == .minus)
                .subtract
            else
                .add;
            const right = try self.parseFactor(ignore_new_line);
            errdefer right.destroy(self.allocator);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                kind,
                right,
                position,
            );
        }

        return expr;
    }

    fn parseFactor(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        const BinaryKind = ParsedExpr.Kind.Binary.Kind;
        var expr = try self.parseConcat(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.slash, ignore_new_line) or
            self.match(.star, ignore_new_line))
        {
            self.skipNewLines();

            const kind: BinaryKind = if (self.previous().kind == .slash)
                .divide
            else
                .multiply;
            const right = try self.parseConcat(ignore_new_line);
            errdefer right.destroy(self.allocator);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                kind,
                right,
                position,
            );
        }

        return expr;
    }

    fn parseConcat(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        const BinaryKind = ParsedExpr.Kind.Binary.Kind;
        var expr = try self.parseUnary(ignore_new_line);
        errdefer expr.destroy(self.allocator);

        while (self.match(.plus_plus, ignore_new_line)) {
            self.skipNewLines();

            const kind = BinaryKind.concat;
            const right = try self.parseUnary(ignore_new_line);
            errdefer right.destroy(self.allocator);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                kind,
                right,
                position,
            );
        }

        return expr;
    }

    fn parseUnary(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        if (self.match(.minus, ignore_new_line) or
            self.match(.not, ignore_new_line))
        {
            self.skipNewLines();

            const UnaryKind = ParsedExpr.Kind.Unary.Kind;
            const token = self.previous();
            const kind: UnaryKind = if (token.kind == .minus)
                .negate_num
            else
                .negate_bool;
            const right = try self.parseUnary(ignore_new_line);
            errdefer right.destroy(self.allocator);

            return try ParsedExpr.Kind.Unary.create(
                self.allocator,
                kind,
                right,
                token.position,
            );
        }

        return try self.parsePrimary(ignore_new_line);
    }

    fn parsePrimary(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        if (self.match(.true_, ignore_new_line) or self.match(.false_, ignore_new_line)) {
            const prev = self.previous();
            return try ParsedExpr.Kind.Literal.create(
                self.allocator,
                .{ .bool = prev.lexeme.len == 4 },
                prev.position,
            );
        }

        if (self.match(.int, ignore_new_line)) {
            const prev = self.previous();
            const int = std.fmt.parseInt(i64, prev.lexeme, 10) catch |err| switch (err) {
                error.Overflow => blk: {
                    try self.parserError(prev, .int_literal_overflows);
                    break :blk 0;
                },
                else => unreachable,
            };
            return try ParsedExpr.Kind.Literal.create(
                self.allocator,
                .{ .int = int },
                prev.position,
            );
        }

        if (self.match(.float, ignore_new_line)) {
            const prev = self.previous();
            const float = std.fmt.parseFloat(f64, prev.lexeme) catch unreachable;
            return try ParsedExpr.Kind.Literal.create(self.allocator, .{ .float = float }, prev.position);
        }

        if (self.match(.string, ignore_new_line)) {
            const prev = self.previous();
            const string = prev.lexeme[1 .. prev.lexeme.len - 1];
            return try ParsedExpr.Kind.Literal.create(self.allocator, .{ .string = string }, prev.position);
        }

        if (self.match(.left_paren, ignore_new_line)) {
            const expr = try self.parseExpr(true);
            errdefer expr.destroy(self.allocator);

            _ = try self.consume(.right_paren, .expected_right_paren_after_expr);
            return expr;
        }

        if (self.match(.invalid, ignore_new_line)) {
            try self.parserError(self.previous(), .{ .invalid_token = self.previous().lexeme });
        } else {
            try self.parserError(self.peek(), .expected_expression);
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
        token_kind: Token.Kind,
        diagnostic_kind: DiagnosticEntry.Kind,
    ) Error!Token {
        if (self.check(token_kind)) {
            return self.advance();
        }

        try self.parserError(self.peek(), diagnostic_kind);
        return error.ParseFailure;
    }

    fn matchStmtTerminator(self: *Self) bool {
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
            if (self.previous().kind == .semicolon and !self.check(.semicolon)) {
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
        diagnostic_kind: DiagnosticEntry.Kind,
    ) Error!void {
        if (self.diagnostics) |diagnostics| {
            // in case of ever needing to alloc something in here, make sure to
            // use diagnostics.allocator instead of self.allocator. this is
            // necessary for lang-tests where a new allocator is created for
            // each test to detect memory leaks. that allocator then gets
            // deinited while diagnostics are owned by the tests.
            try diagnostics.add(.{
                .kind = diagnostic_kind,
                .token = token,
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
