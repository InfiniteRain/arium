const std = @import("std");
const shared = @import("shared");
const tokenizer_mod = @import("tokenizer.zig");
const parsed_ast_mod = @import("parsed_ast.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const expectError = std.testing.expectError;
const allocPrint = std.fmt.allocPrint;
const SharedDiags = shared.Diags;
const Writer = shared.Writer;
const Token = tokenizer_mod.Token;
const Tokenizer = tokenizer_mod.Tokenizer;
const Position = tokenizer_mod.Position;
const ParsedExpr = parsed_ast_mod.ParsedExpr;
const ParsedStmt = parsed_ast_mod.ParsedStmt;

pub const Parser = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        ParseFailure,
    };

    pub const DiagEntry = struct {
        pub const Kind = union(enum) {
            expected_end_token: Token.Kind,
            invalid_token: []const u8,
            expected_expression,
            expected_left_paren_before_expr,
            expected_right_paren_after_expr,
            int_literal_overflows,
            expected_name,
            expected_equal_after_name,
            invalid_assignment_target,
        };

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
            switch (self.kind) {
                .invalid_token,
                => |msg| allocator.free(msg),

                .expected_end_token,
                .expected_expression,
                .expected_left_paren_before_expr,
                .expected_right_paren_after_expr,
                .int_literal_overflows,
                .expected_name,
                .expected_equal_after_name,
                .invalid_assignment_target,
                => {},
            }
        }

        kind: Kind,
        position: Position,
    };

    pub const Diags = SharedDiags(DiagEntry);

    allocator: Allocator,
    tokenizer: *Tokenizer = undefined,
    prev_token: Token = undefined,
    current_token: Token = undefined,
    diags: ?*Diags = null,

    pub fn init(arena_allocator: *ArenaAllocator) Self {
        return .{
            .allocator = arena_allocator.allocator(),
        };
    }

    pub fn parse(
        self: *Self,
        tokenizer: *Tokenizer,
        diags: ?*Diags,
    ) Error!*ParsedExpr {
        self.tokenizer = tokenizer;
        self.current_token = tokenizer.scanNonCommentToken();
        self.diags = diags;
        return try self.parseBlock(.eof, .{ .line = 1, .column = 1 });
    }

    fn parseStmt(self: *Self) Error!*ParsedStmt {
        errdefer self.synchronize();

        if (self.match(.let, false)) |token| {
            return try self.parseLetStmt(token.position);
        }

        if (self.match(.assert, false)) |token| {
            return try self.parseAssertStmt(token.position);
        }

        if (self.match(.print, false)) |token| {
            return try self.parsePrintStmt(token.position);
        }

        return try self.parseExprStmt(self.peek().position);
    }

    fn parseLetStmt(self: *Self, position: Position) Error!*ParsedStmt {
        const is_mutable = self.match(.mut, false) != null;
        const name_token = try self.consume(.identifier, .expected_name);
        _ = try self.consume(.equal, .expected_equal_after_name);
        const expr = try self.parseExpr(false);

        return try ParsedStmt.Kind.Let.create(
            self.allocator,
            is_mutable,
            name_token.lexeme,
            expr,
            position,
        );
    }

    fn parseAssertStmt(self: *Self, position: Position) Error!*ParsedStmt {
        _ = try self.consume(.left_paren, .expected_left_paren_before_expr);

        const expr = try self.parseExpr(false);

        _ = try self.consume(.right_paren, .expected_right_paren_after_expr);

        return try ParsedStmt.Kind.Assert.create(self.allocator, expr, position);
    }

    fn parsePrintStmt(self: *Self, position: Position) Error!*ParsedStmt {
        const expr = try self.parseExpr(false);
        return try ParsedStmt.Kind.Print.create(self.allocator, expr, position);
    }

    fn parseExprStmt(self: *Self, position: Position) Error!*ParsedStmt {
        const expr = try self.parseExpr(false);

        return try ParsedStmt.Kind.Expr.create(self.allocator, expr, position);
    }

    fn parseExpr(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        return try self.parseAssignment(ignore_new_line);
    }

    fn parseAssignment(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        const expr = try self.parseOr(ignore_new_line);

        while (self.match(.equal, ignore_new_line) != null) {
            const value_expr = try self.parseAssignment(ignore_new_line);

            switch (expr.kind) {
                .variable => |variable| return ParsedExpr.Kind.Assigment.create(
                    self.allocator,
                    variable.name,
                    value_expr,
                    position,
                ),
                else => try self.addDiag(.invalid_assignment_target, position),
            }
        }

        return expr;
    }

    fn parseOr(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        var expr = try self.parseAnd(ignore_new_line);

        while (self.match(.or_, ignore_new_line) != null) {
            self.skipNewLines();

            const right = try self.parseAnd(ignore_new_line);

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

        while (self.match(.and_, ignore_new_line) != null) {
            self.skipNewLines();

            const right = try self.parseEquality(ignore_new_line);

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

        while (self.match(.{ .bang_equal, .equal_equal }, ignore_new_line)) |token| {
            self.skipNewLines();

            const kind: BinaryKind = if (token.kind == .bang_equal)
                .not_equal
            else
                .equal;
            const right = try self.parseComparison(ignore_new_line);

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

        while (self.match(.{
            .greater,
            .greater_equal,
            .less,
            .less_equal,
        }, ignore_new_line)) |token| {
            self.skipNewLines();

            const kind: BinaryKind = switch (token.kind) {
                .greater => .greater,
                .greater_equal => .greater_equal,
                .less => .less,
                .less_equal => .less_equal,
                else => unreachable,
            };
            const right = try self.parseTerm(ignore_new_line);

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

        while (self.match(.{ .minus, .plus }, ignore_new_line)) |token| {
            self.skipNewLines();

            const kind: BinaryKind = if (token.kind == .minus)
                .subtract
            else
                .add;
            const right = try self.parseFactor(ignore_new_line);

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

        while (self.match(.{ .slash, .star }, ignore_new_line)) |token| {
            self.skipNewLines();

            const kind: BinaryKind = if (token.kind == .slash)
                .divide
            else
                .multiply;
            const right = try self.parseConcat(ignore_new_line);

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

        while (self.match(.plus_plus, ignore_new_line) != null) {
            self.skipNewLines();

            const kind = BinaryKind.concat;
            const right = try self.parseUnary(ignore_new_line);

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
        if (self.match(.{ .minus, .not }, ignore_new_line)) |token| {
            self.skipNewLines();

            const UnaryKind = ParsedExpr.Kind.Unary.Kind;
            const kind: UnaryKind = if (token.kind == .minus)
                .negate_num
            else
                .negate_bool;
            const right = try self.parseUnary(ignore_new_line);

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
        if (self.match(.{ .true_, .false_ }, ignore_new_line)) |token| {
            return try ParsedExpr.Kind.Literal.create(
                self.allocator,
                .{ .bool = token.lexeme.len == 4 },
                token.position,
            );
        }

        if (self.match(.int, ignore_new_line)) |token| {
            const int = std.fmt.parseInt(i64, token.lexeme, 10) catch |err| switch (err) {
                error.Overflow => blk: {
                    try self.addDiag(.int_literal_overflows, token.position);
                    break :blk 0;
                },
                else => unreachable,
            };
            return try ParsedExpr.Kind.Literal.create(
                self.allocator,
                .{ .int = int },
                token.position,
            );
        }

        if (self.match(.float, ignore_new_line)) |token| {
            const float = std.fmt.parseFloat(f64, token.lexeme) catch unreachable;
            return try ParsedExpr.Kind.Literal.create(
                self.allocator,
                .{ .float = float },
                token.position,
            );
        }

        if (self.match(.identifier, ignore_new_line)) |token| {
            return try ParsedExpr.Kind.Variable.create(
                self.allocator,
                token.lexeme,
                token.position,
            );
        }

        if (self.match(.string, ignore_new_line)) |token| {
            const string = try self.allocator.dupe(
                u8,
                token.lexeme[1 .. token.lexeme.len - 1],
            );

            return try ParsedExpr.Kind.Literal.create(
                self.allocator,
                .{ .string = string },
                token.position,
            );
        }

        if (self.match(.left_paren, ignore_new_line) != null) {
            const expr = try self.parseExpr(true);

            _ = try self.consume(.right_paren, .expected_right_paren_after_expr);
            return expr;
        }

        if (self.match(.do, ignore_new_line)) |token| {
            return try self.parseBlock(.end, token.position);
        }

        if (self.match(.invalid, ignore_new_line)) |token| {
            try self.addDiag(.{ .invalid_token = token.lexeme }, token.position);
        } else {
            try self.addDiag(.expected_expression, self.peek().position);
        }

        return error.ParseFailure;
    }

    fn parseBlock(
        self: *Self,
        end_token_kind: Token.Kind,
        position: Position,
    ) Error!*ParsedExpr {
        const stmts = ArrayList(*ParsedStmt).init(self.allocator);

        var block = try ParsedExpr.Kind.Block.create(
            self.allocator,
            stmts,
            position,
        );

        if (self.match(end_token_kind, true) != null) {
            try self.addUniExprStmtToBlock(&block.kind.block, position);
            return block;
        }

        const old_num_diags = if (self.diags) |diags| diags.getLen() else 0;
        var ends_with_semicolon = false;

        while (true) {
            const stmt = self.parseStmt() catch |err| switch (err) {
                error.ParseFailure => blk: {
                    // todo: perhaps add semicolon error message here
                    if (self.check(.eof)) {
                        break;
                    }

                    break :blk try ParsedStmt.Kind.Expr.create(
                        self.allocator,
                        try ParsedExpr.Kind.Literal.create(
                            self.allocator,
                            .unit, // literal kind doesn't matter here
                            position,
                        ),
                        position,
                    );
                },
                else => return err,
            };

            try block.kind.block.stmts.append(stmt);

            const terminator = self.matchStmtTerminator();
            ends_with_semicolon = terminator == .semicolon;

            if (terminator == .none or self.check(end_token_kind)) {
                break;
            }
        }

        _ = try self.consume(
            end_token_kind,
            .{ .expected_end_token = end_token_kind },
        );

        if (self.diags != null and self.diags.?.getLen() > old_num_diags) {
            return error.ParseFailure;
        }

        if (ends_with_semicolon or block.kind.block.stmts.getLast().kind != .expr) {
            try self.addUniExprStmtToBlock(&block.kind.block, position);
        }

        return block;
    }

    fn addUniExprStmtToBlock(
        self: *Self,
        block: *ParsedExpr.Kind.Block,
        position: Position,
    ) Error!void {
        const expr = try ParsedExpr.Kind.Literal.create(
            self.allocator,
            .unit,
            position,
        );

        const exprStmt = try ParsedStmt.Kind.Expr.create(
            self.allocator,
            expr,
            position,
        );

        try block.stmts.append(exprStmt);
    }

    fn match(self: *Self, arg: anytype, ignore_new_line: bool) ?Token {
        if (ignore_new_line) {
            self.skipNewLines();
        }

        const ArgType = @TypeOf(arg);
        const arg_type_info = @typeInfo(ArgType);
        const token_stuct = if (ArgType == @TypeOf(.enum_literal) or ArgType == Token.Kind)
            .{arg}
        else if (arg_type_info == .Struct and arg_type_info.Struct.is_tuple)
            arg
        else {
            @compileError("expected arg to be of type Token.Kind or a tuple of Token.Kind");
        };

        inline for (@typeInfo(@TypeOf(token_stuct)).Struct.fields) |field| {
            if (self.check(@field(token_stuct, field.name))) {
                return self.advance();
            }
        }

        return null;
    }

    fn consume(
        self: *Self,
        token_kind: Token.Kind,
        diag_kind: DiagEntry.Kind,
    ) Error!Token {
        if (self.check(token_kind)) {
            return self.advance();
        }

        try self.addDiag(diag_kind, self.peek().position);
        return error.ParseFailure;
    }

    fn matchStmtTerminator(self: *Self) enum { new_line, semicolon, none } {
        if (self.check(.new_line)) {
            self.skipNewLines();
            return .new_line;
        }

        if (self.check(.semicolon)) {
            _ = self.advance();
            self.skipNewLines();
            return .semicolon;
        }

        return .none;
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

    fn prev(self: *Self) Token {
        return self.prev_token;
    }

    fn advance(self: *Self) Token {
        if (self.current_token.kind == .eof) {
            return self.current_token;
        }

        self.prev_token = self.current_token;
        self.current_token = self.tokenizer.scanNonCommentToken();

        return self.prev_token;
    }

    fn skipNewLines(self: *Self) void {
        while (self.check(.new_line)) {
            _ = self.advance();
        }
    }

    fn synchronize(self: *Self) void {
        while (self.peek().kind != .eof) {
            if (self.prev().kind == .semicolon) {
                self.skipNewLines();
                return;
            }

            switch (self.peek().kind) {
                .print,
                .assert,
                .let,
                => return,

                else => _ = self.advance(),
            }
        }
    }

    fn addDiag(
        self: *Self,
        diag_kind: DiagEntry.Kind,
        position: Position,
    ) Error!void {
        if (self.diags) |diags| {
            // in case of ever needing to alloc something in here, make sure to
            // use diags.allocator instead of self.allocator. this is
            // necessary for lang-tests where a new allocator is created for
            // each test to detect memory leaks. that allocator then gets
            // deinited while diags are owned by the tests.
            try diags.add(.{
                .kind = switch (diag_kind) {
                    .invalid_token,
                    => |msg| .{
                        .invalid_token = try diags.allocator.dupe(u8, msg),
                    },

                    .expected_end_token,
                    .expected_expression,
                    .expected_left_paren_before_expr,
                    .expected_right_paren_after_expr,
                    .int_literal_overflows,
                    .expected_name,
                    .expected_equal_after_name,
                    .invalid_assignment_target,
                    => diag_kind,
                },
                .position = position,
            });
        }
    }
};

test "should free all memory on successful parse" {
    // GIVEN
    const allocator = std.testing.allocator;

    var arena_allocator = ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    const source = "print (2 + 2) * -2";
    var tokenizer = Tokenizer.init(source);

    // WHEN - THEN
    var parser = Parser.init(&arena_allocator);
    _ = try parser.parse(&tokenizer, null);
}

test "should free all memory on unsuccessful parse" {
    // GIVEN
    const allocator = std.testing.allocator;

    var arena_allocator = ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    const source = "print (2 + 2) print -2 + 2)";
    var tokenizer = Tokenizer.init(source);

    // WHEN - THEN
    var parser = Parser.init(&arena_allocator);

    const result = parser.parse(&tokenizer, null);
    try expectError(Parser.Error.ParseFailure, result);
}
