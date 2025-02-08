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
const ascii = std.ascii;
const SharedDiags = shared.Diags;
const Writer = shared.Writer;
const Token = tokenizer_mod.Token;
const Tokenizer = tokenizer_mod.Tokenizer;
const Position = tokenizer_mod.Position;
const ParsedExpr = parsed_ast_mod.ParsedExpr;
const ParsedStmt = parsed_ast_mod.ParsedStmt;
const ParsedType = parsed_ast_mod.ParsedType;

pub const Parser = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        ParseFailure,
    };

    pub const DiagEntry = struct {
        pub const Kind = union(enum) {
            expected_end_token: ArrayList(Token.Kind),
            invalid_token: []const u8,
            expected_expression,
            expected_left_paren_before_expr,
            expected_right_paren_after_expr,
            int_literal_overflows,
            expected_name_after_let,
            expected_equal_after_name,
            invalid_assignment_target,
            expected_type,
            variable_name_not_lower_case: []const u8,
            expected_token_after_condition: Token.Kind,
            expected_name_after_fn,
            expected_left_paren_before_args,
            expected_right_paren_after_args,
            expected_colon_after_arg,
            expected_colon_after_args,
        };

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
            switch (self.kind) {
                .invalid_token,
                .variable_name_not_lower_case,
                => |str| allocator.free(str),

                .expected_end_token,
                => |*token_list| token_list.clearAndFree(),

                .expected_expression,
                .expected_left_paren_before_expr,
                .expected_right_paren_after_expr,
                .int_literal_overflows,
                .expected_name_after_let,
                .expected_equal_after_name,
                .invalid_assignment_target,
                .expected_type,
                .expected_token_after_condition,
                .expected_name_after_fn,
                .expected_left_paren_before_args,
                .expected_right_paren_after_args,
                .expected_colon_after_arg,
                .expected_colon_after_args,
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
        return (try self.parseBlock(.eof, .{ .line = 1, .column = 1 }))[0];
    }

    fn parseStmt(self: *Self) Error!*ParsedStmt {
        errdefer self.synchronize();

        if (self.match(.@"fn", false)) |token| {
            return try self.parseFnStmt(token.position);
        }

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

    fn parseFnStmt(self: *Self, position: Position) Error!*ParsedStmt {
        const name_token = try self.consume(.identifier, .expected_name_after_fn);

        if (!ascii.isLower(name_token.lexeme[0])) {
            try self.addDiag(.{ .variable_name_not_lower_case = name_token.lexeme }, position);
        }

        var args = ArrayList(ParsedStmt.Kind.Fn.Arg).init(self.allocator);

        _ = try self.consume(.left_paren, .expected_left_paren_before_args);

        while (self.match(.identifier, true)) |identifier_token| {
            _ = try self.consume(.colon, .expected_colon_after_arg);
            const arg_type = try self.parseType();

            try args.append(.{
                .name = try self.allocator.dupe(u8, identifier_token.lexeme),
                .type = arg_type,
                .name_position = identifier_token.position,
                .type_position = arg_type.position,
            });

            if (self.match(.comma, true) == null or self.peek().kind == .right_paren) {
                break;
            }
        }

        _ = try self.consume(.right_paren, .expected_right_paren_after_args);
        _ = try self.consume(.colon, .expected_colon_after_args);

        const return_type = try self.parseType();
        const body, _ = try self.parseBlock(.end, position);

        return ParsedStmt.Kind.Fn.create(
            self.allocator,
            name_token.lexeme,
            args,
            return_type,
            body,
            position,
        );
    }

    fn parseLetStmt(self: *Self, position: Position) Error!*ParsedStmt {
        const is_mutable = self.match(.mut, false) != null;
        const name_token = try self.consume(.identifier, .expected_name_after_let);

        if (!ascii.isLower(name_token.lexeme[0])) {
            try self.addDiag(.{ .variable_name_not_lower_case = name_token.lexeme }, position);
        }

        const parsed_type = if (self.match(.colon, false) != null)
            try self.parseType()
        else
            null;

        const expr = if (self.match(.equal, false) != null) try self.parseExpr(false) else null;

        return try ParsedStmt.Kind.Let.create(
            self.allocator,
            is_mutable,
            name_token.lexeme,
            parsed_type,
            expr,
            position,
        );
    }

    fn parseType(self: *Self) Error!*ParsedType {
        if (self.match(.identifier, true)) |token| {
            return ParsedType.Kind.Identifier.create(
                self.allocator,
                token.lexeme,
                token.position,
            );
        }

        if (self.match(.invalid, true)) |token| {
            try self.addDiag(.{ .invalid_token = token.lexeme }, token.position);
        } else {
            try self.addDiag(.expected_type, self.peek().position);
        }

        return error.ParseFailure;
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

        while (self.match(.@"or", ignore_new_line) != null) {
            self.skipNewLines();

            const right = try self.parseAnd(ignore_new_line);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                .@"or",
                right,
                position,
            );
        }

        return expr;
    }

    fn parseAnd(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        const position = self.peek().position;
        var expr = try self.parseEquality(ignore_new_line);

        while (self.match(.@"and", ignore_new_line) != null) {
            self.skipNewLines();

            const right = try self.parseEquality(ignore_new_line);

            expr = try ParsedExpr.Kind.Binary.create(
                self.allocator,
                expr,
                .@"and",
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

        return try self.parseCall(ignore_new_line);
    }

    fn parseCall(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        var expr = try self.parsePrimary(ignore_new_line);

        while (self.match(.left_paren, ignore_new_line)) |left_paren| {
            var args = ArrayList(*ParsedExpr).init(self.allocator);

            while (self.match(.right_paren, true) == null) {
                try args.append(try self.parseExpr(true));

                if (self.match(.comma, true) != null or
                    self.peek().kind == .right_paren)
                {
                    continue;
                }

                try self.addDiag(
                    .expected_right_paren_after_args,
                    self.peek().position,
                );

                return error.ParseFailure;
            }

            expr = try ParsedExpr.Kind.Call.create(
                self.allocator,
                expr,
                args,
                left_paren.position,
            );
        }

        return expr;
    }

    fn parsePrimary(self: *Self, ignore_new_line: bool) Error!*ParsedExpr {
        if (self.match(.{ .true, .false }, ignore_new_line)) |token| {
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
            return (try self.parseBlock(.end, token.position))[0];
        }

        if (self.match(.@"if", ignore_new_line)) |token| {
            return try self.parseIf(token.position);
        }

        if (self.match(.@"for", ignore_new_line)) |token| {
            return try self.parseFor(token.position);
        }

        if (self.match(.@"break", ignore_new_line)) |token| {
            return try self.parseBreak(token.position);
        }

        if (self.match(.@"continue", ignore_new_line)) |token| {
            return try self.parseContinue(token.position);
        }

        if (self.match(.@"return", ignore_new_line)) |token| {
            return try self.parseReturn(token.position);
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
        arg: anytype,
        position: Position,
    ) Error!struct { *ParsedExpr, Token } {
        const stmts = ArrayList(*ParsedStmt).init(self.allocator);

        var block = try ParsedExpr.Kind.Block.create(
            self.allocator,
            stmts,
            false,
            position,
        );

        if (self.match(arg, true)) |end_token| {
            return .{
                block,
                end_token,
            };
        }

        const old_num_diags = if (self.diags) |diags| diags.getLen() else 0;

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
            block.kind.block.ends_with_semicolon = terminator == .semicolon;

            if (terminator == .none or self.check(arg)) {
                break;
            }
        }

        const end_token = try self.consume(
            arg,
            .{ .expected_end_token = try self.tokenArgToArrayList(arg) },
        );

        if (self.diags != null and self.diags.?.getLen() > old_num_diags) {
            return error.ParseFailure;
        }

        return .{ block, end_token };
    }

    fn parseIf(
        self: *Self,
        position: Position,
    ) Error!*ParsedExpr {
        const condition = try self.parseExpr(true);
        const then_token = try self.consume(.then, .{ .expected_token_after_condition = .then });
        const then_block, var end_token = try self.parseBlock(
            .{ .end, .elseif, .@"else" },
            then_token.position,
        );
        var elseif_blocks =
            ArrayList(ParsedExpr.Kind.If.ConditionalBlock).init(self.allocator);

        while (end_token.kind == .elseif) {
            const elseif_condition = try self.parseExpr(true);
            const elseif_then_token = try self.consume(.then, .{ .expected_token_after_condition = .then });
            const elseif_block, end_token = try self.parseBlock(
                .{ .end, .elseif, .@"else" },
                elseif_then_token.position,
            );

            try elseif_blocks.append(.{
                .condition = elseif_condition,
                .block = elseif_block,
            });
        }

        const else_block = if (end_token.kind == .@"else")
            (try self.parseBlock(.end, end_token.position))[0]
        else
            null;

        return try ParsedExpr.Kind.If.create(
            self.allocator,
            .{
                .condition = condition,
                .block = then_block,
            },
            elseif_blocks,
            else_block,
            position,
        );
    }

    fn parseFor(
        self: *Self,
        position: Position,
    ) Error!*ParsedExpr {
        const condition = if (self.match(.do, true) != null)
            null
        else blk: {
            const expr = try self.parseExpr(true);
            _ = try self.consume(.do, .{ .expected_token_after_condition = .do });
            break :blk expr;
        };

        const body_block = (try self.parseBlock(.end, position))[0];

        return try ParsedExpr.Kind.For.create(
            self.allocator,
            condition,
            body_block,
            position,
        );
    }

    fn parseBreak(self: *Self, position: Position) Error!*ParsedExpr {
        return try ParsedExpr.Kind.Break.create(
            self.allocator,
            position,
        );
    }

    fn parseContinue(self: *Self, position: Position) Error!*ParsedExpr {
        return try ParsedExpr.Kind.Continue.create(
            self.allocator,
            position,
        );
    }

    fn parseReturn(self: *Self, position: Position) Error!*ParsedExpr {
        var right: ?*ParsedExpr = null;

        if (self.matchStmtTerminator() == .none) {
            right = try self.parseExpr(false);
        }

        return try ParsedExpr.Kind.Return.create(
            self.allocator,
            right,
            position,
        );
    }

    fn match(self: *Self, arg: anytype, ignore_new_line: bool) ?Token {
        if (ignore_new_line) {
            self.skipNewLines();
        }

        if (self.check(arg)) {
            return self.advance();
        }

        return null;
    }

    fn consume(
        self: *Self,
        arg: anytype,
        diag_kind: DiagEntry.Kind,
    ) Error!Token {
        if (self.check(arg)) {
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

    fn check(self: *Self, arg: anytype) bool {
        const ArgType = @TypeOf(arg);
        const arg_type_info = @typeInfo(ArgType);
        const token_stuct = if (ArgType == @TypeOf(.enum_literal) or ArgType == Token.Kind)
            .{arg}
        else if (arg_type_info == .@"struct" and arg_type_info.@"struct".is_tuple)
            arg
        else
            @compileError("expected arg to be of type Token.Kind or a tuple of Token.Kind");

        inline for (@typeInfo(@TypeOf(token_stuct)).@"struct".fields) |field| {
            if (self.peek().kind == @field(token_stuct, field.name)) {
                return true;
            }
        }

        return false;
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

    fn tokenArgToArrayList(
        self: *Self,
        arg: anytype,
    ) error{OutOfMemory}!ArrayList(Token.Kind) {
        const ArgType = @TypeOf(arg);
        var array_list = ArrayList(Token.Kind).init(self.allocator);

        if (ArgType == @TypeOf(.enum_literal) or ArgType == Token.Kind) {
            try array_list.append(arg);
            return array_list;
        }

        inline for (@typeInfo(ArgType).@"struct".fields) |field| {
            try array_list.append(@field(arg, field.name));
        }

        return array_list;
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
                .kind = try shared.clone.createClone(
                    diags.allocator,
                    diag_kind,
                    .{DiagEntry.Kind},
                ),
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
