const std = @import("std");
const tokenizer_mod = @import("tokenizer.zig");
const Ast = @import("ast.zig").Ast;

const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const Loc = tokenizer_mod.Loc;
const fmt = std.fmt;
const assert = std.debug.assert;

pub const Parser = struct {
    allocator: Allocator,
    tokenizer: *Tokenizer = undefined,
    ast: Ast = undefined,
    prev_token: Token = undefined,
    current_token: Token = undefined,
    diags: ?ArrayListUnmanaged(Diag) = null,

    pub const Error = error{ParseFailure} || Allocator.Error;

    pub const Diag = struct {
        tag: Tag,
        loc: Loc,

        const Tag = union(enum) {
            expected_end_token: ExpectedEndTokens,
            expected_expression,
            invalid_token,

            const ExpectedEndTokens = struct {
                Token.Tag,
                ?Token.Tag,
                ?Token.Tag,
            };
        };
    };

    pub const NlMode = enum { ignore_nl, obey_nl };

    pub fn init(
        allocator: Allocator,
        diags: ?ArrayListUnmanaged(Diag),
    ) Parser {
        return .{
            .allocator = allocator,
            .diags = diags,
        };
    }

    pub fn parse(
        self: *Parser,
        tokenizer: *Tokenizer,
    ) Error!Ast {
        self.tokenizer = tokenizer;
        self.ast = .{};
        self.current_token = self.nextNonCommentToken();

        self.ast.root = try self.parseBlock(.eof, .{ .index = 1, .len = 0 });

        return self.ast;
    }

    fn parseStmt(self: *Parser) Error!Ast.Index {
        // todo: errdefer.syncrhonize()

        return switch (self.peek().tag) {
            .print => self.parsePrintStmt(),
            else => self.parseExprStmt(),
        };
    }

    fn parsePrintStmt(self: *Parser) Error!Ast.Index {
        const print = self.advance();
        const expr = try self.parseExpr(.obey_nl);

        return try self.astAdd(
            .{ .print = expr },
            print.loc.extend(expr.toLoc(&self.ast)),
        );
    }

    fn parseExprStmt(self: *Parser) Error!Ast.Index {
        const expr = try self.parseExpr(.obey_nl);

        return try self.astAdd(
            .{ .expr_stmt = expr },
            expr.toLoc(&self.ast),
        );
    }

    fn parseExpr(self: *Parser, nl_mode: NlMode) Error!Ast.Index {
        return try self.parseTerm(nl_mode);
    }

    fn parseTerm(self: *Parser, nl_mode: NlMode) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseFactor(nl_mode);

        while (self.match(.{ .minus, .plus }, nl_mode)) |token| {
            self.skipNewLines();

            const rhs = try self.parseFactor(nl_mode);
            const binary: Ast.Key.Binary = .{ .lhs = expr, .rhs = rhs };

            expr = try self.astAdd(
                if (token.tag == .minus)
                    .{ .subtraction = binary }
                else
                    .{ .addition = binary },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseFactor(self: *Parser, nl_mode: NlMode) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseUnary(nl_mode);

        while (self.match(.{ .slash, .star }, nl_mode)) |token| {
            self.skipNewLines();

            const rhs = try self.parseUnary(nl_mode);
            const binary: Ast.Key.Binary = .{ .lhs = expr, .rhs = rhs };

            expr = try self.astAdd(
                if (token.tag == .slash)
                    .{ .division = binary }
                else
                    .{ .multiplication = binary },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseUnary(self: *Parser, nl_mode: NlMode) Error!Ast.Index {
        if (self.match(.{ .minus, .not }, nl_mode)) |token| {
            self.skipNewLines();

            const rhs = try self.parseUnary(nl_mode);
            const unary: Ast.Key.Unary = .{ .rhs = rhs };

            return try self.astAdd(
                if (token.tag == .minus)
                    .{ .negation_num = unary }
                else
                    .{ .negation_bool = unary },
                token.loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return try self.parsePrimary(nl_mode);
    }

    fn parsePrimary(self: *Parser, nl_mode: NlMode) Error!Ast.Index {
        if (nl_mode == .ignore_nl) {
            self.skipNewLines();
        }

        const token = self.advance();

        return switch (token.tag) {
            .int => try self.astAdd(.literal_float, token.loc),
            .float => try self.astAdd(.literal_float, token.loc),
            .true, .false => try self.astAdd(.literal_bool, token.loc),
            .string => try self.astAdd(.literal_string, token.loc),
            .do => try self.parseBlock(.end, token.loc),
            else => {
                if (token.tag == .invalid) {
                    try self.diagsAdd(.invalid_token, token.loc);
                } else {
                    try self.diagsAdd(.expected_expression, self.peek().loc);
                }
                return error.ParseFailure;
            },
        };
    }

    fn parseBlock(
        self: *Parser,
        end_tokens: anytype,
        loc: Loc,
    ) Error!Ast.Index {
        const index: u32 = @intCast(self.ast.getExtraLen());
        var len: u32 = 0;

        if (self.match(end_tokens, .ignore_nl)) |end_token| {
            return try self.astAdd(
                .{ .block = .{ .index = index, .len = len } },
                loc.extend(end_token.loc),
            );
        }

        const old_num_diags = self.diagsLen();
        var evals = false;

        while (true) {
            const stmt = self.parseStmt() catch |err| switch (err) {
                error.ParseFailure => blk: {
                    if (self.check(.eof)) {
                        break;
                    }

                    // todo: why create this here? check if i can improve logic
                    break :blk try self.astAdd(
                        .{ .expr_stmt = try self.astAdd(
                            .literal_unit,
                            loc,
                        ) },
                        loc,
                    );
                },
                else => return err,
            };

            try self.ast.addExtra(self.allocator, stmt.to());
            len += 1;

            const terminator = self.matchStmtTerminator();
            evals = terminator != .semicolon and
                stmt.toKey(&self.ast) == .expr_stmt;

            if (terminator == .none or self.check(end_tokens)) {
                break;
            }
        }

        const end_token = try self.consume(
            end_tokens,
            .{ .expected_end_token = tokensArgToDiagStruct(end_tokens) },
        );

        if (self.diagsLen() > old_num_diags) {
            return error.ParseFailure;
        }

        const block: Ast.Key.Block = .{ .index = index, .len = len };

        return try self.astAdd(
            if (evals)
                .{ .eval_block = block }
            else
                .{ .block = block },
            loc.extend(end_token.loc),
        );
    }

    fn advance(self: *Parser) Token {
        if (self.current_token.tag == .eof) {
            return self.current_token;
        }

        self.prev_token = self.current_token;
        self.current_token = self.nextNonCommentToken();

        return self.prev_token;
    }

    fn match(self: *Parser, tokens: anytype, nl_mode: NlMode) ?Token {
        if (nl_mode == .ignore_nl) {
            self.skipNewLines();
        }

        if (self.check(tokens)) {
            return self.advance();
        }

        return null;
    }

    fn consume(self: *Parser, tokens: anytype, diag: Diag.Tag) Error!Token {
        if (self.check(tokens)) {
            return self.advance();
        } else {
            std.debug.print("actual: {any}\n", .{self.peek()});
        }

        try self.diagsAdd(diag, self.peek().loc);
        return error.ParseFailure;
    }

    fn matchStmtTerminator(self: *Parser) enum { new_line, semicolon, none } {
        return switch (self.peek().tag) {
            .new_line => blk: {
                self.skipNewLines();
                break :blk .new_line;
            },
            .semicolon => blk: {
                _ = self.advance();
                self.skipNewLines();
                break :blk .semicolon;
            },
            else => .none,
        };
    }

    fn check(self: *Parser, arg: anytype) bool {
        const ArgType = @TypeOf(arg);
        const arg_type_info = @typeInfo(ArgType);
        const token_stuct = if (ArgType == @TypeOf(.enum_literal) or ArgType == Token.Tag)
            .{arg}
        else if (arg_type_info == .@"struct" and arg_type_info.@"struct".is_tuple)
            arg
        else
            @compileError("expected arg to be of type Token.Tag or a tuple of Token.Tag");

        inline for (@typeInfo(@TypeOf(token_stuct)).@"struct".fields) |field| {
            if (self.peek().tag == @field(token_stuct, field.name)) {
                return true;
            }
        }

        return false;
    }

    fn peek(self: *Parser) Token {
        return self.current_token;
    }

    fn prev(self: *Parser) Token {
        return self.prev_token;
    }

    fn nextNonCommentToken(self: *Parser) Token {
        while (true) {
            const token = self.tokenizer.next();

            if (token.tag != .comment) {
                return token;
            }
        }
    }

    fn skipNewLines(self: *Parser) void {
        while (self.check(.new_line)) {
            _ = self.advance();
        }
    }

    fn astAdd(self: *Parser, key: Ast.Key, loc: Loc) Allocator.Error!Ast.Index {
        return self.ast.add(self.allocator, key, loc);
    }

    fn diagsAdd(self: *Parser, diag: Diag.Tag, loc: Loc) Allocator.Error!void {
        if (self.diags) |*diags| {
            try diags.append(self.allocator, .{ .tag = diag, .loc = loc });
        }
    }

    fn diagsLen(self: *Parser) usize {
        return if (self.diags) |diags|
            diags.items.len
        else
            0;
    }

    fn tokensArgToDiagStruct(
        tokens: anytype,
    ) Diag.Tag.ExpectedEndTokens {
        const ArgType = @TypeOf(tokens);

        if (ArgType == @TypeOf(.enum_literal) or ArgType == Token.Tag) {
            return .{ tokens, null, null };
        }

        var end_tokens: Diag.Tag.ExpectedEndTokens = .{
            undefined,
            null,
            null,
        };

        inline for (@typeInfo(ArgType).@"struct".fields, 0..) |field, i| {
            if (i > 2) {
                @compileError("only three fields can be processed");
            }

            @field(end_tokens, field.name) = @field(tokens, field.name);
        }

        return end_tokens;
    }
};
