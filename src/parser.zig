const std = @import("std");
const shared = @import("shared");
const tokenizer_mod = @import("tokenizer.zig");
const Ast = @import("ast.zig").Ast;

const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const Loc = tokenizer_mod.Loc;
const meta = std.meta;
const fmt = std.fmt;
const assert = std.debug.assert;

pub const Parser = struct {
    allocator: Allocator,
    tokenizer: *Tokenizer = undefined,
    ast: Ast = undefined,
    prev_token: Token = undefined,
    current_token: Token = undefined,
    scratch: ArrayListUnmanaged(Ast.Index) = .{},
    diags: ?*ArrayListUnmanaged(Diag) = null,

    pub const Error = error{ParseFailure} || Allocator.Error;

    pub const Diag = struct {
        tag: Tag,
        loc: Loc,

        const Tag = union(enum) {
            expected_end_token: EndTokens,
            expected_expression,
            invalid_assignment_target,
            invalid_token,

            const end_tokens_size = 3;
            const EndTokens = [end_tokens_size]?Token.Tag;

            fn endTokensFrom(tokens: anytype) EndTokens {
                const tuple = shared.meta.normalizeArgs(tokens);
                var end_tokens: EndTokens = .{null} ** end_tokens_size;

                inline for (meta.fields(@TypeOf(tuple)), 0..) |field, index| {
                    end_tokens[index] = @field(tuple, field.name);
                }

                return end_tokens;
            }
        };
    };

    pub const NlMode = enum { ignore_nl, terminate_on_nl };

    pub fn init(
        allocator: Allocator,
        diags: ?*ArrayListUnmanaged(Diag),
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
        const expr = try self.parseExpr(.terminate_on_nl);

        return try self.astAdd(
            .{ .print = expr },
            print.loc.extend(expr.toLoc(&self.ast)),
        );
    }

    fn parseExprStmt(self: *Parser) Error!Ast.Index {
        const expr = try self.parseExpr(.terminate_on_nl);

        return try self.astAdd(
            .{ .expr_stmt = expr },
            expr.toLoc(&self.ast),
        );
    }

    fn parseExpr(self: *Parser, nl_mode: NlMode) Error!Ast.Index {
        return try self.parseAssignment(nl_mode);
    }

    fn parseAssignment(self: *Parser, nl_mode: NlMode) Error!Ast.Index {
        const loc = self.peek().loc;
        const expr = try self.parseTerm(nl_mode);

        while (self.match(.equal, nl_mode)) |token| {
            const value_expr = try self.parseAssignment(nl_mode);

            if (expr.toTag(&self.ast) == .variable) {
                return self.astAdd(
                    .{ .assignment = .{
                        .lhs = expr,
                        .rhs = value_expr,
                    } },
                    loc.extend(value_expr.toLoc(&self.ast)),
                );
            }

            try self.diagsAdd(.invalid_assignment_target, token.loc);
        }

        return expr;
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

            return try self.astAdd(
                if (token.tag == .minus)
                    .{ .negation_num = rhs }
                else
                    .{ .negation_bool = rhs },
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
            .identifier => try self.astAdd(.variable, token.loc),
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
        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);

        if (self.match(end_tokens, .ignore_nl)) |end_token| {
            return try self.astAdd(
                .{ .block = &[_]Ast.Index{} },
                loc.extend(end_token.loc),
            );
        }

        var has_error = false;
        var ends_with_semicolon = false;

        while (true) {
            const stmt = self.parseStmt() catch |err| switch (err) {
                error.ParseFailure => blk: {
                    has_error = true;

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

            try self.scratch.append(self.allocator, stmt);

            const terminator = self.matchStmtTerminator();
            ends_with_semicolon = terminator == .semicolon;

            if (terminator == .none or self.check(end_tokens)) {
                break;
            }
        }

        const end_token = try self.consume(
            end_tokens,
            .{ .expected_end_token = Diag.Tag.endTokensFrom(end_tokens) },
        );

        if (has_error) {
            return error.ParseFailure;
        }

        const indexes = self.scratch.items[scratch_top..];

        return try self.astAdd(
            if (ends_with_semicolon)
                .{ .block_semicolon = indexes }
            else
                .{ .block = indexes },
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

    fn check(self: *Parser, args: anytype) bool {
        const tuple = shared.meta.normalizeArgs(args);

        inline for (meta.fields(@TypeOf(tuple))) |field| {
            if (self.peek().tag == @field(tuple, field.name)) {
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
        if (self.diags) |diags| {
            try diags.append(self.allocator, .{ .tag = diag, .loc = loc });
        }
    }
};
