const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const meta = std.meta;
const fmt = std.fmt;
const assert = std.debug.assert;

const shared = @import("shared");

const ast_mod = @import("ast.zig");
const Ast = ast_mod.Ast;
const fixed_array_mod = @import("fixed_array.zig");
const FixedArray = fixed_array_mod.FixedArray;
const tokenizer_mod = @import("tokenizer.zig");
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const Loc = tokenizer_mod.Loc;

pub const Parser = struct {
    allocator: Allocator,
    tokenizer: *Tokenizer,
    ast: Ast,
    prev_token: Token,
    current_token: Token,
    diags: *Diags,
    scratch: *Scratch,

    pub const Error = error{ParseFailure} || Allocator.Error;

    pub const Diags = struct {
        entries: ArrayListUnmanaged(Entry),

        pub const Entry = struct {
            tag: Tag,
            loc: Loc,

            pub const Tag = union(enum) {
                expected_end_token: EndTokens,
                expected_expression,
                expected_token_after_condition: Token.Tag,
                expected_identifier,
                expected_right_paren_after_args,
                expected_left_paren_before_params,
                expected_right_paren_after_params,
                expected_colon_after_params,
                expected_colon_after_param,
                expected_right_paren_after_expr,
                invalid_assignment_target,
                invalid_token,

                pub const EndTokens = FixedArray(Token.Tag, 3);
            };
        };

        pub const empty: Diags = .{
            .entries = .empty,
        };

        pub fn deinit(self: *Diags, allocator: Allocator) void {
            self.entries.deinit(allocator);
        }
    };

    pub const Scratch = struct {
        nodes: ArrayListUnmanaged(Ast.Index),

        pub const empty: Scratch = .{
            .nodes = .empty,
        };

        pub fn deinit(self: *Scratch, allocator: Allocator) void {
            self.nodes.deinit(allocator);
        }
    };

    pub const TerminationMode = enum {
        greedy,
        newline_terminated,
    };

    pub fn parse(
        allocator: Allocator,
        tokenizer: *Tokenizer,
        diags: *Diags,
        scratch: *Scratch,
    ) Error!Ast {
        var parser = Parser{
            .allocator = allocator,
            .tokenizer = tokenizer,
            .ast = .init(tokenizer.source),
            .prev_token = undefined,
            .current_token = undefined,
            .diags = diags,
            .scratch = scratch,
        };

        parser.current_token = parser.nextNonCommentToken();

        try parser.ast.nodes.append(allocator, undefined);
        try parser.ast.locs.append(allocator, undefined);

        const block, const loc = try parser.parseBlockExprKey(
            .eof,
            .{ .index = 1, .len = 0 },
        );

        try parser.setNode(0, block, loc);

        return parser.ast;
    }

    fn parseType(self: *Parser) Error!Ast.Index {
        const token = self.advance();

        return switch (token.tag) {
            .identifier => try self.addNode(.identifier, token.loc),
            else => {
                if (token.tag == .invalid) {
                    try self.addDiag(.invalid_token, token.loc);
                } else {
                    try self.addDiag(.expected_expression, self.peek().loc);
                }
                return error.ParseFailure;
            },
        };
    }

    fn parseStmt(self: *Parser) Error!Ast.Index {
        return switch (self.peek().tag) {
            .assert => self.parseAssertStmt(),
            .print => self.parsePrintStmt(),
            .let => self.parseLetStmt(),
            .@"fn" => self.parseFnStmt(),
            else => self.parseExprStmt(),
        };
    }

    fn parseAssertStmt(self: *Parser) Error!Ast.Index {
        const assert_token = self.advance();
        const expr = try self.parseExpr(.newline_terminated);

        return try self.addNode(
            .{ .assert = expr },
            assert_token.loc.extend(expr.toLoc(&self.ast)),
        );
    }

    fn parsePrintStmt(self: *Parser) Error!Ast.Index {
        const print_token = self.advance();
        const expr = try self.parseExpr(.newline_terminated);

        return try self.addNode(
            .{ .print = expr },
            print_token.loc.extend(expr.toLoc(&self.ast)),
        );
    }

    fn parseLetStmt(self: *Parser) Error!Ast.Index {
        const let_token = self.advance();
        const is_mutable = self.match(.mut, .newline_terminated) != null;
        const name_identifier = try self.parseIdentifier();
        const type_opt = if (self.match(.colon, .newline_terminated) != null)
            try self.parseType()
        else
            null;
        const expr_opt = if (self.match(.equal, .newline_terminated) != null) blk: {
            self.skipNewLines();
            break :blk try self.parseExpr(.newline_terminated);
        } else null;
        const let: Ast.Key.Let = .{
            .identifier = name_identifier,
            .type = type_opt,
            .expr = expr_opt,
        };

        return try self.addNode(
            if (is_mutable) .{ .let_mut = let } else .{ .let = let },
            let_token.loc.extend(
                (expr_opt orelse (type_opt orelse name_identifier)).toLoc(
                    &self.ast,
                ),
            ),
        );
    }

    fn parseFnStmt(self: *Parser) Error!Ast.Index {
        const scratch_top = self.scratch.nodes.items.len;
        defer self.scratch.nodes.shrinkRetainingCapacity(scratch_top);

        const fn_token = self.advance();
        const identifier = try self.parseIdentifier();

        _ = try self.consume(.left_paren, .expected_left_paren_before_params);

        while (self.match(.right_paren, .greedy) == null) {
            const param_identifier = try self.parseIdentifier();
            _ = try self.consume(.colon, .expected_colon_after_param);
            const arg_type = try self.parseType();

            try self.scratch.nodes.appendSlice(self.allocator, &[_]Ast.Index{
                param_identifier,
                arg_type,
            });

            if (self.match(.comma, .greedy) == null and
                self.peek().tag != .right_paren)
            {
                try self.addDiag(
                    .expected_right_paren_after_params,
                    self.peek().loc,
                );
                return error.ParseFailure;
            }
        }

        _ = try self.consume(.colon, .expected_colon_after_params);

        const return_type = try self.parseType();
        const body = try self.parseBlockExpr(.end, fn_token.loc);

        return try self.addNode(
            .{ .@"fn" = .{
                .identifier = identifier,
                .params = @ptrCast(self.scratch.nodes.items[scratch_top..]),
                .return_type = return_type,
                .body = body,
            } },
            fn_token.loc.extend(body.toLoc(&self.ast)),
        );
    }

    fn parseExprStmt(self: *Parser) Error!Ast.Index {
        const expr = try self.parseExpr(.newline_terminated);

        return try self.addNode(
            .{ .expr_stmt = expr },
            expr.toLoc(&self.ast),
        );
    }

    fn parseExpr(self: *Parser, termination: TerminationMode) Error!Ast.Index {
        return try self.parseAssignmentExpr(termination);
    }

    fn parseAssignmentExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        const expr = try self.parseOrExpr(termination);

        while (self.match(.equal, termination)) |token| {
            self.skipNewLines();

            const rhs = try self.parseAssignmentExpr(termination);

            if (expr.toTag(&self.ast) == .identifier) {
                return try self.addNode(
                    .{ .assignment = .{
                        .lhs = expr,
                        .rhs = rhs,
                    } },
                    loc.extend(rhs.toLoc(&self.ast)),
                );
            }

            try self.addDiag(.invalid_assignment_target, token.loc);
        }

        return expr;
    }

    fn parseOrExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseAndExpr(termination);

        while (self.match(.@"or", termination) != null) {
            self.skipNewLines();

            const rhs = try self.parseAndExpr(termination);

            expr = try self.addNode(
                .{ .@"or" = .{
                    .lhs = expr,
                    .rhs = rhs,
                } },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseAndExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseEqualityExpr(termination);

        while (self.match(.@"and", termination) != null) {
            self.skipNewLines();

            const rhs = try self.parseEqualityExpr(termination);

            expr = try self.addNode(
                .{ .@"and" = .{
                    .lhs = expr,
                    .rhs = rhs,
                } },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseEqualityExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseComparisonExpr(termination);

        while (self.match(
            .{ .equal_equal, .bang_equal },
            termination,
        )) |token| {
            self.skipNewLines();

            const rhs = try self.parseComparisonExpr(termination);
            const binary: Ast.Key.Binary = .{ .lhs = expr, .rhs = rhs };

            expr = try self.addNode(
                if (token.tag == .equal_equal)
                    .{ .equal = binary }
                else
                    .{ .not_equal = binary },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseComparisonExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseTermExpr(termination);

        while (self.match(
            .{ .greater, .greater_equal, .less, .less_equal },
            termination,
        )) |token| {
            self.skipNewLines();

            const rhs = try self.parseTermExpr(termination);
            const binary: Ast.Key.Binary = .{ .lhs = expr, .rhs = rhs };

            expr = try self.addNode(
                switch (token.tag) {
                    .greater => .{ .greater_than = binary },
                    .greater_equal => .{ .greater_equal = binary },
                    .less => .{ .less_than = binary },
                    .less_equal => .{ .less_equal = binary },
                    else => unreachable,
                },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseTermExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseFactorExpr(termination);

        while (self.match(.{ .minus, .plus }, termination)) |token| {
            self.skipNewLines();

            const rhs = try self.parseFactorExpr(termination);
            const binary: Ast.Key.Binary = .{ .lhs = expr, .rhs = rhs };

            expr = try self.addNode(
                if (token.tag == .minus)
                    .{ .sub = binary }
                else
                    .{ .add = binary },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseFactorExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseConcatExpr(termination);

        while (self.match(.{ .slash, .star }, termination)) |token| {
            self.skipNewLines();

            const rhs = try self.parseConcatExpr(termination);
            const binary: Ast.Key.Binary = .{ .lhs = expr, .rhs = rhs };

            expr = try self.addNode(
                if (token.tag == .slash)
                    .{ .div = binary }
                else
                    .{ .mul = binary },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseConcatExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parseUnaryExpr(termination);

        while (self.match(.plus_plus, termination) != null) {
            self.skipNewLines();

            const rhs = try self.parseUnaryExpr(termination);

            expr = try self.addNode(
                .{ .concat = .{ .lhs = expr, .rhs = rhs } },
                loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return expr;
    }

    fn parseUnaryExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        if (self.match(.{ .minus, .not }, termination)) |token| {
            self.skipNewLines();

            const rhs = try self.parseUnaryExpr(termination);

            return try self.addNode(
                if (token.tag == .minus)
                    .{ .neg_num = rhs }
                else
                    .{ .neg_bool = rhs },
                token.loc.extend(rhs.toLoc(&self.ast)),
            );
        }

        return try self.parseCallExpr(termination);
    }

    fn parseCallExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        const loc = self.peek().loc;
        var expr = try self.parsePrimaryExpr(termination);

        while (self.match(.left_paren, termination) != null) {
            const scratch_top = self.scratch.nodes.items.len;
            defer self.scratch.nodes.shrinkRetainingCapacity(scratch_top);

            while (self.match(.right_paren, .greedy) == null) {
                try self.scratch.nodes.append(
                    self.allocator,
                    try self.parseExpr(.greedy),
                );

                if (self.match(.comma, .greedy) == null and
                    self.peek().tag != .right_paren)
                {
                    try self.addDiag(
                        .expected_right_paren_after_args,
                        self.peek().loc,
                    );
                    return error.ParseFailure;
                }
            }

            const args = self.scratch.nodes.items[scratch_top..];
            const call: Ast.Key = if (args.len == 0)
                .{ .call_simple = .{ .callee = expr, .arg = null } }
            else if (args.len == 1)
                .{ .call_simple = .{ .callee = expr, .arg = args[0] } }
            else
                .{ .call = .{ .callee = expr, .args = args } };

            expr = try self.addNode(call, loc.extend(self.prev().loc));
        }

        return expr;
    }

    fn parsePrimaryExpr(
        self: *Parser,
        termination: TerminationMode,
    ) Error!Ast.Index {
        if (termination == .greedy) {
            self.skipNewLines();
        }

        const token = self.advance();

        return switch (token.tag) {
            .int => try self.addNode(.literal_int, token.loc),
            .float => try self.addNode(.literal_float, token.loc),
            .true, .false => try self.addNode(.literal_bool, token.loc),
            .string => try self.addNode(.literal_string, token.loc),
            .identifier => try self.addNode(.identifier, token.loc),
            .left_paren => try self.parseGroupExpr(),
            .do => try self.parseBlockExpr(.end, token.loc),
            .@"if" => try self.parseIfExpr(),
            .@"for" => try self.parseForExpr(),
            .@"break" => try self.parseBreakExpr(),
            .@"continue" => try self.parseContinueExpr(),
            .@"return" => try self.parseReturnExpr(),
            else => {
                if (token.tag == .invalid) {
                    try self.addDiag(.invalid_token, token.loc);
                } else {
                    try self.addDiag(.expected_expression, self.peek().loc);
                }
                return error.ParseFailure;
            },
        };
    }

    fn parseIdentifier(self: *Parser) Error!Ast.Index {
        const token = try self.consume(.identifier, .expected_identifier);
        return try self.addNode(.identifier, token.loc);
    }

    fn parseGroupExpr(self: *Parser) Error!Ast.Index {
        const expr = try self.parseExpr(.greedy);
        _ = try self.consume(.right_paren, .expected_right_paren_after_expr);
        return expr;
    }

    fn parseBlockExpr(
        self: *Parser,
        end_tokens: anytype,
        loc: Loc,
    ) Error!Ast.Index {
        const key, const block_loc = try self.parseBlockExprKey(
            end_tokens,
            loc,
        );
        return try self.addNode(key, block_loc);
    }

    fn parseBlockExprKey(
        self: *Parser,
        end_tokens: anytype,
        loc: Loc,
    ) Error!struct { Ast.Key, Loc } {
        const scratch_top = self.scratch.nodes.items.len;
        defer self.scratch.nodes.shrinkRetainingCapacity(scratch_top);

        if (self.match(end_tokens, .greedy)) |end_token| {
            return .{
                .{ .block = &[_]Ast.Index{} },
                loc.extend(end_token.loc),
            };
        }

        const old_errors_num = self.diags.entries.items.len;
        var ends_with_semicolon = false;

        while (self.peek().tag != .eof) {
            const stmt = self.parseStmt() catch |err| switch (err) {
                error.ParseFailure => {
                    self.synchronize();
                    continue;
                },
                else => return err,
            };

            try self.scratch.nodes.append(self.allocator, stmt);

            const terminator = self.matchStmtTerminator();
            ends_with_semicolon = terminator == .semicolon;

            if (terminator == .none or self.check(end_tokens)) {
                break;
            }
        }

        const end_token = try self.consume(
            end_tokens,
            .{ .expected_end_token = .from(end_tokens) },
        );

        if (self.diags.entries.items.len > old_errors_num) {
            return error.ParseFailure;
        }

        const indexes = self.scratch.nodes.items[scratch_top..];

        return .{
            if (ends_with_semicolon)
                .{ .block_semicolon = indexes }
            else
                .{ .block = indexes },
            loc.extend(end_token.loc),
        };
    }

    fn parseIfExpr(self: *Parser) Error!Ast.Index {
        const scratch_top = self.scratch.nodes.items.len;
        defer self.scratch.nodes.shrinkRetainingCapacity(scratch_top);

        const loc = self.prev().loc;

        fsm: switch (enum {
            @"if",
            if_elseif,
        }.@"if") {
            .@"if" => {
                const condition = try self.parseExpr(.greedy);
                const then_token = try self.consume(.then, .{
                    .expected_token_after_condition = .then,
                });
                const body = try self.parseBlockExpr(
                    .{ .end, .@"else", .elseif },
                    then_token.loc,
                );
                const conditional: Ast.Key.Conditional = .{
                    .condition = condition,
                    .body = body,
                };

                switch (self.prev().tag) {
                    .end => {
                        return try self.addNode(
                            .{ .@"if" = conditional },
                            loc.extend(body.toLoc(&self.ast)),
                        );
                    },
                    .@"else" => {
                        const else_block = try self.parseBlockExpr(
                            .end,
                            self.prev().loc,
                        );

                        return try self.addNode(
                            .{ .if_else = .{
                                .conditional = conditional,
                                .else_block = else_block,
                            } },
                            loc.extend(else_block.toLoc(&self.ast)),
                        );
                    },
                    .elseif => {
                        try self.scratch.nodes.appendSlice(
                            self.allocator,
                            &[_]Ast.Index{ condition, body },
                        );

                        continue :fsm .if_elseif;
                    },
                    else => unreachable,
                }
            },
            .if_elseif => {
                const condition = try self.parseExpr(.greedy);
                const then_token = try self.consume(.then, .{
                    .expected_token_after_condition = .then,
                });
                const body = try self.parseBlockExpr(
                    .{ .end, .elseif, .@"else" },
                    then_token.loc,
                );

                try self.scratch.nodes.appendSlice(
                    self.allocator,
                    &[_]Ast.Index{ condition, body },
                );

                switch (self.prev().tag) {
                    .end => {
                        return try self.addNode(
                            .{ .if_elseif = .{
                                .conditionals = @ptrCast(
                                    self.scratch.nodes.items[scratch_top..],
                                ),
                            } },
                            loc.extend(
                                self.scratch.nodes.getLast().toLoc(&self.ast),
                            ),
                        );
                    },
                    .elseif => {
                        continue :fsm .if_elseif;
                    },
                    .@"else" => {
                        const else_block = try self.parseBlockExpr(
                            .end,
                            self.prev().loc,
                        );

                        return try self.addNode(
                            .{ .if_elseif_else = .{
                                .conditionals = @ptrCast(
                                    self.scratch.nodes.items[scratch_top..],
                                ),
                                .else_block = else_block,
                            } },
                            loc.extend(else_block.toLoc(&self.ast)),
                        );
                    },
                    else => unreachable,
                }
            },
        }
    }

    fn parseForExpr(self: *Parser) Error!Ast.Index {
        const loc = self.prev().loc;
        const condition_opt = if (self.peek().tag != .do)
            try self.parseExpr(.greedy)
        else
            null;
        const do_token = try self.consume(
            .do,
            .{ .expected_token_after_condition = .do },
        );
        const body_block = try self.parseBlockExpr(.end, do_token.loc);

        return try self.addNode(
            if (condition_opt) |condition|
                .{ .for_conditional = .{
                    .condition = condition,
                    .body = body_block,
                } }
            else
                .{ .@"for" = body_block },
            loc.extend(body_block.toLoc(&self.ast)),
        );
    }

    fn parseBreakExpr(self: *Parser) Error!Ast.Index {
        return try self.addNode(
            .@"break",
            self.prev().loc,
        );
    }

    fn parseContinueExpr(self: *Parser) Error!Ast.Index {
        return try self.addNode(
            .@"continue",
            self.prev().loc,
        );
    }

    fn parseReturnExpr(self: *Parser) Error!Ast.Index {
        const loc = self.prev().loc;

        if (self.matchStmtTerminator() == .none) {
            const expr = try self.parseExpr(.newline_terminated);
            return try self.addNode(
                .{ .return_value = expr },
                loc.extend(expr.toLoc(&self.ast)),
            );
        }

        return try self.addNode(.@"return", loc);
    }

    fn advance(self: *Parser) Token {
        if (self.current_token.tag == .eof) {
            return self.current_token;
        }

        self.prev_token = self.current_token;
        self.current_token = self.nextNonCommentToken();

        return self.prev_token;
    }

    fn match(
        self: *Parser,
        tokens: anytype,
        termination: TerminationMode,
    ) ?Token {
        if (termination == .greedy) {
            self.skipNewLines();
        }

        if (self.check(tokens)) {
            return self.advance();
        }

        return null;
    }

    fn consume(
        self: *Parser,
        tokens: anytype,
        diag: Diags.Entry.Tag,
    ) Error!Token {
        if (self.check(tokens)) {
            return self.advance();
        }

        try self.addDiag(diag, self.peek().loc);
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

    fn synchronize(self: *Parser) void {
        while (self.peek().tag != .eof) {
            if (self.prev().tag == .semicolon) {
                self.skipNewLines();
                return;
            }

            switch (self.peek().tag) {
                .print,
                .assert,
                .let,
                .@"fn",
                => return,

                else => _ = self.advance(),
            }
        }
    }

    fn addNode(
        self: *Parser,
        key: Ast.Key,
        loc: Loc,
    ) Allocator.Error!Ast.Index {
        try self.ast.locs.append(self.allocator, loc);
        try self.ast.nodes.append(self.allocator, try self.prepareNode(key));

        assert(self.ast.nodes.len == self.ast.locs.items.len);

        return Ast.Index.from(self.ast.nodes.len - 1);
    }

    fn setNode(
        self: *Parser,
        index: u32,
        key: Ast.Key,
        loc: Loc,
    ) Allocator.Error!void {
        self.ast.locs.items[index] = loc;
        self.ast.nodes.set(index, try self.prepareNode(key));
    }

    fn prepareNode(self: *Parser, key: Ast.Key) Allocator.Error!Ast.Node {
        return switch (key) {
            .literal_unit => .{ .tag = .literal_unit },
            .literal_int => .{ .tag = .literal_int },
            .literal_float => .{ .tag = .literal_float },
            .literal_bool => .{ .tag = .literal_bool },
            .literal_string => .{ .tag = .literal_string },
            .add => |binary| prepareBinary(.add, binary),
            .sub => |binary| prepareBinary(.sub, binary),
            .mul => |binary| prepareBinary(.mul, binary),
            .div => |binary| prepareBinary(.div, binary),
            .concat => |binary| prepareBinary(.concat, binary),
            .@"or" => |binary| prepareBinary(.@"or", binary),
            .@"and" => |binary| prepareBinary(.@"and", binary),
            .equal => |binary| prepareBinary(.equal, binary),
            .not_equal => |binary| prepareBinary(.not_equal, binary),
            .greater_than => |binary| prepareBinary(.greater_than, binary),
            .greater_equal => |binary| prepareBinary(.greater_equal, binary),
            .less_than => |binary| prepareBinary(.less_than, binary),
            .less_equal => |binary| prepareBinary(.less_equal, binary),
            .neg_bool => |index| .{
                .tag = .neg_bool,
                .a = index.toInt(),
            },
            .neg_num => |index| .{
                .tag = .neg_num,
                .a = index.toInt(),
            },
            .block => |indexes| try self.prepareBlock(.block, indexes),
            .block_semicolon => |indexes| try self.prepareBlock(
                .block_semicolon,
                indexes,
            ),
            .identifier => .{ .tag = .identifier },
            .assignment => |binary| prepareBinary(.assignment, binary),
            .@"if" => |conditional| .{
                .tag = .@"if",
                .a = conditional.condition.toInt(),
                .b = conditional.body.toInt(),
            },
            .if_else => |if_else| blk: {
                try self.ast.extra.appendSlice(self.allocator, &[_]u32{
                    if_else.conditional.body.toInt(),
                    if_else.else_block.toInt(),
                });

                break :blk .{
                    .tag = .if_else,
                    .a = if_else.conditional.condition.toInt(),
                    .b = @intCast(self.ast.extra.items.len - 2),
                };
            },
            .if_elseif => |if_elseif| blk: {
                const data: []const u32 = @ptrCast(if_elseif.conditionals);

                try self.ast.extra.appendSlice(
                    self.allocator,
                    data,
                );

                break :blk .{
                    .tag = .if_elseif,
                    .a = @intCast(if_elseif.conditionals.len),
                    .b = @intCast(self.ast.extra.items.len - data.len),
                };
            },
            .if_elseif_else => |if_elseif_else| blk: {
                const data: []const u32 = @ptrCast(if_elseif_else.conditionals);

                try self.ast.extra.ensureUnusedCapacity(
                    self.allocator,
                    data.len + 1,
                );
                self.ast.extra.appendSliceAssumeCapacity(data);
                self.ast.extra.appendAssumeCapacity(
                    if_elseif_else.else_block.toInt(),
                );

                break :blk .{
                    .tag = .if_elseif_else,
                    .a = @intCast(if_elseif_else.conditionals.len),
                    .b = @intCast(self.ast.extra.items.len - data.len - 1),
                };
            },
            .@"for" => |index| .{
                .tag = .@"for",
                .a = index.toInt(),
            },
            .for_conditional => |conditional| .{
                .tag = .for_conditional,
                .a = conditional.condition.toInt(),
                .b = conditional.body.toInt(),
            },
            .@"break" => .{ .tag = .@"break" },
            .@"continue" => .{ .tag = .@"continue" },
            .@"return" => .{ .tag = .@"return" },
            .return_value => |index| .{
                .tag = .return_value,
                .a = index.toInt(),
            },
            .call => |call| blk: {
                try self.ast.extra.ensureUnusedCapacity(
                    self.allocator,
                    call.args.len + 1,
                );
                self.ast.extra.appendAssumeCapacity(@intCast(call.args.len));
                self.ast.extra.appendSliceAssumeCapacity(@ptrCast(call.args));

                break :blk .{
                    .tag = .call,
                    .a = call.callee.toInt(),
                    .b = @intCast(self.ast.extra.items.len - call.args.len - 1),
                };
            },
            .call_simple => |call_simple| .{
                .tag = .call_simple,
                .a = call_simple.callee.toInt(),
                .b = if (call_simple.arg) |arg| arg.toInt() else 0,
            },

            .assert => |expr| .{ .tag = .assert, .a = expr.toInt() },
            .print => |expr| .{ .tag = .print, .a = expr.toInt() },
            .expr_stmt => |expr| .{ .tag = .expr_stmt, .a = expr.toInt() },
            .let => |let| try self.prepareLet(.let, let),
            .let_mut => |let| try self.prepareLet(.let_mut, let),
            .@"fn" => |@"fn"| blk: {
                try self.ast.extra.ensureUnusedCapacity(
                    self.allocator,
                    @"fn".params.len + 3,
                );
                self.ast.extra.appendAssumeCapacity(@intCast(@"fn".params.len));
                self.ast.extra.appendSliceAssumeCapacity(
                    @ptrCast(@"fn".params),
                );
                self.ast.extra.appendSliceAssumeCapacity(
                    &[_]u32{
                        @"fn".return_type.toInt(),
                        @"fn".body.toInt(),
                    },
                );

                break :blk .{
                    .tag = .@"fn",
                    .a = @"fn".identifier.toInt(),
                    .b = @intCast(
                        self.ast.extra.items.len - @"fn".params.len * 2 - 3,
                    ),
                };
            },
        };
    }

    fn prepareBinary(
        tag: Ast.Node.Tag,
        binary: Ast.Key.Binary,
    ) Ast.Node {
        return .{
            .tag = tag,
            .a = binary.lhs.toInt(),
            .b = binary.rhs.toInt(),
        };
    }

    fn prepareBlock(
        self: *Parser,
        tag: Ast.Node.Tag,
        indexes: []const Ast.Index,
    ) Allocator.Error!Ast.Node {
        try self.ast.extra.appendSlice(self.allocator, @ptrCast(indexes));

        return .{
            .tag = tag,
            .a = @intCast(indexes.len),
            .b = @intCast(self.ast.extra.items.len - indexes.len),
        };
    }

    fn prepareLet(
        self: *Parser,
        tag: Ast.Node.Tag,
        let: Ast.Key.Let,
    ) Allocator.Error!Ast.Node {
        try self.ast.extra.appendSlice(self.allocator, &[_]u32{
            if (let.type) |@"type"| @"type".toInt() else 0,
            if (let.expr) |expr| expr.toInt() else 0,
        });

        return .{
            .tag = tag,
            .a = let.identifier.toInt(),
            .b = @intCast(self.ast.extra.items.len - 2),
        };
    }

    fn addDiag(
        self: *Parser,
        diag: Diags.Entry.Tag,
        loc: Loc,
    ) Allocator.Error!void {
        try self.diags.entries.append(
            self.allocator,
            .{ .tag = diag, .loc = loc },
        );
    }
};
