const std = @import("std");
const arium = @import("arium");
const shared = @import("shared");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = arium.Token;
const Tokenizer = arium.Tokenizer;
const Position = arium.Position;
const Parser = arium.Parser;
const Sema = arium.Sema;
const SemaExpr = arium.SemaExpr;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const SharedDiagnostics = shared.Diagnostics;
const meta = shared.meta;

pub const Config = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        ConfigParseFailure,
    };

    pub const DirectiveError = error{
        OutOfMemory,
        DirectiveParseFailure,
    };

    pub const DiagnosticEntry = struct {
        message: []const u8,
        position: Position,

        pub fn deinit(self: *DiagnosticEntry, allocator: Allocator) void {
            allocator.free(self.message);
        }
    };

    pub const Diagnostics = SharedDiagnostics(DiagnosticEntry);

    pub const Kind = enum {
        parse,
        sema,
        compile,
        run,
    };

    pub const Directive = enum {
        out,
        err_parser,
        err_sema,
        err_compiler,
        err_vm,
    };

    pub const Expectations = struct {
        allocator: Allocator,
        out: ArrayList(u8),
        err_parser: Parser.Diagnostics,
        err_sema: Sema.Diagnostics,
        err_compiler: Compiler.Diagnostics,
        err_vm: Vm.Diagnostics,

        pub fn init(allocator: Allocator) Expectations {
            return .{
                .allocator = allocator,
                .out = ArrayList(u8).init(allocator),
                .err_parser = Parser.Diagnostics.init(allocator),
                .err_sema = Sema.Diagnostics.init(allocator),
                .err_compiler = Compiler.Diagnostics.init(allocator),
                .err_vm = Vm.Diagnostics.init(allocator),
            };
        }

        pub fn deinit(self: *Expectations) void {
            self.out.clearAndFree();
            self.err_parser.deinit();
            self.err_sema.deinit();
            self.err_compiler.deinit();
            self.err_vm.deinit();
        }
    };

    const DirectiveContext = struct {
        allocator: Allocator,
        kind: Kind,
        diags: *Diagnostics,
        position: Position,
        split_iter: *SplitIter,
        expectations: *Expectations,
    };

    const SplitIter = std.mem.SplitIterator(u8, .scalar);

    allocator: Allocator,
    path: []const u8,
    source: []const u8,
    kind: Kind,
    expectations: Expectations,
    vm_config: Vm.Config,

    pub fn initFromOwnedPathAndSource(
        allocator: Allocator,
        path: []const u8,
        source: []const u8,
        diags: *Diagnostics,
    ) Error!Self {
        errdefer allocator.free(path);
        errdefer allocator.free(source);

        return try parse(allocator, path, source, diags);
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.path);
        self.allocator.free(self.source);
        self.expectations.deinit();
    }

    fn parse(
        allocator: Allocator,
        path: []const u8,
        source: []const u8,
        diags: *Diagnostics,
    ) Error!Self {
        var tokenizer = Tokenizer.init(source);
        const first_token = tokenizer.scanToken();
        const config_line_opt = parseConfigComment(first_token);

        if (config_line_opt == null) {
            return configParseFailure(
                diags,
                first_token.position,
                "Test header is missing.",
                .{},
            );
        }

        const config_line = config_line_opt.?;
        const test_kind_opt = std.meta.stringToEnum(Kind, config_line);

        if (test_kind_opt == null) {
            return configParseFailure(
                diags,
                first_token.position,
                "Invalid test kind {s}.",
                .{config_line},
            );
        }

        const test_kind = test_kind_opt.?;
        var expectations = Expectations.init(allocator);
        errdefer expectations.deinit();

        var current_token = tokenizer.scanToken();

        while (current_token.kind != .eof) : (current_token = tokenizer.scanToken()) {
            const directive_line_opt = parseConfigComment(current_token);

            if (directive_line_opt == null) {
                continue;
            }

            var split = std.mem.splitScalar(u8, directive_line_opt.?, ' ');
            var ctx: DirectiveContext = .{
                .allocator = allocator,
                .kind = test_kind,
                .diags = diags,
                .position = current_token.position,
                .split_iter = &split,
                .expectations = &expectations,
            };

            parseDirective(&ctx) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {},
            };
        }

        if (diags.getLen() > 0) {
            return error.ConfigParseFailure;
        }

        return .{
            .allocator = allocator,
            .path = path,
            .source = source,
            .kind = test_kind,
            .vm_config = .{},
            .expectations = expectations,
        };
    }

    fn parseConfigComment(token: Token) ?[]const u8 {
        return if (token.kind == .comment and
            token.lexeme.len >= 3 and token.lexeme[2] == '/')
            std.mem.trim(u8, token.lexeme[3..], " ")
        else
            null;
    }

    fn parseDirective(ctx: *DirectiveContext) DirectiveError!void {
        const directive_kind_str = try parseStr(ctx);
        const directive_kind_opt = std.meta.stringToEnum(Directive, directive_kind_str);

        if (directive_kind_opt == null) {
            return directiveParseFailure(
                ctx,
                "Invalid directive '{s}'.",
                .{directive_kind_str},
            );
        }

        switch (directive_kind_opt.?) {
            .out => try parseOutDirective(ctx),
            .err_parser => try parseErrParserDirective(ctx),
            .err_sema => try parseErrSemaDirective(ctx),
            .err_compiler => try parseErrCompilerDirective(ctx),
            .err_vm => try parseErrVmDirective(ctx),
        }
    }

    fn parseErrParserDirective(ctx: *DirectiveContext) DirectiveError!void {
        const line = try parseInt(ctx, u64);
        var diag_kind = try parseUnionVariant(ctx, Parser.DiagnosticEntry.Kind);

        switch (diag_kind) {
            .expected_end_token,
            => meta.setUnionValue(&diag_kind, try parseEnumVariant(ctx, Token.Kind)),

            .invalid_token,
            => meta.setUnionValue(&diag_kind, parseRestString(ctx)),

            .expected_statement,
            .expected_expression,
            .expected_left_paren_before_expr,
            .expected_right_paren_after_expr,
            .int_literal_overflows,
            => {},
        }

        try parseEndOfLine(ctx);

        try ctx.expectations.err_parser.add(.{
            .kind = diag_kind,
            .position = .{
                .line = line,
                .column = 0, // not part of the check for now
            },
        });
    }

    fn parseErrSemaDirective(ctx: *DirectiveContext) DirectiveError!void {
        if (ctx.kind != .sema and ctx.kind != .compile and ctx.kind != .run) {
            return illegalDirectiveFailure(ctx, .err_sema);
        }

        const line = try parseInt(ctx, u64);
        var diag_kind = try parseUnionVariant(ctx, Sema.DiagnosticEntry.Kind);

        switch (diag_kind) {
            .unexpected_operand_type,
            .unexpected_concat_type,
            .unexpected_equality_type,
            => meta.setUnionValue(&diag_kind, Sema.DiagnosticEntry.EvalTypeTuple{
                try parseEvalKind(ctx),
                try parseEvalKind(ctx),
            }),

            .expected_expr_type,
            .unexpected_arithmetic_type,
            .unexpected_comparison_type,
            .unexpected_logical_type,
            .unexpected_logical_negation_type,
            .unexpected_arithmetic_negation_type,
            => meta.setUnionValue(&diag_kind, try parseEvalKind(ctx)),
        }

        try parseEndOfLine(ctx);

        try ctx.expectations.err_sema.add(.{
            .kind = diag_kind,
            .position = .{
                .line = line,
                .column = 0, // not part of the check for now
            },
        });
    }

    fn parseErrCompilerDirective(ctx: *DirectiveContext) DirectiveError!void {
        if (ctx.kind != .compile and ctx.kind != .run) {
            return illegalDirectiveFailure(ctx, .err_compiler);
        }

        const line = try parseInt(ctx, u64);
        const diag_kind = try parseEnumVariant(ctx, Compiler.DiagnosticEntry.Kind);

        try parseEndOfLine(ctx);

        try ctx.expectations.err_compiler.add(.{
            .kind = diag_kind,
            .position = .{
                .line = line,
                .column = 0, // not part of the check for now
            },
        });
    }

    fn parseErrVmDirective(ctx: *DirectiveContext) DirectiveError!void {
        if (ctx.kind != .run) {
            return illegalDirectiveFailure(ctx, .err_vm);
        }

        const line = try parseInt(ctx, u64);
        const diag_kind = try parseEnumVariant(ctx, Vm.DiagnosticEntry.Kind);

        try parseEndOfLine(ctx);

        try ctx.expectations.err_vm.add(.{
            .kind = diag_kind,
            .position = .{
                .line = line,
                .column = 0, // not part of the check for now
            },
        });
    }

    fn parseOutDirective(ctx: *DirectiveContext) DirectiveError!void {
        if (ctx.kind != .run) {
            return illegalDirectiveFailure(ctx, .out);
        }

        try ctx.expectations.out.appendSlice(parseRestString(ctx));
        try ctx.expectations.out.append('\n');
    }

    fn parseEvalKind(
        ctx: *DirectiveContext,
    ) DirectiveError!SemaExpr.EvalType {
        var eval_type = try parseUnionVariant(ctx, SemaExpr.EvalType);

        switch (eval_type) {
            .obj,
            => meta.setUnionValue(
                &eval_type,
                try parseEnumVariant(ctx, SemaExpr.EvalType.ObjKind),
            ),

            .unit,
            .int,
            .float,
            .bool,
            .invalid,
            => {},
        }

        return eval_type;
    }

    fn parseStr(ctx: *DirectiveContext) DirectiveError![]const u8 {
        return nextNonEmpty(ctx) orelse
            return directiveParseFailure(ctx, "Expected string.", .{});
    }

    fn parseInt(ctx: *DirectiveContext, T: type) DirectiveError!T {
        const msg = "Expected {s}.";
        const args = .{@typeName(T)};

        const param = nextNonEmpty(ctx) orelse
            return directiveParseFailure(ctx, msg, args);

        return std.fmt.parseInt(T, param, 10) catch {
            return directiveParseFailure(ctx, msg, args);
        };
    }

    fn parseUnionVariant(ctx: *DirectiveContext, T: type) DirectiveError!T {
        const msg = "Expected a union variant of {s}.";
        const args = .{@typeName(T)};

        const param = nextNonEmpty(ctx) orelse
            return directiveParseFailure(ctx, msg, args);

        return meta.stringToUnion(T, param) orelse
            return directiveParseFailure(ctx, msg, args);
    }

    fn parseEnumVariant(
        ctx: *DirectiveContext,
        comptime T: type,
    ) DirectiveError!T {
        const msg = "Expected an enum variant of {s}.";
        const args = .{@typeName(T)};

        const param = nextNonEmpty(ctx) orelse
            return directiveParseFailure(ctx, msg, args);

        return std.meta.stringToEnum(T, param) orelse
            return directiveParseFailure(ctx, msg, args);
    }

    fn parseRestString(ctx: *DirectiveContext) []const u8 {
        return std.mem.trim(u8, ctx.split_iter.rest(), " ");
    }

    fn parseEndOfLine(ctx: *DirectiveContext) DirectiveError!void {
        if (nextNonEmpty(ctx) != null) {
            return directiveParseFailure(ctx, "Expected end of comment.", .{});
        }
    }

    fn addDiag(
        diags: *Diagnostics,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
    ) error{OutOfMemory}!void {
        try diags.add(.{
            .message = try std.fmt.allocPrint(diags.allocator, fmt, args),
            .position = position,
        });
    }

    fn nextNonEmpty(ctx: *DirectiveContext) ?[]const u8 {
        var next = ctx.split_iter.next();

        while (next != null and next.?.len == 0) {
            next = ctx.split_iter.next();
        }

        return next;
    }

    fn configParseFailure(
        diags: *Diagnostics,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
    ) error{ OutOfMemory, ConfigParseFailure } {
        try addDiag(diags, position, fmt, args);
        return error.ConfigParseFailure;
    }

    fn directiveParseFailure(
        ctx: *DirectiveContext,
        comptime fmt: []const u8,
        args: anytype,
    ) DirectiveError {
        try addDiag(ctx.diags, ctx.position, fmt, args);
        return error.DirectiveParseFailure;
    }

    fn illegalDirectiveFailure(
        ctx: *DirectiveContext,
        directive: Directive,
    ) DirectiveError {
        return directiveParseFailure(
            ctx,
            "Test of kind '{s}' can't use directive '{s}'.",
            .{ @tagName(ctx.kind), @tagName(directive) },
        );
    }
};
