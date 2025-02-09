const std = @import("std");
const arium = @import("arium");
const shared = @import("shared");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const comptimePrint = std.fmt.comptimePrint;
const Token = arium.Token;
const Tokenizer = arium.Tokenizer;
const Loc = arium.Loc;
const Parser = arium.Parser;
const Sema = arium.Sema;
const SemaExpr = arium.SemaExpr;
const SemaType = arium.SemaType;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const SharedDiags = shared.Diags;
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

    pub const DiagEntry = struct {
        message: []const u8,
        position: Loc,

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
            allocator.free(self.message);
        }
    };

    pub const Diags = SharedDiags(DiagEntry);

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
        err_parser: Parser.Diags,
        err_sema: Sema.Diags,
        err_compiler: Compiler.Diags,
        err_vm: Vm.Diags,

        pub fn init(allocator: Allocator) Expectations {
            return .{
                .allocator = allocator,
                .out = ArrayList(u8).init(allocator),
                .err_parser = Parser.Diags.init(allocator),
                .err_sema = Sema.Diags.init(allocator),
                .err_compiler = Compiler.Diags.init(allocator),
                .err_vm = Vm.Diags.init(allocator),
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

    const DirectiveCtx = struct {
        allocator: Allocator,
        kind: Kind,
        diags: *Diags,
        position: Loc,
        split_iter: *SplitIter,
        expectations: *Expectations,
    };

    const SplitIter = std.mem.SplitIterator(u8, .scalar);

    const ParsableTypes = .{
        Parser.DiagEntry.Kind,
        Loc,
        Sema.DiagEntry.Kind,
        Sema.DiagEntry.SemaTypeTuple,
        Sema.DiagEntry.ArityMismatch,
        Sema.DiagEntry.ArgTypeMismatch,
        SemaType,
        SemaType.Fn,
        Compiler.DiagEntry.Kind,
        Vm.DiagEntry.Kind,
        Token.Tag,
    };

    const defaultPosition = Loc{
        .start = 0,
        .end = 0,
    };

    allocator: Allocator,
    path: []const u8,
    source: [:0]const u8,
    kind: Kind,
    expectations: Expectations,
    vm_config: Vm.Config,

    pub fn initFromOwnedPathAndSource(
        allocator: Allocator,
        path: []const u8,
        source: [:0]const u8,
        diags: *Diags,
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
        source: [:0]const u8,
        diags: *Diags,
    ) Error!Self {
        var tokenizer = Tokenizer.init(source);
        const first_token = tokenizer.next();
        const config_line_opt = parseConfigComment(first_token, source);

        if (config_line_opt == null) {
            return configParseFailure(
                diags,
                first_token.loc,
                "Test header is missing.",
                .{},
            );
        }

        const config_line = config_line_opt.?;
        const test_kind_opt = std.meta.stringToEnum(Kind, config_line);

        if (test_kind_opt == null) {
            return configParseFailure(
                diags,
                first_token.loc,
                "Invalid test kind {s}.",
                .{config_line},
            );
        }

        const test_kind = test_kind_opt.?;
        var expectations = Expectations.init(allocator);
        errdefer expectations.deinit();

        var current_token = tokenizer.next();

        while (current_token.tag != .eof) : (current_token = tokenizer.next()) {
            const directive_line_opt = parseConfigComment(current_token, source);

            if (directive_line_opt == null) {
                continue;
            }

            var split_iter = std.mem.splitScalar(u8, directive_line_opt.?, ' ');
            var ctx: DirectiveCtx = .{
                .allocator = allocator,
                .kind = test_kind,
                .diags = diags,
                .position = current_token.loc,
                .split_iter = &split_iter,
                .expectations = &expectations,
            };

            parseDirective(&ctx) catch |err| switch (err) {
                error.DirectiveParseFailure => {},
                error.OutOfMemory => return error.OutOfMemory,
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

    fn parseConfigComment(token: Token, source: []const u8) ?[]const u8 {
        const lexeme = source[token.loc.start..token.loc.end];
        return if (token.tag == .comment and
            lexeme.len >= 3 and lexeme[2] == '/')
            std.mem.trim(u8, lexeme[3..], " ")
        else
            null;
    }

    fn parseDirective(ctx: *DirectiveCtx) DirectiveError!void {
        const directive_kind_str = try parseStr(
            ctx,
            "Expected test directive.",
            .{},
        );
        const directive_kind = std.meta.stringToEnum(
            Directive,
            directive_kind_str,
        ) orelse {
            return directiveParseFailure(
                ctx,
                "Invalid directive '{s}'.",
                .{directive_kind_str},
            );
        };

        const is_illegal = switch (directive_kind) {
            .out => ctx.kind == .run,
            .err_parser => true,
            .err_sema => ctx.kind == .sema or ctx.kind == .compile or ctx.kind == .run,
            .err_compiler => ctx.kind == .compile or ctx.kind == .run,
            .err_vm => ctx.kind == .run,
        };

        if (!is_illegal) {
            return illegalDirectiveFailure(ctx, directive_kind);
        }

        switch (directive_kind) {
            .out => try parseOutDirective(ctx),
            .err_parser => try parseDiag(&ctx.expectations.err_parser, ctx),
            .err_sema => try parseDiag(&ctx.expectations.err_sema, ctx),
            .err_compiler => try parseDiag(&ctx.expectations.err_compiler, ctx),
            .err_vm => try parseDiag(&ctx.expectations.err_vm, ctx),
        }
    }

    fn parseDiag(
        diags: anytype,
        ctx: *DirectiveCtx,
    ) DirectiveError!void {
        const ChildDiagEntry =
            @typeInfo(@TypeOf(diags.entries.items)).pointer.child;

        const line = try parseType(u32, ctx);
        const diag = try parseType(ChildDiagEntry.Kind, ctx);

        try parseEndOfSplitIter(ctx, "Expected end of comment.");

        try diags.add(.{
            .kind = diag,
            .position = .{
                // hack until rewrite: start represents line number, handled with a special case
                .start = line,
                .end = 0,
            },
        });
    }

    fn parseType(T: type, ctx: *DirectiveCtx) DirectiveError!T {
        const type_info = @typeInfo(T);
        const type_name = @typeName(T);

        if (T == []u8 or T == []const u8) {
            return try ctx.allocator.dupe(
                u8,
                try parseStr(ctx, "Expected string.", .{}),
            );
        }

        if (comptime meta.isArrayList(T)) {
            return try parseArrayList(T, ctx);
        }

        if (type_info == .pointer and type_info.pointer.size == .one) {
            return try parseOne(T, ctx);
        }

        switch (type_info) {
            .void,
            => return,

            .int,
            => return try parseInt(T, ctx),

            .@"enum",
            => if (comptime meta.typeInTuple(T, ParsableTypes)) {
                return try parseEnum(T, ctx);
            } else {
                @compileError(comptimePrint(
                    "enum {s} isn't marked as parsable",
                    .{type_name},
                ));
            },

            .@"union",
            => if (comptime meta.typeInTuple(T, ParsableTypes)) {
                return try parseUnion(T, ctx);
            } else {
                @compileError(comptimePrint(
                    "union {s} isn't marked as parsable",
                    .{type_name},
                ));
            },

            .@"struct",
            => {
                if (!comptime meta.typeInTuple(T, ParsableTypes)) {
                    @compileError(comptimePrint(
                        "stuct {s} isn't marked as parsable",
                        .{type_name},
                    ));
                }

                return try parseStruct(T, ctx);
            },

            else => @compileError(comptimePrint(
                "no parsing exists for {s} / {s}",
                .{ type_name, @tagName(type_info) },
            )),
        }
    }

    fn parseOne(T: type, ctx: *DirectiveCtx) DirectiveError!T {
        const type_info = @typeInfo(T);
        const ChildType = type_info.pointer.child;
        const new_instance = try ctx.allocator.create(ChildType);

        new_instance.* = try parseType(ChildType, ctx);

        return new_instance;
    }

    fn parseStruct(T: type, ctx: *DirectiveCtx) DirectiveError!T {
        const type_info = @typeInfo(T);
        var tuple: T = undefined;

        inline for (type_info.@"struct".fields) |field| {
            @field(tuple, field.name) = try parseType(field.type, ctx);
        }

        return tuple;
    }

    fn parseUnion(T: type, ctx: *DirectiveCtx) DirectiveError!T {
        const error_msg = "Expected a union variant of {s}";
        const error_args = .{@typeName(T)};
        const variant = try parseStr(ctx, error_msg, error_args);

        inline for (@typeInfo(T).@"union".fields) |field| {
            if (std.mem.eql(u8, field.name, variant)) {
                const value = try parseType(field.type, ctx);

                return @unionInit(T, field.name, value);
            }
        }

        return directiveParseFailure(ctx, error_msg, error_args);
    }

    fn parseEnum(T: type, ctx: *DirectiveCtx) DirectiveError!T {
        const msg = "Expected an enum variant of {s}.";
        const args = .{@typeName(T)};

        const variant = nextNonEmpty(ctx) orelse
            return directiveParseFailure(ctx, msg, args);

        return std.meta.stringToEnum(T, variant) orelse
            return directiveParseFailure(ctx, msg, args);
    }

    fn parseInt(T: type, ctx: *DirectiveCtx) DirectiveError!T {
        const msg = "Expected {s}.";
        const args = .{@typeName(T)};

        const param = nextNonEmpty(ctx) orelse
            return directiveParseFailure(ctx, msg, args);

        return std.fmt.parseInt(T, param, 10) catch {
            return directiveParseFailure(ctx, msg, args);
        };
    }

    fn parseArrayList(T: type, ctx: *DirectiveCtx) DirectiveError!T {
        const ChildType = @typeInfo(T.Slice).pointer.child;
        var new_array = ArrayList(ChildType).init(ctx.allocator);
        const list_str = try parseGroup(
            ctx,
            "list",
            '[',
            ']',
            "Expected a list of {s}.",
            .{@typeName(ChildType)},
        );
        var split_iter = std.mem.splitScalar(u8, list_str, ',');
        const parent_split_iter = ctx.split_iter;

        while (split_iter.next()) |segment| {
            if (segment.len == 0) {
                continue;
            }

            var inner_split_iter = std.mem.splitScalar(u8, segment, ' ');

            ctx.split_iter = &inner_split_iter;

            try new_array.append(try parseType(ChildType, ctx));
            try parseEndOfSplitIter(ctx, "Expected end of list.");

            ctx.split_iter = parent_split_iter;
        }

        return new_array;
    }

    fn parseGroup(
        ctx: *DirectiveCtx,
        group_name: []const u8,
        begin: u8,
        end: u8,
        comptime error_fmt: []const u8,
        error_args: anytype,
    ) DirectiveError![]const u8 {
        const old_index = ctx.split_iter.index orelse
            return directiveParseFailure(ctx, error_fmt, error_args);
        const next = ctx.split_iter.next() orelse
            return directiveParseFailure(ctx, error_fmt, error_args);

        if (next[0] != begin) {
            return next;
        }

        if (next.len > 1 and next[0] == begin and next[next.len - 1] == end) {
            return next[1 .. next.len - 1];
        }

        while (ctx.split_iter.next()) |split| {
            if (split.len > 0 and split[split.len - 1] == end) {
                const buffer = ctx.split_iter.buffer;

                return if (ctx.split_iter.index) |index|
                    buffer[old_index + 1 .. index - 1]
                else
                    buffer[old_index + 1 .. buffer.len - 1];
            }
        }

        return directiveParseFailure(ctx, "Unterminated {s}.", .{group_name});
    }

    fn parseStr(
        ctx: *DirectiveCtx,
        comptime error_fmt: []const u8,
        error_args: anytype,
    ) DirectiveError![]const u8 {
        return try parseGroup(ctx, "string", '"', '"', error_fmt, error_args);
    }

    fn parseRestStr(ctx: *DirectiveCtx) []const u8 {
        const rest = std.mem.trim(u8, ctx.split_iter.rest(), " ");
        while (ctx.split_iter.next()) |_| {}
        return rest;
    }

    fn parseEndOfSplitIter(
        ctx: *DirectiveCtx,
        comptime msg: []const u8,
    ) DirectiveError!void {
        if (nextNonEmpty(ctx) != null) {
            return directiveParseFailure(ctx, msg, .{});
        }
    }

    fn parseOutDirective(ctx: *DirectiveCtx) DirectiveError!void {
        if (ctx.kind != .run) {
            return illegalDirectiveFailure(ctx, .out);
        }

        try ctx.expectations.out.appendSlice(parseRestStr(ctx));
        try ctx.expectations.out.append('\n');
    }

    fn addDiag(
        diags: *Diags,
        position: Loc,
        comptime fmt: []const u8,
        args: anytype,
    ) error{OutOfMemory}!void {
        try diags.add(.{
            .message = try std.fmt.allocPrint(diags.allocator, fmt, args),
            .position = position,
        });
    }

    fn nextNonEmpty(ctx: *DirectiveCtx) ?[]const u8 {
        var next = ctx.split_iter.next();

        while (next != null and next.?.len == 0) {
            next = ctx.split_iter.next();
        }

        return next;
    }

    fn configParseFailure(
        diags: *Diags,
        position: Loc,
        comptime fmt: []const u8,
        args: anytype,
    ) error{ OutOfMemory, ConfigParseFailure } {
        try addDiag(diags, position, fmt, args);
        return error.ConfigParseFailure;
    }

    fn directiveParseFailure(
        ctx: *DirectiveCtx,
        comptime fmt: []const u8,
        args: anytype,
    ) DirectiveError {
        try addDiag(ctx.diags, ctx.position, fmt, args);
        return error.DirectiveParseFailure;
    }

    fn illegalDirectiveFailure(
        ctx: *DirectiveCtx,
        directive: Directive,
    ) DirectiveError {
        return directiveParseFailure(
            ctx,
            "Test of kind '{s}' can't use directive '{s}'.",
            .{ @tagName(ctx.kind), @tagName(directive) },
        );
    }
};
