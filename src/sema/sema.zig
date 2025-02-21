const std = @import("std");
const shared = @import("shared");
const sema_ast_mod = @import("sema_ast.zig");
const tokenizer_mod = @import("../tokenizer.zig");
const parser_mod = @import("../parser.zig");
const limits = @import("../limits.zig");
const ast_mod = @import("../ast.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const meta = std.meta;
const allocPrint = std.fmt.allocPrint;
const expectError = std.testing.expectError;
const SharedDiags = shared.Diags;
const Writer = shared.Writer;
const SemaExpr = sema_ast_mod.SemaExpr;
const SemaStmt = sema_ast_mod.SemaStmt;
const SemaType = sema_ast_mod.SemaType;
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const Loc = tokenizer_mod.Loc;
const Parser = parser_mod.Parser;
const Ast = ast_mod.Ast;

pub const Sema = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        SemaFailure,
    };

    pub const DiagEntry = struct {
        pub const SemaTypeTuple = struct { SemaType, SemaType };

        pub const ArityMismatch = struct { u8, u8 };

        pub const ArgTypeMismatch = struct { u8, SemaType, SemaType };

        pub const Kind = union(enum) {
            expected_expr_type: SemaType,
            unexpected_arithmetic_type: SemaType,
            unexpected_operand_type: SemaTypeTuple,
            unexpected_concat_type: SemaTypeTuple,
            unexpected_equality_type: SemaTypeTuple,
            unexpected_comparison_type: SemaType,
            unexpected_logical_type: SemaType,
            unexpected_logical_negation_type: SemaType,
            unexpected_arithmetic_negation_type: SemaType,
            too_many_locals,
            value_not_found: []const u8,
            unexpected_assignment_type: SemaTypeTuple,
            immutable_mutation: []const u8,
            type_not_found: []const u8,
            value_not_assigned: []const u8,
            unexpected_else_type: SemaTypeTuple,
            break_outside_loop,
            continue_outside_loop,
            unexpected_elseif_type: SemaTypeTuple,
            unexpected_return_type: SemaTypeTuple,
            not_callable,
            arity_mismatch: ArityMismatch,
            unexpected_arg_type: ArgTypeMismatch,
            not_all_branches_return,
        };

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
            switch (self.kind) {
                .value_not_found,
                .immutable_mutation,
                .type_not_found,
                .value_not_assigned,
                => |name| allocator.free(name),

                .expected_expr_type,
                .unexpected_arithmetic_type,
                .unexpected_comparison_type,
                .unexpected_logical_type,
                .unexpected_logical_negation_type,
                .unexpected_arithmetic_negation_type,
                => |*sema_type| {
                    deinitSemaType(allocator, sema_type);
                },

                .unexpected_operand_type,
                .unexpected_concat_type,
                .unexpected_equality_type,
                .unexpected_assignment_type,
                .unexpected_else_type,
                .unexpected_elseif_type,
                .unexpected_return_type,
                => |*sema_types| {
                    deinitSemaType(allocator, &sema_types[0]);
                    deinitSemaType(allocator, &sema_types[1]);
                },

                .unexpected_arg_type,
                => |*info| {
                    deinitSemaType(allocator, &info[1]);
                    deinitSemaType(allocator, &info[2]);
                },

                .too_many_locals,
                .break_outside_loop,
                .continue_outside_loop,
                .not_callable,
                .arity_mismatch,
                .not_all_branches_return,
                => {},
            }
        }

        fn deinitSemaType(allocator: Allocator, sema_type: *SemaType) void {
            switch (sema_type.*) {
                .unit,
                .int,
                .float,
                .bool,
                .string,
                .invalid,
                .never,
                => {},

                .@"fn" => |*@"fn"| {
                    @"fn".arg_types.clearAndFree();
                    allocator.destroy(@"fn".return_type);
                },
            }
        }

        kind: Kind,
        position: Loc,
    };

    pub const Diags = SharedDiags(DiagEntry);

    const FnCtx = struct {
        const Scope = struct {
            prev: ?*Scope = null,
            types_map: StringHashMap(SemaType),
            locals_map: StringHashMap(usize),
            locals_top: usize,

            fn init(allocator: Allocator, prev_opt: ?*Scope) Scope {
                return .{
                    .prev = prev_opt,
                    .types_map = StringHashMap(SemaType).init(allocator),
                    .locals_map = StringHashMap(usize).init(allocator),
                    .locals_top = if (prev_opt) |prev| prev.locals_top else 0,
                };
            }
        };

        const Local = struct {
            sema_type: ?SemaType,
            is_mutable: bool,
            is_assigned: bool,
        };

        prev: ?*FnCtx = null,
        locals: ArrayList(Local) = undefined,
        current_scope: *Scope = undefined,
        is_in_loop: bool = false,
        break_pops: usize = 0,
        return_type: SemaType,
    };

    allocator: Allocator,
    diags: ?*Diags = null,
    had_error: bool = false,
    ast: *const Ast = undefined,
    fn_ctx: *FnCtx = undefined,

    pub fn init(allocator: *ArenaAllocator) Self {
        return .{
            .allocator = allocator.allocator(),
        };
    }

    pub fn analyze(
        self: *Self,
        ast: *const Ast,
        diags: ?*Diags,
    ) Error!*SemaStmt.Kind.Fn {
        self.diags = diags;
        self.had_error = false;
        self.ast = ast;

        var current_scope = FnCtx.Scope.init(self.allocator, null);
        var fn_ctx = FnCtx{
            .locals = ArrayList(FnCtx.Local).init(self.allocator),
            .current_scope = &current_scope,
            .return_type = .unit,
        };

        self.fn_ctx = &fn_ctx;

        // todo: is this the best place for this?
        try self.fn_ctx.current_scope.types_map.put("Int", .int);
        try self.fn_ctx.current_scope.types_map.put("Float", .float);
        try self.fn_ctx.current_scope.types_map.put("Bool", .bool);
        try self.fn_ctx.current_scope.types_map.put("String", .string);
        try self.fn_ctx.current_scope.types_map.put("Unit", .unit);

        const sema_fn = try self.analyzeFnStmtAux(
            "",
            null,
            null,
            Ast.Index.from(0),
            .{
                .index = 0,
                .len = 0,
            },
        );

        if (self.had_error) {
            return error.SemaFailure;
        }

        return &sema_fn.kind.@"fn";
    }

    fn analyzeStmt(
        self: *Self,
        key: Ast.Key,
        loc: Loc,
        evals: bool,
    ) Error!*SemaStmt {
        return switch (key) {
            .assert,
            => |index| try self.analyzeAssertStmt(index, loc),

            .print,
            => |index| try self.analyzePrintStmt(index, loc),

            .expr_stmt,
            => |index| try self.analyzeExprStmt(index, loc, evals),

            .let,
            .let_mut,
            => |let| try self.analyzeLetStmt(key, let, loc),

            .@"fn",
            => |@"fn"| try self.analyzeFnStmtAux(
                @"fn".identifier.toStr(self.ast),
                @"fn".params,
                @"fn".return_type,
                @"fn".body,
                loc,
            ),

            else => unreachable,
        };
    }

    fn analyzeAssertStmt(
        self: *Self,
        index: Ast.Index,
        loc: Loc,
    ) Error!*SemaStmt {
        const expr = try self.analyzeExpr(index, true);

        if (!typeSatisfies(expr.sema_type, .bool)) {
            return self.semaFailure(
                loc,
                .{ .expected_expr_type = .bool },
            );
        }

        return try SemaStmt.Kind.Assert.create(self.allocator, expr, loc);
    }

    fn analyzePrintStmt(
        self: *Self,
        index: Ast.Index,
        loc: Loc,
    ) Error!*SemaStmt {
        const expr = try self.analyzeExpr(index, true);

        return try SemaStmt.Kind.Print.create(self.allocator, expr, loc);
    }

    fn analyzeExprStmt(
        self: *Self,
        index: Ast.Index,
        loc: Loc,
        evals: bool,
    ) Error!*SemaStmt {
        const inner_expr = try self.analyzeExpr(index, evals);

        return try SemaStmt.Kind.Expr.create(
            self.allocator,
            inner_expr,
            loc,
        );
    }

    fn analyzeLetStmt(
        self: *Self,
        key: Ast.Key,
        let: Ast.Key.Let,
        loc: Loc,
    ) Error!*SemaStmt {
        const analyzed_type_opt: ?SemaType = if (let.type) |parsed_type|
            try self.analyzeType(parsed_type)
        else
            null;

        const parsed_expr = let.expr orelse {
            const index = try self.declareVariable(
                key == .let_mut,
                false,
                let.identifier.toStr(self.ast),
                analyzed_type_opt,
            );

            return try SemaStmt.Kind.Let.create(
                self.allocator,
                index,
                null,
                loc,
            );
        };

        const expr = self.analyzeExpr(parsed_expr, true) catch |err| switch (err) {
            error.SemaFailure => {
                _ = try self.declareVariable(
                    key == .let_mut,
                    true,
                    let.identifier.toStr(self.ast),
                    .invalid,
                );
                return error.SemaFailure;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        if (self.fn_ctx.current_scope.locals_top >= limits.max_locals) {
            try self.addDiag(loc, .too_many_locals);
        }

        var sema_type = expr.sema_type;

        if (analyzed_type_opt) |analyzed_type| {
            if (typeSatisfies(expr.sema_type, analyzed_type)) {
                sema_type = analyzed_type;
            } else {
                try self.addDiag(loc, .{ .expected_expr_type = analyzed_type });
                sema_type = .invalid;
            }
        }

        const index = try self.declareVariable(
            key == .let_mut,
            true,
            let.identifier.toStr(self.ast),
            sema_type,
        );

        return try SemaStmt.Kind.Let.create(
            self.allocator,
            index,
            expr,
            loc,
        );
    }

    fn analyzeFnStmt(
        self: *Self,
        @"fn": Ast.Index,
        position: Loc,
    ) Error!*SemaStmt {
        const key = @"fn".toKey(self.ast).@"fn";
        return self.analyzeFnStmtAux(
            key.identifier.toLoc(self.ast).toSlice(self.source),
            key.params,
            key.return_type,
            key.body,
            position,
        ) catch |err| {
            if (err == error.SemaFailure) {
                _ = try self.declareVariable(false, true, @"fn".name, .invalid);
            }

            return err;
        };
    }

    fn analyzeFnStmtAux(
        self: *Self,
        name: []const u8,
        args_opt: ?[]const Ast.Key.FnArg,
        parsed_return_type_opt: ?Ast.Index,
        parsed_body: Ast.Index,
        position: Loc,
    ) Error!*SemaStmt {
        const return_type = if (parsed_return_type_opt) |parsed_return_type|
            try self.analyzeType(parsed_return_type)
        else
            .unit;

        var arg_types = ArrayList(SemaType).init(self.allocator);

        if (args_opt) |args| {
            for (args) |arg| {
                try arg_types.append(try self.analyzeType(arg.type));
            }
        }

        const fn_type = try SemaType.Fn.init(self.allocator, arg_types, return_type);
        const prev_fn_ctx = self.fn_ctx;
        var current_scope = FnCtx.Scope.init(self.allocator, null);
        var fn_ctx = FnCtx{
            .locals = ArrayList(FnCtx.Local).init(self.allocator),
            .current_scope = &current_scope,
            .return_type = return_type,
            .prev = prev_fn_ctx,
        };

        var body: *SemaExpr = undefined;
        var locals_count: u8 = undefined;

        {
            self.fn_ctx = &fn_ctx;
            defer self.fn_ctx = prev_fn_ctx;

            _ = try self.declareFn(name, fn_type);

            if (args_opt) |args| {
                for (args, arg_types.items) |arg, arg_type| {
                    _ = try self.declareVariable(
                        false,
                        true,
                        arg.identifier.toStr(self.ast),
                        arg_type,
                    );
                }
            }

            body = try self.analyzeExpr(parsed_body, false);

            if (return_type != .unit) {
                var has_never = false;

                for (body.kind.block.stmts.items) |stmt| {
                    if (isNeverStmt(stmt)) {
                        has_never = true;
                        break;
                    }
                }

                if (!has_never) {
                    return self.semaFailure(position, .not_all_branches_return);
                }
            }

            locals_count = @intCast(self.fn_ctx.locals.items.len);
        }

        const index = if (name.len > 0)
            try self.declareFn(name, fn_type)
        else
            null;

        return try SemaStmt.Kind.Fn.create(
            self.allocator,
            name,
            index,
            @intCast(locals_count),
            body,
            position,
        );
    }

    fn analyzeType(self: *Self, parsed_type: Ast.Index) Error!SemaType {
        const key = parsed_type.toKey(self.ast);
        return switch (key) {
            .identifier => try self.analyzeIdentifierType(parsed_type),
            else => unreachable,
        };
    }

    fn analyzeIdentifierType(
        self: *Self,
        identifier: Ast.Index,
    ) Error!SemaType {
        const loc = identifier.toLoc(self.ast);
        const name = identifier.toStr(self.ast);
        return self.getType(name) orelse
            self.semaFailure(loc, .{ .type_not_found = name });
    }

    fn analyzeExpr(
        self: *Self,
        expr: Ast.Index,
        evals: bool,
    ) Error!*SemaExpr {
        const key = expr.toKey(self.ast);
        const loc = expr.toLoc(self.ast);
        const sema_expr = switch (key) {
            .literal_unit,
            .literal_int,
            .literal_float,
            .literal_bool,
            .literal_string,
            => try self.analyzeLiteralExpr(expr, key, loc, evals),

            .add,
            .sub,
            .mul,
            .div,
            .concat,
            .@"and",
            .@"or",
            .equal,
            .not_equal,
            .greater_than,
            .greater_equal,
            .less_than,
            .less_equal,
            => |binary| try self.analyzeBinaryExpr(
                key,
                binary,
                loc,
                evals,
            ),

            .neg_bool,
            .neg_num,
            => |rhs| try self.analyzeUnaryExpr(key, rhs, loc, evals),

            .block,
            .block_semicolon,
            => |stmts| try self.analyzeBlockExpr(key, stmts, loc, evals),

            .identifier,
            => try self.analyzeVariableExpr(expr, loc, evals),

            .assignment,
            => |binary| try self.analyzeAssignmentExpr(binary, loc, evals),

            .@"if",
            .if_else,
            .if_elseif,
            .if_elseif_else,
            => try self.analyzeIfExpr(key, loc, evals),

            .@"for",
            .for_conditional,
            => try self.analyzeForExpr(key, loc, evals),

            .@"break",
            => try self.analyzeBreakExpr(loc, evals),

            .@"continue",
            => try self.analyzeContinueExpr(loc, evals),

            .@"return",
            .return_value,
            => try self.analyzeReturnExpr(key, loc, evals),

            .call,
            .call_simple,
            => try self.analyzeCallExpr(key, loc, evals),

            .assert,
            .print,
            .expr_stmt,
            .let,
            .let_mut,
            .@"fn",
            => unreachable,
        };

        return sema_expr;
    }

    fn analyzeLiteralExpr(
        self: *Self,
        expr: Ast.Index,
        literal: Ast.Key,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        const str = expr.toStr(self.ast);
        const literal_variant: SemaExpr.Kind.Literal = switch (literal) {
            .literal_unit => .unit,
            .literal_int => .{ .int = std.fmt.parseInt(i64, str, 10) catch unreachable },
            .literal_float => .{ .float = std.fmt.parseFloat(f64, str) catch unreachable },
            .literal_bool => .{ .bool = str.len == 4 },
            .literal_string => .{ .string = try self.allocator.dupe(u8, str[1 .. str.len - 1]) },
            else => unreachable,
        };

        return SemaExpr.Kind.Literal.create(
            self.allocator,
            literal_variant,
            evals,
            loc,
        );
    }

    fn analyzeBinaryExpr(
        self: *Self,
        key: Ast.Key,
        binary: Ast.Key.Binary,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        var lhs = try self.analyzeExpr(binary.lhs, true);

        self.fn_ctx.break_pops += 1;

        var rhs = try self.analyzeExpr(binary.rhs, true);

        self.fn_ctx.break_pops -= 1;

        if (lhs.sema_type == .invalid or rhs.sema_type == .invalid) {
            return try SemaExpr.Kind.Binary.create(
                self.allocator,
                .add_int, // binary kind doesn't matter here
                lhs,
                rhs,
                .invalid,
                evals,
                loc,
            );
        }

        const sema_type = switch (key) {
            .add,
            .sub,
            .mul,
            .div,
            => try self.analyzeArithmeticBinaryExpr(lhs, rhs, loc),

            .concat,
            => try self.analyzeConcatBinaryExpr(lhs, rhs, loc),

            .equal,
            .not_equal,
            => try self.analyzeEqualBinaryExpr(lhs, rhs, loc),

            .greater_than,
            .greater_equal,
            .less_than,
            .less_equal,
            => try self.analyzeComparisonBinaryExpr(lhs, rhs, loc),

            .@"and",
            .@"or",
            => blk: {
                const res = try self.analyzeLogicalBinaryExpr(lhs, rhs, loc);

                if (!isBranching(lhs)) {
                    lhs = try self.wrapInBoolComparison(lhs);
                }

                if (!isBranching(rhs)) {
                    rhs = try self.wrapInBoolComparison(rhs);
                }

                break :blk res;
            },

            else => unreachable,
        };

        return try SemaExpr.Kind.Binary.create(
            self.allocator,
            translateBinaryKind(lhs.sema_type, key),
            lhs,
            rhs,
            sema_type,
            evals,
            loc,
        );
    }

    fn analyzeArithmeticBinaryExpr(
        self: *Self,
        lhs: *SemaExpr,
        rhs: *SemaExpr,
        loc: Loc,
    ) Error!SemaType {
        if (!typeSatisfies(lhs.sema_type, .int) and !typeSatisfies(rhs.sema_type, .float)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_arithmetic_type = lhs.sema_type },
            );
        }

        if (!typeSatisfies(rhs.sema_type, lhs.sema_type)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_operand_type = .{ lhs.sema_type, rhs.sema_type } },
            );
        }

        return lhs.sema_type;
    }

    fn analyzeConcatBinaryExpr(
        self: *Self,
        lhs: *SemaExpr,
        rhs: *SemaExpr,
        loc: Loc,
    ) Error!SemaType {
        if (!typeSatisfies(lhs.sema_type, .string) or !typeSatisfies(rhs.sema_type, .string)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_concat_type = .{ lhs.sema_type, rhs.sema_type } },
            );
        }

        return lhs.sema_type;
    }

    fn analyzeEqualBinaryExpr(
        self: *Self,
        lhs: *SemaExpr,
        rhs: *SemaExpr,
        loc: Loc,
    ) Error!SemaType {
        if (!typeSatisfies(rhs.sema_type, lhs.sema_type)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_equality_type = .{ lhs.sema_type, rhs.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeComparisonBinaryExpr(
        self: *Self,
        lhs: *SemaExpr,
        rhs: *SemaExpr,
        loc: Loc,
    ) Error!SemaType {
        if (!typeSatisfies(lhs.sema_type, .int) and !typeSatisfies(rhs.sema_type, .float)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_comparison_type = lhs.sema_type },
            );
        }

        if (!typeSatisfies(rhs.sema_type, lhs.sema_type)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_operand_type = .{ lhs.sema_type, rhs.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeLogicalBinaryExpr(
        self: *Self,
        lhs: *SemaExpr,
        rhs: *SemaExpr,
        loc: Loc,
    ) Error!SemaType {
        if (!typeSatisfies(lhs.sema_type, .bool)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_logical_type = lhs.sema_type },
            );
        }

        if (!typeSatisfies(rhs.sema_type, lhs.sema_type)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_operand_type = .{ lhs.sema_type, rhs.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeUnaryExpr(
        self: *Self,
        key: Ast.Key,
        rhs: Ast.Index,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        const rhs_key = rhs.toKey(self.ast);
        const right = try self.analyzeExpr(rhs, true);

        if (right.sema_type == .invalid) {
            return try SemaExpr.Kind.Unary.create(
                self.allocator,
                .negate_bool, // unary kind doesn't matter here
                right,
                .invalid,
                evals,
                loc,
            );
        }

        const sema_type = switch (key) {
            .neg_bool,
            => try self.analyzeNegateBoolUnaryExpr(right, loc),

            .neg_num,
            => try self.analyzeNegateNumUnaryExpr(right, loc),

            else => {
                std.debug.print("{any}\n", .{rhs_key});
                unreachable;
            },
        };

        return try SemaExpr.Kind.Unary.create(
            self.allocator,
            translateUnaryKind(right.sema_type, rhs_key),
            right,
            sema_type,
            evals,
            loc,
        );
    }

    fn analyzeNegateBoolUnaryExpr(
        self: *Self,
        rhs: *SemaExpr,
        loc: Loc,
    ) Error!SemaType {
        if (!typeSatisfies(rhs.sema_type, .bool)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_logical_negation_type = rhs.sema_type },
            );
        }

        return rhs.sema_type;
    }

    fn analyzeNegateNumUnaryExpr(
        self: *Self,
        rhs: *SemaExpr,
        loc: Loc,
    ) Error!SemaType {
        if (!typeSatisfies(rhs.sema_type, .int) and !typeSatisfies(rhs.sema_type, .float)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_arithmetic_negation_type = rhs.sema_type },
            );
        }

        return rhs.sema_type;
    }

    fn analyzeBlockExpr(
        self: *Self,
        key: Ast.Key,
        stmts: []const Ast.Index,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        var scope = FnCtx.Scope.init(self.allocator, self.fn_ctx.current_scope);
        var sema_stmts = ArrayList(*SemaStmt).init(self.allocator);
        var sema_type: SemaType = undefined;

        {
            self.fn_ctx.current_scope = &scope;
            defer self.fn_ctx.current_scope = self.fn_ctx.current_scope.prev.?;

            var is_never = false;

            for (stmts, 0..) |stmt, index| {
                const stmt_key = stmt.toKey(self.ast);
                const stmt_loc = stmt.toLoc(self.ast);
                const should_eval =
                    index == stmts.len - 1 and
                    key == .block and
                    stmt_key == .expr_stmt and
                    evals;

                const sema_stmt = self.analyzeStmt(
                    stmt_key,
                    stmt_loc,
                    should_eval,
                ) catch |err| switch (err) {
                    error.SemaFailure => continue,
                    error.OutOfMemory => return error.OutOfMemory,
                };

                try sema_stmts.append(sema_stmt);

                if (isNeverStmt(sema_stmt)) {
                    is_never = true;
                    break;
                }
            }

            if (!is_never and (sema_stmts.items.len == 0 or
                sema_stmts.getLast().kind != .expr or
                key == .block_semicolon))
            {
                try sema_stmts.append(try self.unitStmt(loc, evals));
            }

            sema_type = if (is_never)
                .never
            else
                sema_stmts.getLast().kind.expr.expr.sema_type;
        }

        return try SemaExpr.Kind.Block.create(
            self.allocator,
            sema_stmts,
            sema_type,
            evals,
            loc,
        );
    }

    fn analyzeVariableExpr(
        self: *Self,
        expr: Ast.Index,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        const name = expr.toStr(self.ast);
        const index, const local = self.getVariable(name) orelse {
            return self.semaFailure(
                loc,
                .{ .value_not_found = name },
            );
        };

        if (!local.is_assigned) {
            return self.semaFailure(
                loc,
                .{ .value_not_assigned = name },
            );
        }

        return try SemaExpr.Kind.Variable.create(
            self.allocator,
            index,
            local.sema_type.?,
            evals,
            loc,
        );
    }

    fn analyzeAssignmentExpr(
        self: *Self,
        binary: Ast.Key.Binary,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        const name = binary.lhs.toStr(self.ast);

        const index, const local = self.getVariable(name) orelse {
            return self.semaFailure(
                loc,
                .{ .value_not_found = name },
            );
        };

        if (!local.is_mutable and local.is_assigned) {
            return self.semaFailure(
                loc,
                .{ .immutable_mutation = name },
            );
        }

        const right = try self.analyzeExpr(binary.rhs, true);

        self.fn_ctx.locals.items[index].is_assigned = true;

        if (local.sema_type != null and !typeSatisfies(right.sema_type, local.sema_type.?)) {
            return self.semaFailure(
                loc,
                .{ .unexpected_assignment_type = .{ local.sema_type.?, right.sema_type } },
            );
        } else if (local.sema_type == null) {
            self.fn_ctx.locals.items[index].sema_type = right.sema_type;
        }

        return try SemaExpr.Kind.Assignment.create(
            self.allocator,
            index,
            right,
            evals,
            loc,
        );
    }

    fn analyzeIfExpr(
        self: *Self,
        key: Ast.Key,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        var condition: Ast.Key.Conditional = undefined;
        var elseif_blocks: []const Ast.Key.Conditional = &[_]Ast.Key.Conditional{};
        var else_block: ?Ast.Index = null;

        switch (key) {
            .@"if" => |index| {
                condition = index;
            },
            .if_else => |if_else| {
                condition = if_else.conditional;
                else_block = if_else.else_block;
            },
            .if_elseif => |if_elseif| {
                condition = if_elseif.conditionals[0];
                elseif_blocks = if_elseif.conditionals[1..];
            },
            .if_elseif_else => |if_elseif_else| {
                condition = if_elseif_else.conditionals[0];
                elseif_blocks = if_elseif_else.conditionals[1..];
                else_block = if_elseif_else.else_block;
            },
            else => unreachable,
        }

        return self.analyzeIfAux(
            condition,
            elseif_blocks,
            else_block,
            null,
            loc,
            evals,
        );
    }

    fn analyzeIfAux(
        self: *Self,
        conditional_block: Ast.Key.Conditional,
        elseif_blocks: []const Ast.Key.Conditional,
        else_block_opt: ?Ast.Index,
        sema_type_opt: ?SemaType,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        const condition = try self.analyzeCondition(conditional_block.condition);
        const then_block = try self.analyzeExpr(conditional_block.body, evals);
        const last_then = then_block.kind.block.stmts.getLastOrNull();

        if (evals and else_block_opt == null and
            (last_then == null or
            last_then.?.kind != .expr or
            last_then.?.kind.expr.expr.kind != .literal or
            last_then.?.kind.expr.expr.kind.literal != .unit))
        {
            try then_block.kind.block.stmts.append(
                try SemaStmt.Kind.Expr.create(
                    self.allocator,
                    try SemaExpr.Kind.Literal.create(
                        self.allocator,
                        .unit,
                        true,
                        loc,
                    ),
                    loc,
                ),
            );
            then_block.sema_type = if (then_block.sema_type != .never)
                .unit
            else
                .never;

            if (last_then.?.kind == .expr) {
                last_then.?.kind.expr.expr.evals = false;
            }
        }

        const sema_type = if (sema_type_opt != null and sema_type_opt.? != .never)
            sema_type_opt.?
        else
            then_block.sema_type;

        if (evals and
            sema_type != .never and
            !typeSatisfies(then_block.sema_type, sema_type))
        {
            return self.semaFailure(
                loc,
                .{
                    .unexpected_elseif_type = .{
                        sema_type,
                        then_block.sema_type,
                    },
                },
            );
        }

        const else_block = if (elseif_blocks.len > 0)
            try self.analyzeIfAux(
                elseif_blocks[0],
                elseif_blocks[1..],
                else_block_opt,
                sema_type,
                elseif_blocks[0].body.toLoc(self.ast),
                evals,
            )
        else if (else_block_opt) |else_block|
            try self.analyzeExpr(else_block, evals)
        else
            try SemaExpr.Kind.Block.create(
                self.allocator,
                blk: {
                    var else_block_stmts = ArrayList(*SemaStmt).init(self.allocator);
                    try else_block_stmts.append(try self.unitStmt(loc, evals));
                    break :blk else_block_stmts;
                },
                .unit,
                evals,
                loc,
            );

        if (evals and
            sema_type != .never and
            !typeSatisfies(else_block.sema_type, sema_type))
        {
            return self.semaFailure(
                else_block.position,
                .{
                    .unexpected_else_type = .{
                        sema_type,
                        else_block.sema_type,
                    },
                },
            );
        }

        return try SemaExpr.Kind.If.create(
            self.allocator,
            condition,
            then_block,
            else_block,
            if (then_block.sema_type == .never)
                else_block.sema_type
            else
                then_block.sema_type,
            evals,
            loc,
        );
    }

    fn analyzeCondition(
        self: *Self,
        expr: Ast.Index,
    ) Error!*SemaExpr {
        const condition = try self.analyzeExpr(expr, true);

        if (!typeSatisfies(condition.sema_type, .bool)) {
            return self.semaFailure(
                condition.position,
                .{ .expected_expr_type = .bool },
            );
        }

        return if (isBranching(condition))
            condition
        else
            try self.wrapInBoolComparison(condition);
    }

    fn analyzeForExpr(
        self: *Self,
        key: Ast.Key,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        var condition: *SemaExpr = undefined;
        var body_block: *SemaExpr = undefined;

        {
            const prev_pops = self.fn_ctx.break_pops;

            self.fn_ctx.break_pops = 0;
            defer self.fn_ctx.break_pops = prev_pops;

            condition = if (key == .for_conditional)
                try self.analyzeCondition(key.for_conditional.condition)
            else
                try self.wrapInBoolComparison(try SemaExpr.Kind.Literal.create(
                    self.allocator,
                    .{ .bool = true },
                    true,
                    loc,
                ));

            {
                const prev_is_in_loop = self.fn_ctx.is_in_loop;

                self.fn_ctx.is_in_loop = true;
                defer self.fn_ctx.is_in_loop = prev_is_in_loop;

                body_block = try self.analyzeExpr(
                    if (key == .for_conditional)
                        key.for_conditional.body
                    else
                        key.@"for",
                    false,
                );
            }
        }

        return try SemaExpr.Kind.For.create(
            self.allocator,
            condition,
            body_block,
            evals,
            loc,
        );
    }

    fn analyzeBreakExpr(
        self: *Self,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        if (!self.fn_ctx.is_in_loop) {
            return self.semaFailure(
                loc,
                .break_outside_loop,
            );
        }

        return try SemaExpr.Kind.Break.create(
            self.allocator,
            self.fn_ctx.break_pops,
            evals,
            loc,
        );
    }

    fn analyzeContinueExpr(
        self: *Self,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        if (!self.fn_ctx.is_in_loop) {
            return self.semaFailure(
                loc,
                .continue_outside_loop,
            );
        }

        return try SemaExpr.Kind.Continue.create(
            self.allocator,
            self.fn_ctx.break_pops,
            evals,
            loc,
        );
    }

    fn analyzeReturnExpr(
        self: *Self,
        key: Ast.Key,
        position: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        const sema_right: *SemaExpr = if (key == .return_value)
            self.analyzeExpr(key.return_value, true) catch |err| switch (err) {
                error.SemaFailure => return try SemaExpr.Kind.Return.create(
                    self.allocator,
                    try self.unitLiteral(position, evals),
                    evals,
                    position,
                ),
                else => return err,
            }
        else
            try self.unitLiteral(position, true);

        if (!typeSatisfies(sema_right.sema_type, self.fn_ctx.return_type)) {
            try self.addDiag(
                position,
                .{
                    .unexpected_return_type = .{
                        self.fn_ctx.return_type,
                        sema_right.sema_type,
                    },
                },
            );
        }

        return try SemaExpr.Kind.Return.create(
            self.allocator,
            sema_right,
            evals,
            position,
        );
    }

    fn analyzeCallExpr(
        self: *Self,
        key: Ast.Key,
        loc: Loc,
        evals: bool,
    ) Error!*SemaExpr {
        const callee = if (key == .call)
            try self.analyzeExpr(key.call.callee, true)
        else
            try self.analyzeExpr(key.call_simple.callee, true);

        if (callee.sema_type == .invalid) {
            return SemaExpr.Kind.Call.create(
                self.allocator,
                callee,
                ArrayList(*SemaExpr).init(self.allocator),
                .invalid,
                evals,
                loc,
            );
        }

        if (callee.sema_type != .@"fn") {
            return self.semaFailure(loc, .not_callable);
        }

        const args = if (key == .call)
            key.call.args
        else if (key == .call_simple and key.call_simple.arg == null)
            &[_]Ast.Index{}
        else
            &[_]Ast.Index{key.call_simple.arg.?};

        const arg_types = callee.sema_type.@"fn".arg_types;

        if (arg_types.items.len != args.len) {
            return self.semaFailure(
                loc,
                .{
                    .arity_mismatch = .{
                        @intCast(arg_types.items.len),
                        @intCast(args.len),
                    },
                },
            );
        }

        var sema_args = ArrayList(*SemaExpr).init(self.allocator);

        for (arg_types.items, args, 1..) |arg_type, arg, index| {
            const arg_expr = try self.analyzeExpr(arg, true);

            if (!typeSatisfies(arg_expr.sema_type, arg_type)) {
                return self.semaFailure(
                    arg.toLoc(self.ast),
                    .{
                        .unexpected_arg_type = .{
                            @intCast(index),
                            arg_type,
                            arg_expr.sema_type,
                        },
                    },
                );
            }

            try sema_args.append(arg_expr);
        }

        return SemaExpr.Kind.Call.create(
            self.allocator,
            callee,
            sema_args,
            callee.sema_type.@"fn".return_type.*,
            evals,
            loc,
        );
    }

    fn getType(
        self: *Self,
        name: []const u8,
    ) ?SemaType {
        var current_fn_ctx: ?*FnCtx = self.fn_ctx;

        while (current_fn_ctx) |fn_ctx| {
            var current_scope: ?*FnCtx.Scope = fn_ctx.current_scope;

            while (current_scope) |scope| {
                if (scope.types_map.get(name)) |sema_type| {
                    return sema_type;
                }

                current_scope = scope.prev;
            }

            current_fn_ctx = fn_ctx.prev;
        }

        return null;
    }

    fn declareVariable(
        self: *Self,
        is_mutable: bool,
        is_assigned: bool,
        name: []const u8,
        sema_type: ?SemaType,
    ) Allocator.Error!usize {
        const scope = self.fn_ctx.current_scope;
        const local: FnCtx.Local = .{
            .sema_type = sema_type,
            .is_mutable = is_mutable,
            .is_assigned = is_assigned,
        };

        if (scope.locals_top == self.fn_ctx.locals.items.len) {
            try self.fn_ctx.locals.append(local);
        } else {
            self.fn_ctx.locals.items[scope.locals_top] = local;
        }

        try self.fn_ctx.current_scope.locals_map.put(name, scope.locals_top);
        scope.locals_top += 1;

        return scope.locals_top - 1;
    }

    fn declareFn(
        self: *Self,
        name: []const u8,
        sema_type: ?SemaType,
    ) Allocator.Error!usize {
        return try self.declareVariable(
            false,
            true,
            name,
            sema_type,
        );
    }

    fn getVariable(
        self: *Self,
        name: []const u8,
    ) ?struct { usize, FnCtx.Local } {
        var current_scope: ?*FnCtx.Scope = self.fn_ctx.current_scope;

        while (current_scope) |scope| {
            if (scope.locals_map.get(name)) |index| {
                return .{ index, self.fn_ctx.locals.items[index] };
            }

            current_scope = scope.prev;
        }

        return null;
    }

    fn unitStmt(self: *Self, position: Loc, evals: bool) Error!*SemaStmt {
        return try SemaStmt.Kind.Expr.create(
            self.allocator,
            try self.unitLiteral(position, evals),
            position,
        );
    }

    fn unitLiteral(self: *Self, position: Loc, evals: bool) Error!*SemaExpr {
        return try SemaExpr.Kind.Literal.create(
            self.allocator,
            .unit,
            evals,
            position,
        );
    }

    fn wrapInBoolComparison(self: *Self, expr: *SemaExpr) Error!*SemaExpr {
        return try SemaExpr.Kind.Binary.create(
            self.allocator,
            .equal_bool,
            expr,
            try SemaExpr.Kind.Literal.create(
                self.allocator,
                .{ .bool = true },
                true,
                expr.position,
            ),
            .bool,
            true,
            expr.position,
        );
    }

    fn semaFailure(
        self: *Self,
        position: Loc,
        diag_kind: DiagEntry.Kind,
    ) Error {
        try self.addDiag(position, diag_kind);
        return error.SemaFailure;
    }

    fn addDiag(
        self: *Self,
        position: Loc,
        diag_kind: DiagEntry.Kind,
    ) Error!void {
        self.had_error = true;

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
                    .{
                        DiagEntry.Kind,
                        DiagEntry.SemaTypeTuple,
                        DiagEntry.ArityMismatch,
                        DiagEntry.ArgTypeMismatch,
                        SemaType,
                        SemaType.Fn,
                    },
                ),
                .position = position,
            });
        }
    }

    fn typeSatisfies(sema_type: SemaType, target: SemaType) bool {
        if (sema_type == .invalid or target == .invalid) {
            return true;
        }

        if (sema_type == .never) {
            return true;
        }

        switch (sema_type) {
            .unit,
            .int,
            .float,
            .bool,
            .string,
            .invalid,
            .never,
            => return @intFromEnum(sema_type) == @intFromEnum(target),

            .@"fn" => |@"fn"| {
                if (target != .@"fn") {
                    return false;
                }

                if (!typeSatisfies(@"fn".return_type.*, target.@"fn".return_type.*)) {
                    return false;
                }

                if (@"fn".arg_types.items.len != target.@"fn".arg_types.items.len) {
                    return false;
                }

                for (
                    @"fn".arg_types.items,
                    target.@"fn".arg_types.items,
                ) |type_arg, target_arg| {
                    if (!typeSatisfies(type_arg, target_arg)) {
                        return false;
                    }
                }

                return true;
            },
        }
    }

    fn isBranching(expr: *SemaExpr) bool {
        if (expr.kind != .binary) {
            return false;
        }

        return switch (expr.kind.binary.kind) {
            .equal_int,
            .equal_float,
            .equal_bool,
            .equal_obj,
            .not_equal_int,
            .not_equal_float,
            .not_equal_bool,
            .not_equal_obj,
            .greater_int,
            .greater_float,
            .greater_equal_int,
            .greater_equal_float,
            .less_int,
            .less_float,
            .less_equal_int,
            .less_equal_float,
            .@"or",
            .@"and",
            => true,

            .add_int,
            .add_float,
            .subtract_int,
            .subtract_float,
            .multiply_int,
            .multiply_float,
            .divide_int,
            .divide_float,
            .concat,
            => false,
        };
    }

    fn isNeverStmt(stmt: *const SemaStmt) bool {
        return switch (stmt.kind) {
            .assert => |assert| assert.expr.sema_type == .never,
            .print => |print| print.expr.sema_type == .never,
            .expr => |expr| expr.expr.sema_type == .never,
            .let => |let| let.expr != null and let.expr.?.sema_type == .never,
            .@"fn" => false,
        };
    }

    fn translateBinaryKind(
        sema_type: SemaType,
        key: Ast.Key,
    ) SemaExpr.Kind.Binary.Kind {
        return switch (sema_type) {
            .int => switch (key) {
                .add => .add_int,
                .sub => .subtract_int,
                .div => .divide_int,
                .mul => .multiply_int,
                .equal => .equal_int,
                .not_equal => .not_equal_int,
                .greater_than => .greater_int,
                .greater_equal => .greater_equal_int,
                .less_than => .less_int,
                .less_equal => .less_equal_int,

                else => unreachable,
            },
            .float => switch (key) {
                .add => .add_float,
                .sub => .subtract_float,
                .div => .divide_float,
                .mul => .multiply_float,
                .equal => .equal_float,
                .not_equal => .not_equal_float,
                .greater_than => .greater_float,
                .greater_equal => .greater_equal_float,
                .less_than => .less_float,
                .less_equal => .less_equal_float,

                else => unreachable,
            },
            .bool => switch (key) {
                .equal => .equal_bool,
                .not_equal => .not_equal_bool,
                .@"and" => .@"and",
                .@"or" => .@"or",

                else => unreachable,
            },
            .string => switch (key) {
                .concat => .concat,
                .equal => .equal_obj,
                .not_equal => .not_equal_obj,

                else => unreachable,
            },

            .unit,
            .@"fn",
            .invalid,
            .never,
            => unreachable,
        };
    }

    fn translateUnaryKind(
        sema_type: SemaType,
        unary_rhs_key: Ast.Key,
    ) SemaExpr.Kind.Unary.Kind {
        return switch (sema_type) {
            .int => switch (unary_rhs_key) {
                .neg_num,
                => .negate_int,

                else => unreachable,
            },
            .float => switch (unary_rhs_key) {
                .neg_num,
                => .negate_float,

                else => unreachable,
            },
            .bool => switch (unary_rhs_key) {
                .neg_bool,
                => .negate_bool,

                else => unreachable,
            },

            .string,
            .unit,
            .@"fn",
            .never,
            .invalid,
            => unreachable,
        };
    }
};

test "should free all memory on successful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    var arena_allocator = ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    const source = "print (2 + 2) * -2";
    var tokenizer = Tokenizer.init(source);

    var diags: std.ArrayListUnmanaged(Parser.Diag) = .{};
    defer diags.deinit(allocator);

    var parser = Parser.init(allocator);
    defer parser.deinit();

    var ast = try parser.parse(&tokenizer, &diags);
    defer ast.deinit(allocator);

    // WHEN - THEN
    var sema = Sema.init(&arena_allocator);
    _ = try sema.analyze(&ast, null);
}

test "should free all memory on unsuccessful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    var arena_allocator = ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    const source = "print (2 + 2) * -false";
    var tokenizer = Tokenizer.init(source);

    var parser = Parser.init(allocator);
    defer parser.deinit();

    var diags: std.ArrayListUnmanaged(Parser.Diag) = .{};
    defer diags.deinit(allocator);

    var ast = try parser.parse(&tokenizer, &diags);
    defer ast.deinit(allocator);

    // WHEN - THEN
    var sema = Sema.init(&arena_allocator);

    const result = sema.analyze(&ast, null);
    try expectError(Sema.Error.SemaFailure, result);
}
