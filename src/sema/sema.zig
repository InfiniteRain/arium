const std = @import("std");
const shared = @import("shared");
const parsed_ast_mod = @import("../parser/parsed_ast.zig");
const sema_ast_mod = @import("sema_ast.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const parser_mod = @import("../parser/parser.zig");
const limits = @import("../limits.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const meta = std.meta;
const allocPrint = std.fmt.allocPrint;
const expectError = std.testing.expectError;
const SharedDiags = shared.Diags;
const Writer = shared.Writer;
const ParsedExpr = parsed_ast_mod.ParsedExpr;
const ParsedStmt = parsed_ast_mod.ParsedStmt;
const ParsedType = parsed_ast_mod.ParsedType;
const SemaExpr = sema_ast_mod.SemaExpr;
const SemaStmt = sema_ast_mod.SemaStmt;
const SemaType = sema_ast_mod.SemaType;
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const Position = tokenizer_mod.Position;
const Parser = parser_mod.Parser;

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
        position: Position,
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
    fn_ctx: *FnCtx = undefined,

    pub fn init(allocator: *ArenaAllocator) Self {
        return .{
            .allocator = allocator.allocator(),
        };
    }

    pub fn analyze(
        self: *Self,
        block: *ParsedExpr,
        diags: ?*Diags,
    ) Error!*SemaStmt.Kind.Fn {
        self.diags = diags;
        self.had_error = false;

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
            block,
            .{},
        );

        if (self.had_error) {
            return error.SemaFailure;
        }

        return &sema_fn.kind.@"fn";
    }

    fn analyzeStmt(self: *Self, stmt: *ParsedStmt, evals: bool) Error!*SemaStmt {
        return switch (stmt.kind) {
            .assert => |*assert| try self.analyzeAssertStmt(assert, stmt.position),
            .print => |*print| try self.analyzePrintStmt(print, stmt.position),
            .expr => |*expr| try self.analyzeExprStmt(expr, stmt.position, evals),
            .let => |*let| try self.analyzeLetStmt(let, stmt.position),
            .@"fn" => |*@"fn"| try self.analyzeFnStmt(@"fn", stmt.position),
        };
    }

    fn analyzeAssertStmt(
        self: *Self,
        assert: *ParsedStmt.Kind.Assert,
        position: Position,
    ) Error!*SemaStmt {
        const expr = try self.analyzeExpr(assert.expr, true);

        if (typeSatisfies(expr.sema_type, .bool)) {
            return self.semaFailure(
                position,
                .{ .expected_expr_type = .bool },
            );
        }

        return try SemaStmt.Kind.Assert.create(self.allocator, expr, position);
    }

    fn analyzePrintStmt(
        self: *Self,
        print: *ParsedStmt.Kind.Print,
        position: Position,
    ) Error!*SemaStmt {
        const expr = try self.analyzeExpr(print.expr, true);

        return try SemaStmt.Kind.Print.create(self.allocator, expr, position);
    }

    fn analyzeExprStmt(
        self: *Self,
        expr: *ParsedStmt.Kind.Expr,
        position: Position,
        evals: bool,
    ) Error!*SemaStmt {
        const inner_expr = try self.analyzeExpr(expr.expr, evals);

        return try SemaStmt.Kind.Expr.create(
            self.allocator,
            inner_expr,
            position,
        );
    }

    fn analyzeLetStmt(
        self: *Self,
        let: *ParsedStmt.Kind.Let,
        position: Position,
    ) Error!*SemaStmt {
        const analyzed_type_opt: ?SemaType = if (let.parsed_type) |parsed_type|
            try self.analyzeType(parsed_type)
        else
            null;

        const parsed_expr = let.expr orelse {
            const index = try self.declareVariable(
                let.is_mutable,
                false,
                let.name,
                analyzed_type_opt,
            );

            return try SemaStmt.Kind.Let.create(
                self.allocator,
                index,
                null,
                position,
            );
        };

        const expr = self.analyzeExpr(parsed_expr, true) catch |err| switch (err) {
            error.SemaFailure => {
                _ = try self.declareVariable(
                    let.is_mutable,
                    true,
                    let.name,
                    .invalid,
                );
                return error.SemaFailure;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        if (self.fn_ctx.current_scope.locals_top >= limits.max_locals) {
            try self.addDiag(position, .too_many_locals);
        }

        var sema_type = expr.sema_type;

        if (analyzed_type_opt) |analyzed_type| {
            if (typeSatisfies(expr.sema_type, analyzed_type)) {
                sema_type = analyzed_type;
            } else {
                try self.addDiag(position, .{ .expected_expr_type = analyzed_type });
                sema_type = .invalid;
            }
        }

        const index = try self.declareVariable(
            let.is_mutable,
            true,
            let.name,
            sema_type,
        );

        return try SemaStmt.Kind.Let.create(
            self.allocator,
            index,
            expr,
            position,
        );
    }

    fn analyzeFnStmt(
        self: *Self,
        @"fn": *ParsedStmt.Kind.Fn,
        position: Position,
    ) Error!*SemaStmt {
        return self.analyzeFnStmtAux(
            @"fn".name,
            @"fn".args,
            @"fn".return_type,
            @"fn".body,
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
        args_opt: ?ArrayList(ParsedStmt.Kind.Fn.Arg),
        parsed_return_type_opt: ?*ParsedType,
        parsed_body: *ParsedExpr,
        position: Position,
    ) Error!*SemaStmt {
        const return_type = if (parsed_return_type_opt) |parsed_return_type|
            try self.analyzeType(parsed_return_type)
        else
            .unit;

        var arg_types = ArrayList(SemaType).init(self.allocator);

        if (args_opt) |args| {
            for (args.items) |arg| {
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
                for (args.items, arg_types.items) |arg, arg_type| {
                    _ = try self.declareVariable(
                        false,
                        true,
                        arg.name,
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

    fn analyzeType(self: *Self, parsed_type: *ParsedType) Error!SemaType {
        return switch (parsed_type.kind) {
            .identifier,
            => |identifier| try self.analyzeIdentifierType(identifier, parsed_type.position),
        };
    }

    fn analyzeIdentifierType(
        self: *Self,
        identifier: ParsedType.Kind.Identifier,
        position: Position,
    ) Error!SemaType {
        return self.getType(identifier.name) orelse
            self.semaFailure(position, .{ .type_not_found = identifier.name });
    }

    fn analyzeExpr(
        self: *Self,
        expr: *ParsedExpr,
        evals: bool,
    ) Error!*SemaExpr {
        const sema_expr = switch (expr.kind) {
            .literal => |*literal| try self.analyzeLiteralExpr(literal, expr.position, evals),
            .binary => |*binary| try self.analyzeBinaryExpr(binary, expr.position, evals),
            .unary => |*unary| try self.analyzeUnaryExpr(unary, expr.position, evals),
            .block => |*block| try self.analyzeBlockExpr(block, expr.position, evals),
            .variable => |*variable| try self.analyzeVariableExpr(variable, expr.position, evals),
            .assignment => |*assignment| try self.analyzeAssignmentExpr(assignment, expr.position, evals),
            .@"if" => |*@"if"| try self.analyzeIfExpr(@"if", expr.position, evals),
            .@"for" => |*@"for"| try self.analyzeForExpr(@"for", expr.position, evals),
            .@"break" => try self.analyzeBreakExpr(expr.position, evals),
            .@"continue" => try self.analyzeContinueExpr(expr.position, evals),
            .@"return" => |*@"return"| try self.analyzeReturnExpr(@"return", expr.position, evals),
            .call => |*call| try self.analyzeCallExpr(call, expr.position, evals),
        };

        return sema_expr;
    }

    fn analyzeLiteralExpr(
        self: *Self,
        literal: *ParsedExpr.Kind.Literal,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        const literal_variant: SemaExpr.Kind.Literal = switch (literal.kind) {
            .unit => .unit,
            .int => |int| .{ .int = int },
            .float => |float| .{ .float = float },
            .bool => |bool_| .{ .bool = bool_ },
            .string => |string| .{ .string = try self.allocator.dupe(u8, string) },
        };

        return SemaExpr.Kind.Literal.create(
            self.allocator,
            literal_variant,
            evals,
            position,
        );
    }

    fn analyzeBinaryExpr(
        self: *Self,
        binary: *ParsedExpr.Kind.Binary,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        var left = try self.analyzeExpr(binary.left, true);

        self.fn_ctx.break_pops += 1;

        var right = try self.analyzeExpr(binary.right, true);

        self.fn_ctx.break_pops -= 1;

        if (left.sema_type == .invalid or right.sema_type == .invalid) {
            return try SemaExpr.Kind.Binary.create(
                self.allocator,
                .add_int, // binary kind doesn't matter here
                left,
                right,
                .invalid,
                evals,
                position,
            );
        }

        const sema_type = switch (binary.kind) {
            .add,
            .subtract,
            .divide,
            .multiply,
            => try self.analyzeArithmeticBinaryExpr(left, right, position),

            .concat,
            => try self.analyzeConcatBinaryExpr(left, right, position),

            .equal,
            .not_equal,
            => try self.analyzeEqualBinaryExpr(left, right, position),

            .greater,
            .greater_equal,
            .less,
            .less_equal,
            => try self.analyzeComparisonBinaryExpr(left, right, position),

            .@"or",
            .@"and",
            => blk: {
                const res = try self.analyzeLogicalBinaryExpr(left, right, position);

                if (!isBranching(left)) {
                    left = try self.wrapInBoolComparison(left);
                }

                if (!isBranching(right)) {
                    right = try self.wrapInBoolComparison(right);
                }

                break :blk res;
            },
        };

        return try SemaExpr.Kind.Binary.create(
            self.allocator,
            translateBinaryKind(left.sema_type, binary.kind),
            left,
            right,
            sema_type,
            evals,
            position,
        );
    }

    fn analyzeArithmeticBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (!typeSatisfies(left.sema_type, .int) and !typeSatisfies(right.sema_type, .float)) {
            return self.semaFailure(
                position,
                .{ .unexpected_arithmetic_type = left.sema_type },
            );
        }

        if (!typeSatisfies(right.sema_type, left.sema_type)) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return left.sema_type;
    }

    fn analyzeConcatBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (!typeSatisfies(left.sema_type, .string) or !typeSatisfies(right.sema_type, .string)) {
            return self.semaFailure(
                position,
                .{ .unexpected_concat_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return left.sema_type;
    }

    fn analyzeEqualBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (!typeSatisfies(right.sema_type, left.sema_type)) {
            return self.semaFailure(
                position,
                .{ .unexpected_equality_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeComparisonBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (!typeSatisfies(left.sema_type, .int) and !typeSatisfies(right.sema_type, .float)) {
            return self.semaFailure(
                position,
                .{ .unexpected_comparison_type = left.sema_type },
            );
        }

        if (!typeSatisfies(right.sema_type, left.sema_type)) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeLogicalBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (!typeSatisfies(left.sema_type, .bool)) {
            return self.semaFailure(
                position,
                .{ .unexpected_logical_type = left.sema_type },
            );
        }

        if (!typeSatisfies(right.sema_type, left.sema_type)) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeUnaryExpr(
        self: *Self,
        unary: *ParsedExpr.Kind.Unary,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        const right = try self.analyzeExpr(unary.right, true);

        if (right.sema_type == .invalid) {
            return try SemaExpr.Kind.Unary.create(
                self.allocator,
                .negate_bool, // unary kind doesn't matter here
                right,
                .invalid,
                evals,
                position,
            );
        }

        const sema_type = switch (unary.kind) {
            .negate_bool,
            => try self.analyzeNegateBoolUnaryExpr(right, position),

            .negate_num,
            => try self.analyzeNegateNumUnaryExpr(right, position),
        };

        return try SemaExpr.Kind.Unary.create(
            self.allocator,
            translateUnaryKind(right.sema_type, unary.kind),
            right,
            sema_type,
            evals,
            position,
        );
    }

    fn analyzeNegateBoolUnaryExpr(
        self: *Self,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (!typeSatisfies(right.sema_type, .bool)) {
            return self.semaFailure(
                position,
                .{ .unexpected_logical_negation_type = right.sema_type },
            );
        }

        return right.sema_type;
    }

    fn analyzeNegateNumUnaryExpr(
        self: *Self,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (!typeSatisfies(right.sema_type, .int) and !typeSatisfies(right.sema_type, .float)) {
            return self.semaFailure(
                position,
                .{ .unexpected_arithmetic_negation_type = right.sema_type },
            );
        }

        return right.sema_type;
    }

    fn analyzeBlockExpr(
        self: *Self,
        block: *ParsedExpr.Kind.Block,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        var scope = FnCtx.Scope.init(self.allocator, self.fn_ctx.current_scope);

        self.fn_ctx.current_scope = &scope;

        var sema_stmts = ArrayList(*SemaStmt).init(self.allocator);
        var is_never = false;

        for (block.stmts.items, 0..) |parser_stmt, index| {
            const should_eval =
                index == block.stmts.items.len - 1 and
                !block.ends_with_semicolon and
                parser_stmt.kind == .expr and
                evals;

            const sema_stmt = self.analyzeStmt(parser_stmt, should_eval) catch |err| switch (err) {
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
            block.ends_with_semicolon))
        {
            try sema_stmts.append(try self.unitStmt(position, evals));
        }

        const sema_type = if (is_never)
            .never
        else
            sema_stmts.getLast().kind.expr.expr.sema_type;

        self.fn_ctx.current_scope = self.fn_ctx.current_scope.prev.?;

        return try SemaExpr.Kind.Block.create(
            self.allocator,
            sema_stmts,
            sema_type,
            evals,
            position,
        );
    }

    fn analyzeVariableExpr(
        self: *Self,
        variable: *ParsedExpr.Kind.Variable,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        const index, const local = self.getVariable(variable.name) orelse {
            return self.semaFailure(
                position,
                .{ .value_not_found = variable.name },
            );
        };

        if (!local.is_assigned) {
            return self.semaFailure(
                position,
                .{ .value_not_assigned = variable.name },
            );
        }

        return try SemaExpr.Kind.Variable.create(
            self.allocator,
            index,
            local.sema_type.?,
            evals,
            position,
        );
    }

    fn analyzeAssignmentExpr(
        self: *Self,
        assignment: *ParsedExpr.Kind.Assigment,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        const index, const local = self.getVariable(assignment.name) orelse {
            return self.semaFailure(
                position,
                .{ .value_not_found = assignment.name },
            );
        };

        if (!local.is_mutable and local.is_assigned) {
            return self.semaFailure(
                position,
                .{ .immutable_mutation = assignment.name },
            );
        }

        const right = try self.analyzeExpr(assignment.right, true);

        self.fn_ctx.locals.items[index].is_assigned = true;

        if (local.sema_type != null and !typeSatisfies(right.sema_type, local.sema_type.?)) {
            return self.semaFailure(
                position,
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
            position,
        );
    }

    fn analyzeIfExpr(
        self: *Self,
        @"if": *ParsedExpr.Kind.If,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        return self.analyzeIfAux(
            @"if".conditional_block,
            @"if".elseif_blocks.items[0..],
            @"if".else_block,
            null,
            position,
            evals,
        );
    }

    fn analyzeIfAux(
        self: *Self,
        conditional_block: ParsedExpr.Kind.If.ConditionalBlock,
        elseif_blocks: []ParsedExpr.Kind.If.ConditionalBlock,
        else_block_opt: ?*ParsedExpr,
        sema_type_opt: ?SemaType,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        if (evals and else_block_opt == null) {
            try conditional_block.block.kind.block.stmts.append(
                try ParsedStmt.Kind.Expr.create(
                    self.allocator,
                    try ParsedExpr.Kind.Literal.create(
                        self.allocator,
                        .unit,
                        position,
                    ),
                    position,
                ),
            );
        }

        const condition = try self.analyzeCondition(conditional_block.condition);
        const then_block = try self.analyzeExpr(conditional_block.block, evals);
        const sema_type = if (sema_type_opt != null and sema_type_opt.? != .never)
            sema_type_opt.?
        else
            then_block.sema_type;

        if (evals and
            sema_type != .never and
            !typeSatisfies(then_block.sema_type, sema_type))
        {
            return self.semaFailure(
                position,
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
                elseif_blocks[0].block.position,
                evals,
            )
        else if (else_block_opt) |else_block|
            try self.analyzeExpr(else_block, evals)
        else
            try SemaExpr.Kind.Block.create(
                self.allocator,
                blk: {
                    var else_block_stmts = ArrayList(*SemaStmt).init(self.allocator);
                    try else_block_stmts.append(try self.unitStmt(position, evals));
                    break :blk else_block_stmts;
                },
                .unit,
                evals,
                position,
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
            position,
        );
    }

    fn analyzeCondition(
        self: *Self,
        expr: *ParsedExpr,
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
        @"for": *ParsedExpr.Kind.For,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        var condition: *SemaExpr = undefined;
        var body_block: *SemaExpr = undefined;

        {
            const prev_pops = self.fn_ctx.break_pops;

            self.fn_ctx.break_pops = 0;
            defer self.fn_ctx.break_pops = prev_pops;

            condition = try self.analyzeCondition(
                if (@"for".condition) |unwrapped|
                    unwrapped
                else
                    try ParsedExpr.Kind.Literal.create(
                        self.allocator,
                        .{ .bool = true },
                        position,
                    ),
            );

            {
                const prev_is_in_loop = self.fn_ctx.is_in_loop;

                self.fn_ctx.is_in_loop = true;
                defer self.fn_ctx.is_in_loop = prev_is_in_loop;

                body_block = try self.analyzeExpr(@"for".body_block, false);
            }
        }

        return try SemaExpr.Kind.For.create(
            self.allocator,
            condition,
            body_block,
            evals,
            position,
        );
    }

    fn analyzeBreakExpr(
        self: *Self,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        if (!self.fn_ctx.is_in_loop) {
            return self.semaFailure(
                position,
                .break_outside_loop,
            );
        }

        return try SemaExpr.Kind.Break.create(
            self.allocator,
            self.fn_ctx.break_pops,
            evals,
            position,
        );
    }

    fn analyzeContinueExpr(
        self: *Self,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        if (!self.fn_ctx.is_in_loop) {
            return self.semaFailure(
                position,
                .continue_outside_loop,
            );
        }

        return try SemaExpr.Kind.Continue.create(
            self.allocator,
            self.fn_ctx.break_pops,
            evals,
            position,
        );
    }

    fn analyzeReturnExpr(
        self: *Self,
        @"return": *const ParsedExpr.Kind.Return,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        const sema_right: *SemaExpr = if (@"return".right) |right|
            self.analyzeExpr(right, true) catch |err| switch (err) {
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
        call: *const ParsedExpr.Kind.Call,
        position: Position,
        evals: bool,
    ) Error!*SemaExpr {
        const callee = try self.analyzeExpr(call.callee, true);

        if (callee.sema_type == .invalid) {
            return SemaExpr.Kind.Call.create(
                self.allocator,
                callee,
                ArrayList(*SemaExpr).init(self.allocator),
                .invalid,
                evals,
                position,
            );
        }

        if (callee.sema_type != .@"fn") {
            return self.semaFailure(position, .not_callable);
        }

        const arg_types = callee.sema_type.@"fn".arg_types;

        if (arg_types.items.len != call.args.items.len) {
            return self.semaFailure(
                position,
                .{
                    .arity_mismatch = .{
                        @intCast(arg_types.items.len),
                        @intCast(call.args.items.len),
                    },
                },
            );
        }

        var sema_args = ArrayList(*SemaExpr).init(self.allocator);

        for (arg_types.items, call.args.items, 1..) |arg_type, arg, index| {
            const arg_expr = try self.analyzeExpr(arg, true);

            if (!typeSatisfies(arg_expr.sema_type, arg_type)) {
                return self.semaFailure(
                    arg.position,
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
            position,
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

    fn unitStmt(self: *Self, position: Position, evals: bool) Error!*SemaStmt {
        return try SemaStmt.Kind.Expr.create(
            self.allocator,
            try self.unitLiteral(position, evals),
            position,
        );
    }

    fn unitLiteral(self: *Self, position: Position, evals: bool) Error!*SemaExpr {
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
        position: Position,
        diag_kind: DiagEntry.Kind,
    ) Error {
        try self.addDiag(position, diag_kind);
        return error.SemaFailure;
    }

    fn addDiag(
        self: *Self,
        position: Position,
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
        binary_kind: ParsedExpr.Kind.Binary.Kind,
    ) SemaExpr.Kind.Binary.Kind {
        return switch (sema_type) {
            .int => switch (binary_kind) {
                .add => .add_int,
                .subtract => .subtract_int,
                .divide => .divide_int,
                .multiply => .multiply_int,
                .equal => .equal_int,
                .not_equal => .not_equal_int,
                .greater => .greater_int,
                .greater_equal => .greater_equal_int,
                .less => .less_int,
                .less_equal => .less_equal_int,

                .concat,
                .@"and",
                .@"or",
                => unreachable,
            },
            .float => switch (binary_kind) {
                .add => .add_float,
                .subtract => .subtract_float,
                .divide => .divide_float,
                .multiply => .multiply_float,
                .equal => .equal_float,
                .not_equal => .not_equal_float,
                .greater => .greater_float,
                .greater_equal => .greater_equal_float,
                .less => .less_float,
                .less_equal => .less_equal_float,

                .concat,
                .@"and",
                .@"or",
                => unreachable,
            },
            .bool => switch (binary_kind) {
                .equal => .equal_bool,
                .not_equal => .not_equal_bool,
                .@"and" => .@"and",
                .@"or" => .@"or",

                .add,
                .subtract,
                .divide,
                .multiply,
                .concat,
                .greater,
                .greater_equal,
                .less,
                .less_equal,
                => unreachable,
            },
            .string => switch (binary_kind) {
                .concat => .concat,
                .equal => .equal_obj,
                .not_equal => .not_equal_obj,

                .add,
                .subtract,
                .divide,
                .multiply,
                .greater,
                .greater_equal,
                .less,
                .less_equal,
                .@"and",
                .@"or",
                => unreachable,
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
        unary_kind: ParsedExpr.Kind.Unary.Kind,
    ) SemaExpr.Kind.Unary.Kind {
        return switch (sema_type) {
            .int => switch (unary_kind) {
                .negate_num,
                => .negate_int,

                .negate_bool,
                => unreachable,
            },
            .float => switch (unary_kind) {
                .negate_num,
                => .negate_float,

                .negate_bool,
                => unreachable,
            },
            .bool => switch (unary_kind) {
                .negate_bool,
                => .negate_bool,

                .negate_num,
                => unreachable,
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
    var parser = Parser.init(&arena_allocator);
    const parsed_expr = try parser.parse(&tokenizer, null);

    // WHEN - THEN
    var sema = Sema.init(&arena_allocator);
    _ = try sema.analyze(parsed_expr, null);
}

test "should free all memory on unsuccessful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    var arena_allocator = ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    const source = "print (2 + 2) * -false";
    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(&arena_allocator);

    const parsed_expr = try parser.parse(&tokenizer, null);

    // WHEN - THEN
    var sema = Sema.init(&arena_allocator);

    const result = sema.analyze(parsed_expr, null);
    try expectError(Sema.Error.SemaFailure, result);
}
