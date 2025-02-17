const std = @import("std");
const shared = @import("shared");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_mod = @import("../sema/sema.zig");
const sema_ast_mod = @import("../sema/sema_ast.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("../state/obj.zig");
const tokenizer_mod = @import("../tokenizer.zig");
const limits = @import("../limits.zig");

const mem = std.mem;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const BoundedArray = std.BoundedArray;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const assert = std.debug.assert;
const meta = shared.meta;
const SharedDiags = shared.Diags;
const Writer = shared.Writer;
const CallFrame = managed_memory_mod.CallFrame;
const VmState = managed_memory_mod.VmState;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Sema = sema_mod.Sema;
const SemaExpr = sema_ast_mod.SemaExpr;
const SemaStmt = sema_ast_mod.SemaStmt;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;
const Obj = obj_mod.Obj;
const Loc = tokenizer_mod.Loc;

pub const Compiler = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        CompileFailure,
    };

    pub const DiagEntry = struct {
        pub const Kind = enum {
            too_many_constants,
            jump_too_big,
        };

        kind: Kind,
        position: Loc,
    };

    pub const Diags = SharedDiags(DiagEntry);

    const ExprCtx = struct {
        const JumpInfo = struct {
            index: usize,
            is_inverted: bool,
        };

        const ConditionalBlocks = struct {
            then: *SemaExpr,
            @"else": ?*SemaExpr = null,
            is_loop: bool = false,
        };

        is_root: bool = false,
        is_child_to_logical: bool = false,
        is_current_logical_or: bool = false,
        last_jump: *?JumpInfo,
        else_branch_offsets: *ArrayList(usize),
        then_branch_offsets: *ArrayList(usize),
        conditional_blocks: ?ConditionalBlocks = null,
    };

    const FnCtx = struct {
        pub const FnKind = enum {
            @"fn",
            script,
        };

        fn_kind: FnKind,
        @"fn": *Obj.Fn,
        break_jumps: *ArrayList(usize),
        loop_start: usize,
    };

    managed_allocator: Allocator,
    unmanaged_allocator: Allocator,
    diags: ?*Diags,
    vm_state: *VmState,
    fn_ctx: FnCtx,

    pub fn compile(
        memory: *ManagedMemory,
        arena_allocator: *ArenaAllocator,
        sema_fn: *const SemaStmt.Kind.Fn,
        diags: ?*Diags,
    ) Error!void {
        const managed_allocator = memory.allocator();
        const unmanaged_allocator = arena_allocator.allocator();

        var vm_state: VmState = .{
            .@"fn" = undefined,
            .stack = null,
            .objs = null,
            .strings = StringHashMap(*Obj.String).init(managed_allocator),
        };
        errdefer vm_state.deinit(managed_allocator);

        var compiler = Self{
            .managed_allocator = managed_allocator,
            .unmanaged_allocator = unmanaged_allocator,
            .diags = diags,
            .vm_state = &vm_state,
            .fn_ctx = undefined,
        };

        vm_state.@"fn" = try compiler.compileFnAux(
            &vm_state,
            null,
            .script,
            sema_fn.body,
            sema_fn.locals_count,
        );

        memory.vm_state = vm_state;
    }

    fn compileStmt(
        self: *Self,
        stmt: *const SemaStmt,
    ) Error!void {
        switch (stmt.kind) {
            .assert => |*assert_stmt| try self.compileAssertStmt(assert_stmt, stmt.position),
            .print => |*print| try self.compilePrintStmt(print, stmt.position),
            .expr => |*expr| try self.compileExprStmt(expr),
            .let => |*let| if (let.expr) |expr| try self.compileVariableMutation(let.index, expr, stmt.position),
            .@"fn" => |*@"fn"| try self.compileFn(@"fn", stmt.position),
        }
    }

    fn compileAssertStmt(
        self: *Self,
        assert_stmt: *const SemaStmt.Kind.Assert,
        position: Loc,
    ) Error!void {
        try self.compileExpr(assert_stmt.expr, null, null);
        try self.writeU8(.assert, position);
    }

    fn compilePrintStmt(
        self: *Self,
        print: *const SemaStmt.Kind.Print,
        position: Loc,
    ) Error!void {
        try self.compileExpr(print.expr, null, null);
        try self.writeU8(.print, position);
    }

    fn compileExprStmt(
        self: *Self,
        expr: *const SemaStmt.Kind.Expr,
    ) Error!void {
        try self.compileExpr(expr.expr, null, null);
    }

    fn compileVariableMutation(
        self: *Self,
        index: usize,
        expr_opt: ?*const SemaExpr,
        position: Loc,
    ) Error!void {
        const index_u8: u8 = @intCast(index);

        if (expr_opt) |expr| {
            try self.compileExpr(expr, null, null);
        }

        switch (index_u8) {
            0 => try self.writeU8(.store_local_0, position),
            1 => try self.writeU8(.store_local_1, position),
            2 => try self.writeU8(.store_local_2, position),
            3 => try self.writeU8(.store_local_3, position),
            4 => try self.writeU8(.store_local_4, position),
            else => {
                try self.writeU8(.store_local, position);
                try self.writeU8(index_u8, position);
            },
        }
    }

    fn compileFn(
        self: *Self,
        @"fn": *const SemaStmt.Kind.Fn,
        position: Loc,
    ) Error!void {
        const fn_obj = try self.compileFnAux(
            self.vm_state,
            @"fn".name,
            .@"fn",
            @"fn".body,
            @"fn".locals_count,
        );

        if (@"fn".index) |index| {
            try self.writeConstant(.{ .obj = &fn_obj.obj }, position);
            try self.compileVariableMutation(index, null, position);
        }
    }

    fn compileFnAux(
        self: *Self,
        vm_state: *VmState,
        name_opt: ?[]const u8,
        fn_kind: FnCtx.FnKind,
        block: *const SemaExpr,
        locals_count: u8,
    ) Error!*Obj.Fn {
        const @"fn" = try Obj.Fn.create(
            self.managed_allocator,
            vm_state,
            if (name_opt) |name|
                try Obj.String.createFromCopied(
                    self.managed_allocator,
                    self.vm_state,
                    name,
                )
            else
                null,
            locals_count,
        );

        {
            const prev_fn_ctx = self.fn_ctx;

            self.fn_ctx = .{
                .fn_kind = fn_kind,
                .@"fn" = @"fn",
                .break_jumps = undefined,
                .loop_start = undefined,
            };
            defer self.fn_ctx = prev_fn_ctx;

            try self.compileExpr(block, null, null);
            try self.writeU8(.constant_unit, .{ .start = 0, .end = 0 });
            try self.writeU8(.@"return", .{ .start = 0, .end = 0 });
        }

        return @"fn";
    }

    fn compileExpr(
        self: *Self,
        expr: *const SemaExpr,
        ctx_opt: ?ExprCtx,
        ctx_override: anytype,
    ) Error!void {
        var last_jump: ?ExprCtx.JumpInfo = null;
        var else_branch_offsets_opt: ?ArrayList(usize) = null;
        var then_branch_offsets_opt: ?ArrayList(usize) = null;
        var ctx = if (ctx_opt) |passed_ctx| passed_ctx else blk: {
            else_branch_offsets_opt = ArrayList(usize).init(self.unmanaged_allocator);
            then_branch_offsets_opt = ArrayList(usize).init(self.unmanaged_allocator);

            break :blk ExprCtx{
                .is_root = true,
                .last_jump = &last_jump,
                .else_branch_offsets = &else_branch_offsets_opt.?,
                .then_branch_offsets = &then_branch_offsets_opt.?,
            };
        };

        if (@TypeOf(ctx_override) != @TypeOf(null)) {
            ctx = meta.spread(ctx, ctx_override);
        }

        switch (expr.kind) {
            .literal => |*literal| try self.compileLiteralExpr(literal, expr),
            .binary => |*binary| try self.compileBinaryExpr(binary, expr, ctx),
            .unary => |*unary| try self.compileUnaryExpr(unary, expr),
            .block => |*block| try self.compileBlockExpr(block),
            .variable => |*variable| try self.compileVariableExpr(variable, expr),
            .assignment => |*assignment| try self.compileAssignmentExpr(assignment, expr),
            .@"if" => |*@"if"| try self.compileIfExpr(@"if"),
            .@"for" => |*@"for"| try self.compileForExpr(@"for", expr),
            .@"break" => |*@"break"| try self.compileBreakExpr(@"break", expr),
            .@"continue" => |*@"continue"| try self.compileContinueExpr(@"continue", expr),
            .@"return" => |*@"return"| try self.compileReturnExpr(@"return", expr),
            .call => |*call| try self.compileCallExpr(call, expr),
        }
    }

    fn compileLiteralExpr(
        self: *Self,
        literal: *const SemaExpr.Kind.Literal,
        expr: *const SemaExpr,
    ) Error!void {
        if (!expr.evals) {
            return;
        }

        switch (literal.*) {
            .unit => try self.writeU8(.constant_unit, expr.position),
            .int => |int| switch (int) {
                -1 => try self.writeU8(.constant_int_n1, expr.position),
                0 => try self.writeU8(.constant_int_0, expr.position),
                1 => try self.writeU8(.constant_int_1, expr.position),
                2 => try self.writeU8(.constant_int_2, expr.position),
                3 => try self.writeU8(.constant_int_3, expr.position),
                4 => try self.writeU8(.constant_int_4, expr.position),
                5 => try self.writeU8(.constant_int_5, expr.position),
                else => try self.writeConstant(.{ .int = int }, expr.position),
            },
            .float => |float| {
                if (float == 0) {
                    try self.writeU8(.constant_float_0, expr.position);
                } else if (float == 1) {
                    try self.writeU8(.constant_float_1, expr.position);
                } else if (float == 2) {
                    try self.writeU8(.constant_float_2, expr.position);
                } else {
                    try self.writeConstant(.{ .float = float }, expr.position);
                }
            },
            .bool => |bool_| switch (bool_) {
                true => try self.writeU8(.constant_bool_true, expr.position),
                false => try self.writeU8(.constant_bool_false, expr.position),
            },
            .string => |string| try self.writeConstant(
                .{
                    .obj = &(try Obj.String.createFromCopied(
                        self.managed_allocator,
                        self.vm_state,
                        string,
                    )).obj,
                },
                expr.position,
            ),
        }
    }

    fn compileBinaryExpr(
        self: *Self,
        binary: *const SemaExpr.Kind.Binary,
        expr: *const SemaExpr,
        ctx: ExprCtx,
    ) Error!void {
        switch (binary.kind) {
            .add_int,
            .add_float,
            .subtract_int,
            .subtract_float,
            .multiply_int,
            .multiply_float,
            .divide_int,
            .divide_float,
            .concat,
            => try self.compileArithmeticBinaryExpr(binary, expr.position),

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
            => try self.compileComparisonBinaryExpr(binary, expr.position, ctx),

            .@"or",
            .@"and",
            => try self.compileLogicalBinaryExpr(binary, expr.position, ctx),
        }

        if (!expr.evals) {
            try self.writeU8(.pop, expr.position);
        }
    }

    fn compileComparisonBinaryExpr(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Loc,
        ctx: ExprCtx,
    ) Error!void {
        try self.compileExpr(expr.left, null, null);

        if (expr.right.kind != .literal or expr.right.kind.literal != .bool) {
            try self.compileExpr(expr.right, null, null);
        }

        const cmp_op_code, const if_op_code = getComparisonOpCodes(expr);
        const offset, const is_inverted = try self.compileCondition(
            cmp_op_code,
            if_op_code,
            position,
            ctx,
        );

        if (!ctx.is_child_to_logical) {
            try self.compileConditionalBranches(offset, position, ctx);
        } else if (is_inverted) {
            try ctx.then_branch_offsets.append(offset);
        } else {
            try ctx.else_branch_offsets.append(offset);
        }
    }

    fn compileLogicalBinaryExpr(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Loc,
        ctx: ExprCtx,
    ) Error!void {
        const is_current_logical_or = expr.kind == .@"or";
        var new_else_branch_offsets_opt: ?ArrayList(usize) = null;
        var new_then_branch_offsets_opt: ?ArrayList(usize) = null;
        var else_branch_offsets = ctx.else_branch_offsets;
        var then_branch_offsets = ctx.then_branch_offsets;

        if (expr.kind == .@"or") {
            new_else_branch_offsets_opt = ArrayList(usize).init(self.unmanaged_allocator);
            else_branch_offsets = &new_else_branch_offsets_opt.?;
        } else {
            new_then_branch_offsets_opt = ArrayList(usize).init(self.unmanaged_allocator);
            then_branch_offsets = &new_then_branch_offsets_opt.?;
        }

        const left_ctx = meta.spread(ctx, .{
            .is_root = false,
            .is_child_to_logical = true,
            .is_current_logical_or = is_current_logical_or,
            .else_branch_offsets = else_branch_offsets,
            .then_branch_offsets = then_branch_offsets,
        });

        try self.compileExpr(expr.left, left_ctx, null);

        if (expr.kind == .@"or") {
            if (ctx.last_jump.* != null and !ctx.last_jump.*.?.is_inverted) {
                try self.invertLastBranchJump(left_ctx);
            }

            try self.patchJumps(else_branch_offsets);
        } else {
            if (ctx.last_jump.* != null and ctx.last_jump.*.?.is_inverted) {
                try self.invertLastBranchJump(left_ctx);
            }

            try self.patchJumps(then_branch_offsets);
        }

        try self.compileExpr(expr.right, left_ctx, .{
            .else_branch_offsets = ctx.else_branch_offsets,
            .then_branch_offsets = ctx.then_branch_offsets,
        });

        if (ctx.is_root) {
            // if the deepest branch condition was inverted, invert it back
            // as it is the end of the logical expression
            if (ctx.last_jump.* != null and ctx.last_jump.*.?.is_inverted) {
                try self.invertLastBranchJump(ctx);
            }

            try self.patchJumps(ctx.then_branch_offsets);
            try self.compileConditionalBranches(ctx.else_branch_offsets, position, ctx);
        }
    }

    fn compileArithmeticBinaryExpr(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Loc,
    ) Error!void {
        try self.compileExpr(expr.left, null, null);
        try self.compileExpr(expr.right, null, null);

        const op_code: OpCode = switch (expr.kind) {
            .add_int => .add_int,
            .add_float => .add_float,
            .subtract_int => .subtract_int,
            .subtract_float => .subtract_float,
            .multiply_int => .multiply_int,
            .multiply_float => .multiply_float,
            .divide_int => .divide_int,
            .divide_float => .divide_float,
            .concat => .concat,
            else => unreachable,
        };

        try self.writeU8(op_code, position);
    }

    fn compileUnaryExpr(
        self: *Self,
        unary: *const SemaExpr.Kind.Unary,
        expr: *const SemaExpr,
    ) Error!void {
        try self.compileExpr(unary.right, null, null);
        try self.writeU8(
            switch (unary.kind) {
                .negate_bool => OpCode.negate_bool,
                .negate_int => OpCode.negate_int,
                .negate_float => OpCode.negate_float,
            },
            expr.position,
        );

        if (!expr.evals) {
            try self.writeU8(.pop, expr.position);
        }
    }

    fn compileBlockExpr(
        self: *Self,
        block: *const SemaExpr.Kind.Block,
    ) Error!void {
        for (block.stmts.items) |child_stmt| {
            try self.compileStmt(child_stmt);
        }
    }

    fn compileVariableExpr(
        self: *Self,
        variable: *const SemaExpr.Kind.Variable,
        expr: *const SemaExpr,
    ) Error!void {
        if (!expr.evals) {
            return;
        }

        const index: u8 = @intCast(variable.index);

        switch (index) {
            0 => try self.writeU8(.load_local_0, expr.position),
            1 => try self.writeU8(.load_local_1, expr.position),
            2 => try self.writeU8(.load_local_2, expr.position),
            3 => try self.writeU8(.load_local_3, expr.position),
            4 => try self.writeU8(.load_local_4, expr.position),
            else => {
                try self.writeU8(.load_local, expr.position);
                try self.writeU8(index, expr.position);
            },
        }
    }

    fn compileAssignmentExpr(
        self: *Self,
        assignment: *const SemaExpr.Kind.Assignment,
        expr: *const SemaExpr,
    ) Error!void {
        try self.compileVariableMutation(
            assignment.index,
            assignment.right,
            expr.position,
        );

        if (expr.evals) {
            try self.writeU8(.constant_unit, expr.position);
        }
    }

    fn compileIfExpr(
        self: *Self,
        @"if": *const SemaExpr.Kind.If,
    ) Error!void {
        try self.compileExpr(@"if".condition, null, .{
            .conditional_blocks = ExprCtx.ConditionalBlocks{
                .then = @"if".then_block,
                .@"else" = @"if".else_block,
            },
        });
    }

    fn compileForExpr(
        self: *Self,
        @"for": *const SemaExpr.Kind.For,
        expr: *const SemaExpr,
    ) Error!void {
        var current_break_jumps = ArrayList(usize).init(self.unmanaged_allocator);

        {
            const prev_break_jumps = self.fn_ctx.break_jumps;

            self.fn_ctx.break_jumps = &current_break_jumps;
            defer self.fn_ctx.break_jumps = prev_break_jumps;

            const prev_loop_start = self.fn_ctx.loop_start;

            self.fn_ctx.loop_start = self.fn_ctx.@"fn".chunk.code.items.len;
            defer self.fn_ctx.loop_start = prev_loop_start;

            try self.compileExpr(@"for".condition, null, .{
                .conditional_blocks = ExprCtx.ConditionalBlocks{
                    .then = @"for".body_block,
                    .is_loop = true,
                },
            });

            try self.patchJumps(current_break_jumps);
        }

        if (expr.evals) {
            try self.writeU8(.constant_unit, expr.position);
        }
    }

    fn compileBreakExpr(
        self: *Self,
        @"break": *const SemaExpr.Kind.Break,
        expr: *const SemaExpr,
    ) Error!void {
        for (0..@"break".pops) |_| {
            _ = try self.writeU8(.pop, expr.position);
        }

        const offset = try self.writeJump(.jump, expr.position);
        try self.fn_ctx.break_jumps.append(offset);
    }

    fn compileContinueExpr(
        self: *Self,
        @"continue": *const SemaExpr.Kind.Continue,
        expr: *const SemaExpr,
    ) Error!void {
        for (0..@"continue".pops) |_| {
            _ = try self.writeU8(.pop, expr.position);
        }

        try self.writeNegativeJump(self.fn_ctx.loop_start, expr.position);
    }

    fn compileReturnExpr(
        self: *Self,
        @"return": *const SemaExpr.Kind.Return,
        expr: *const SemaExpr,
    ) Error!void {
        try self.compileExpr(@"return".right, null, null);
        try self.writeU8(.@"return", expr.position);
    }

    fn compileCallExpr(
        self: *Self,
        call: *const SemaExpr.Kind.Call,
        expr: *const SemaExpr,
    ) Error!void {
        try self.compileExpr(call.callee, null, null);

        for (call.args.items) |arg| {
            try self.compileExpr(arg, null, null);
        }

        _ = try self.writeU8(.call, expr.position);
        _ = try self.writeU8(
            @as(u8, @intCast(call.args.items.len)),
            expr.position,
        );

        if (!expr.evals) {
            _ = try self.writeU8(.pop, expr.position);
        }
    }

    fn compileCondition(
        self: *Self,
        cmp_op_code_opt: ?OpCode,
        if_op_code: OpCode,
        position: Loc,
        ctx: ExprCtx,
    ) Error!struct { usize, bool } {
        const invert = ctx.is_current_logical_or and ctx.is_child_to_logical;
        const final_if_op_code = if (invert)
            invertComparisonOpCode(if_op_code)
        else
            if_op_code;

        if (cmp_op_code_opt) |cmp_op_code| {
            try self.writeU8(cmp_op_code, position);
        }

        const offset = try self.writeJump(final_if_op_code, position);
        ctx.last_jump.* = .{ .index = offset - 1, .is_inverted = invert };

        return .{ offset, invert };
    }

    fn compileConditionalBranches(
        self: *Self,
        else_offset: anytype,
        position: Loc,
        ctx: ExprCtx,
    ) Error!void {
        var then_evals_to_never = false;

        if (ctx.conditional_blocks) |blocks| {
            try self.compileExpr(blocks.then, null, null);

            then_evals_to_never = blocks.then.sema_type == .never;
        } else {
            try self.writeU8(.constant_bool_true, position);
        }

        if (ctx.conditional_blocks != null and
            ctx.conditional_blocks.?.@"else" == null)
        {
            if (ctx.conditional_blocks.?.is_loop) {
                try self.writeNegativeJump(self.fn_ctx.loop_start, position);
            }

            try self.patchJumps(else_offset);
            return;
        }

        var then_offset: usize = undefined;

        if (!then_evals_to_never) {
            then_offset = try self.writeJump(.jump, position);
        }

        try self.patchJumps(else_offset);

        if (ctx.conditional_blocks) |blocks| {
            try self.compileExpr(blocks.@"else".?, null, null);
        } else {
            try self.writeU8(.constant_bool_false, position);
        }

        if (!then_evals_to_never) {
            try self.patchJumps(then_offset);
        }
    }

    fn invertComparisonOpCode(op_code: OpCode) OpCode {
        return switch (op_code) {
            .if_equal => .if_not_equal,
            .if_not_equal => .if_equal,
            .if_greater => .if_less_equal,
            .if_greater_equal => .if_less,
            .if_less => .if_greater_equal,
            .if_less_equal => .if_greater,
            .if_true => .if_false,
            .if_false => .if_true,
            else => @panic("non-comparison opcode provided"),
        };
    }

    fn getComparisonOpCodes(
        expr: *const SemaExpr.Kind.Binary,
    ) struct { ?OpCode, OpCode } {
        return switch (expr.kind) {
            .equal_int => .{ .compare_int, .if_not_equal },
            .equal_float => .{ .compare_float, .if_not_equal },
            .equal_bool =>
            // zig fmt: off
                if (expr.right.kind == .literal and expr.right.kind.literal == .bool)
                    if (expr.right.kind.literal.bool)
                        .{ null, .if_false }
                    else
                        .{ null, .if_true }
                else
                    .{ .compare_bool, .if_not_equal },
            // zig fmt: on
            .equal_obj => .{ .compare_obj, .if_not_equal },

            .not_equal_int => .{ .compare_int, .if_equal },
            .not_equal_float => .{ .compare_float, .if_equal },
            .not_equal_bool => .{ .compare_bool, .if_equal },
            .not_equal_obj => .{ .compare_obj, .if_equal },

            .greater_int => .{ .compare_int, .if_less_equal },
            .greater_float => .{ .compare_float, .if_less_equal },

            .greater_equal_int => .{ .compare_int, .if_less },
            .greater_equal_float => .{ .compare_float, .if_less },

            .less_int => .{ .compare_int, .if_greater_equal },
            .less_float => .{ .compare_float, .if_greater_equal },

            .less_equal_int => .{ .compare_int, .if_greater },
            .less_equal_float => .{ .compare_float, .if_greater },

            else => unreachable,
        };
    }

    fn invertLastBranchJump(
        self: *Self,
        ctx: ExprCtx,
    ) Error!void {
        const last_jump = ctx.last_jump.*.?;
        const if_op_code: OpCode = @enumFromInt(self.getChunk().code.items[last_jump.index]);

        try self.getChunk().updateU8(invertComparisonOpCode(if_op_code), last_jump.index);

        if (last_jump.is_inverted) {
            const offset = ctx.then_branch_offsets.pop().?;
            try ctx.else_branch_offsets.append(offset);
        } else {
            const offset = ctx.else_branch_offsets.pop().?;
            try ctx.then_branch_offsets.append(offset);
        }
    }

    fn getChunk(self: *Self) *Chunk {
        return &self.fn_ctx.@"fn".chunk;
    }

    fn writeU8(self: *Self, data: anytype, position: Loc) Error!void {
        try self.getChunk().writeU8(data, position);
    }

    fn writeJump(self: *Self, op_code: OpCode, position: Loc) Error!usize {
        return try self.getChunk().writeJump(op_code, position);
    }

    fn patchJumps(self: *Self, arg: anytype) Error!void {
        const Type = @TypeOf(arg);
        const type_info = @typeInfo(Type);
        const ChildType = if (type_info == .pointer)
            type_info.pointer.child
        else
            Type;

        const jumps = if (Type == usize)
            [_]usize{arg}
        else if (comptime meta.isArrayList(ChildType))
            arg.items
        else
            arg.buffer[0..arg.len];

        for (jumps) |jump| {
            self.getChunk().patchJump(jump) catch |err| switch (err) {
                error.JumpTooBig => {
                    try self.addDiag(.jump_too_big, self.getChunk().positions.items[jump]);
                    return error.CompileFailure;
                },
            };
        }
    }

    fn writeNegativeJump(
        self: *Self,
        offset: usize,
        position: Loc,
    ) Error!void {
        _ = self.getChunk().writeNegativeJump(
            offset,
            position,
        ) catch |err| switch (err) {
            error.JumpTooBig => {
                try self.addDiag(.jump_too_big, position);
                return error.CompileFailure;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
    }

    fn writeConstant(self: *Self, value: Value, position: Loc) Error!void {
        _ = self.getChunk().writeConstant(
            value,
            position,
        ) catch |err| switch (err) {
            error.TooManyConstants => {
                try self.addDiag(.too_many_constants, position);
                return error.CompileFailure;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
    }

    fn addDiag(
        self: *Self,
        diag_kind: DiagEntry.Kind,
        position: Loc,
    ) Error!void {
        if (self.diags) |diags| {
            // in case of ever needing to alloc something in here, make sure to
            // use diags.allocator instead of self.allocator. this is
            // necessary for lang-tests where a new allocator is created for
            // each test to detect memory leaks. that allocator then gets
            // deinited while diags are owned by the tests.
            try diags.add(.{
                .kind = diag_kind,
                .position = position,
            });
        }
    }
};
