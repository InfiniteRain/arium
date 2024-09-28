const std = @import("std");
const shared = @import("shared");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_ast_mod = @import("../sema/sema_ast.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("../state/obj.zig");
const hash_table_mod = @import("../state/hash_table.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const mem = std.mem;
const Allocator = mem.Allocator;
const BoundedArray = std.BoundedArray;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const meta = shared.meta;
const SharedDiags = shared.Diags;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const SemaExpr = sema_ast_mod.SemaExpr;
const SemaStmt = sema_ast_mod.SemaStmt;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;
const Obj = obj_mod.Obj;
const HashTable = hash_table_mod.HashTable;
const Position = tokenizer_mod.Position;

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
        position: Position,
    };

    pub const Diags = SharedDiags(DiagEntry);

    const ExprCtx = struct {
        const JumpInfo = struct {
            index: usize,
            is_inverted: bool,
        };

        is_root: bool = false,
        is_child_to_logical: bool = false,
        is_current_logical_or: bool = false,
        last_jump: *?JumpInfo,
        else_branch_offsets: *ArrayList(usize),
        then_branch_offsets: *ArrayList(usize),
        conditional_blocks: ?struct {
            then: *SemaExpr,
            @"else": ?*SemaExpr = null,
            loop_start: ?usize = null,
        } = null,
    };

    vm_state: *VmState,
    managed_allocator: Allocator,
    unmanaged_allocator: Allocator,
    chunk: Chunk,
    diags: ?*Diags,
    break_jumps: *ArrayList(usize),

    pub fn compile(
        memory: *ManagedMemory,
        block: *const SemaExpr,
        diags: ?*Diags,
    ) Error!void {
        const managed_allocator = memory.allocator();

        var vm_state: VmState = undefined;

        vm_state.objs = null;
        vm_state.strings = try HashTable.init(managed_allocator);
        vm_state.stack = try Stack.init(managed_allocator);

        var compiler = Self{
            .vm_state = &vm_state,
            .managed_allocator = managed_allocator,
            .unmanaged_allocator = memory.backing_allocator,
            .chunk = try Chunk.init(managed_allocator),
            .diags = diags,
            .break_jumps = undefined,
        };

        errdefer {
            compiler.chunk.deinit();
            vm_state.stack.deinit();
            vm_state.strings.deinit();

            var current = vm_state.objs;

            while (current) |obj| {
                const next = obj.next;
                obj.destroy(managed_allocator);
                current = next;
            }
        }

        try compiler.compileExpr(block, null, null);
        try compiler.writeU8(.@"return", .{ .line = 0, .column = 0 });

        vm_state.chunk = compiler.chunk;
        vm_state.ip = @ptrCast(&compiler.chunk.code.items[0]);

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
        }
    }

    fn compileAssertStmt(
        self: *Self,
        assert_stmt: *const SemaStmt.Kind.Assert,
        position: Position,
    ) Error!void {
        try self.compileExpr(assert_stmt.expr, null, null);
        try self.writeU8(.assert, position);
    }

    fn compilePrintStmt(
        self: *Self,
        print: *const SemaStmt.Kind.Print,
        position: Position,
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
        expr: *const SemaExpr,
        position: Position,
    ) Error!void {
        const index_u8: u8 = @intCast(index);

        try self.compileExpr(expr, null, null);

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

    fn compileExpr(
        self: *Self,
        expr: *const SemaExpr,
        ctx_opt: ?ExprCtx,
        ctx_override: anytype,
    ) Error!void {
        var is_branching = false;
        var last_jump: ?ExprCtx.JumpInfo = null;

        var else_branch_offsets_opt: ?ArrayList(usize) = null;
        defer if (else_branch_offsets_opt) |*branches| branches.clearAndFree();

        var then_branch_offsets_opt: ?ArrayList(usize) = null;
        defer if (then_branch_offsets_opt) |*branches| branches.clearAndFree();

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
            .binary => |*binary| {
                is_branching = try self.compileBinaryExpr(binary, expr, ctx);
            },
            .unary => |*unary| try self.compileUnaryExpr(unary, expr),
            .block => |*block| try self.compileBlockExpr(block),
            .variable => |*variable| try self.compileVariableExpr(variable, expr),
            .assignment => |*assignment| try self.compileAssignmentExpr(assignment, expr),
            .@"if" => |*@"if"| try self.compileIfExpr(@"if"),
            .@"for" => |*@"for"| try self.compileForExpr(@"for", expr),
            .@"break" => |*@"break"| try self.compileBreakExpr(@"break", expr),
        }

        if (!is_branching and ctx.is_child_to_logical) {
            const cmp_op_code = .compare_bool;
            const if_op_code = .if_not_equal;

            try self.writeU8(.constant_bool_true, expr.position);
            const offset, const is_inverted = try self.compileCondition(
                cmp_op_code,
                if_op_code,
                expr.position,
                ctx,
            );

            if (is_inverted) {
                try ctx.then_branch_offsets.append(offset);
            } else {
                try ctx.else_branch_offsets.append(offset);
            }
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
    ) Error!bool {
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
            => {
                try self.compileArithmeticBinaryExpr(binary, expr.position);
                return false;
            },

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
            => {
                try self.compileComparisonBinaryExpr(binary, expr.position, ctx);
                return true;
            },

            .@"or",
            .@"and",
            => {
                try self.compileLogicalBinaryExpr(binary, expr.position, ctx);
                return true;
            },
        }

        if (!expr.evals) {
            try self.writeU8(.pop, expr.position);
        }
    }

    fn compileComparisonBinaryExpr(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
        ctx: ExprCtx,
    ) Error!void {
        try self.compileExpr(expr.left, null, null);
        try self.compileExpr(expr.right, null, null);

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
        position: Position,
        ctx: ExprCtx,
    ) Error!void {
        const is_current_logical_or = expr.kind == .@"or";

        var new_else_branch_offsets_opt: ?ArrayList(usize) = null;
        defer if (new_else_branch_offsets_opt) |*branches| branches.deinit();

        var new_then_branch_offsets_opt: ?ArrayList(usize) = null;
        defer if (new_then_branch_offsets_opt) |*branches| branches.deinit();

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
        position: Position,
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
            .conditional_blocks = .{
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
        const last_break_jumps = self.break_jumps;

        var current_break_jumps = ArrayList(usize).init(self.unmanaged_allocator);
        defer current_break_jumps.clearAndFree();

        self.break_jumps = &current_break_jumps;

        try self.compileExpr(@"for".condition, null, .{
            .conditional_blocks = .{
                .then = @"for".body_block,
                .loop_start = self.chunk.code.items.len,
            },
        });

        try self.patchJumps(current_break_jumps);

        self.break_jumps = last_break_jumps;

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
        try self.break_jumps.append(offset);
    }

    fn compileCondition(
        self: *Self,
        cmp_op_code: OpCode,
        if_op_code: OpCode,
        position: Position,
        ctx: ExprCtx,
    ) Error!struct { usize, bool } {
        const invert = ctx.is_current_logical_or and ctx.is_child_to_logical;
        const final_if_op_code = if (invert)
            invertComparisonOpCode(if_op_code)
        else
            if_op_code;
        try self.writeU8(cmp_op_code, position);
        const offset = try self.writeJump(final_if_op_code, position);
        ctx.last_jump.* = .{ .index = offset - 1, .is_inverted = invert };

        return .{ offset, invert };
    }

    fn compileConditionalBranches(
        self: *Self,
        else_offset: anytype,
        position: Position,
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
            if (ctx.conditional_blocks.?.loop_start) |loop_start| {
                try self.writeNegativeJump(loop_start, position);
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
            else => @panic("non-comparison opcode provided"),
        };
    }

    fn getComparisonOpCodes(
        expr: *const SemaExpr.Kind.Binary,
    ) struct { OpCode, OpCode } {
        const cmp_op_code: OpCode, const if_op_code: OpCode = switch (expr.kind) {
            .equal_int => .{ .compare_int, .if_not_equal },
            .equal_float => .{ .compare_float, .if_not_equal },
            .equal_bool => .{ .compare_bool, .if_not_equal },
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

        return .{ cmp_op_code, if_op_code };
    }

    fn invertLastBranchJump(
        self: *Self,
        ctx: ExprCtx,
    ) Error!void {
        const last_jump = ctx.last_jump.*.?;
        const if_op_code: OpCode = @enumFromInt(self.chunk.code.items[last_jump.index]);

        try self.chunk.updateU8(invertComparisonOpCode(if_op_code), last_jump.index);

        if (last_jump.is_inverted) {
            const offset = ctx.then_branch_offsets.pop();
            try ctx.else_branch_offsets.append(offset);
        } else {
            const offset = ctx.else_branch_offsets.pop();
            try ctx.then_branch_offsets.append(offset);
        }
    }

    fn writeU8(self: *Self, data: anytype, position: Position) Error!void {
        try self.chunk.writeU8(data, position);
    }

    fn writeJump(self: *Self, op_code: OpCode, position: Position) Error!usize {
        return try self.chunk.writeJump(op_code, position);
    }

    fn patchJumps(self: *Self, arg: anytype) Error!void {
        const Type = @TypeOf(arg);
        const type_info = @typeInfo(Type);
        const ChildType = if (type_info == .Pointer)
            type_info.Pointer.child
        else
            Type;

        const jumps = if (Type == usize)
            [_]usize{arg}
        else if (comptime meta.isArrayList(ChildType))
            arg.items
        else
            arg.buffer[0..arg.len];

        for (jumps) |jump| {
            self.chunk.patchJump(jump) catch |err| switch (err) {
                error.JumpTooBig => {
                    try self.addDiag(.jump_too_big, self.chunk.positions.items[jump]);
                    return error.CompileFailure;
                },
            };
        }
    }

    fn writeNegativeJump(
        self: *Self,
        offset: usize,
        position: Position,
    ) Error!void {
        _ = self.chunk.writeNegativeJump(
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

    fn writeConstant(self: *Self, value: Value, position: Position) Error!void {
        _ = self.chunk.writeConstant(
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
        position: Position,
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
