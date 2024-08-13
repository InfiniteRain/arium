const std = @import("std");
const shared = @import("shared");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_expr_mod = @import("../sema/sema_expr.zig");
const sema_stmt_mod = @import("../sema/sema_stmt.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("../state/obj.zig");
const hash_table_mod = @import("../state/hash_table.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const mem = std.mem;
const Allocator = mem.Allocator;
const BoundedArray = std.BoundedArray;
const assert = std.debug.assert;
const spread = shared.spread;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const SemaExpr = sema_expr_mod.SemaExpr;
const SemaStmt = sema_stmt_mod.SemaStmt;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;
const Obj = obj_mod.Obj;
const HashTable = hash_table_mod.HashTable;
const Position = tokenizer_mod.Position;

const LogicalOperation = enum {
    or_,
    and_,
};

const ExprContext = struct {
    const JumpInfo = struct {
        index: usize,
        is_inverted: bool,
    };

    is_child_to_logical: bool = false,
    current_logical: ?LogicalOperation = null,
    previous_logical: ?LogicalOperation = null,
    last_jump: *?JumpInfo,
    else_branch_offsets: *BranchOffsets,
    then_branch_offsets: *BranchOffsets,
};

const BranchOffsets = BoundedArray(usize, 128);

pub const Compiler = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        TooManyConstants,
        TooManyBranchJumps,
        JumpTooBig,
    };

    vm_state: *VmState,
    allocator: Allocator,
    chunk: Chunk,

    pub fn compile(memory: *ManagedMemory, stmt: *const SemaStmt) Error!void {
        const allocator = memory.allocator();

        var vm_state: VmState = undefined;

        vm_state.objs = null;
        vm_state.strings = try HashTable.init(allocator);
        vm_state.stack = try Stack.init(allocator);

        var compiler = Self{
            .vm_state = &vm_state,
            .allocator = allocator,
            .chunk = try Chunk.init(allocator),
        };

        try compiler.compileStmt(stmt);
        try compiler.chunk.writeU8(.return_, null);

        vm_state.chunk = compiler.chunk;
        vm_state.ip = @ptrCast(&compiler.chunk.code.items[0]);

        memory.vm_state = vm_state;
    }

    fn compileStmt(
        self: *Self,
        stmt: *const SemaStmt,
    ) Error!void {
        switch (stmt.kind) {
            .block => |block| {
                for (block.stmts.items) |child_stmt| {
                    try self.compileStmt(child_stmt);
                }
            },
            .assert => |assert_stmt| {
                try self.compileExpr(assert_stmt.expr, null);
                try self.chunk.writeU8(.assert, stmt.position);
            },
            .print => |print| {
                try self.compileExpr(print.expr, null);
                try self.chunk.writeU8(.print, stmt.position);
            },
            .invalid => @panic("invalid statement"),
        }
    }

    fn compileExpr(
        self: *Self,
        expr: *const SemaExpr,
        ctx_opt: ?ExprContext,
    ) Error!void {
        var is_branching = false;
        var last_jump: ?ExprContext.JumpInfo = null;
        var else_branch_offsets: BranchOffsets = undefined;
        var then_branch_offsets: BranchOffsets = undefined;
        const ctx = if (ctx_opt) |passed_ctx| passed_ctx else blk: {
            else_branch_offsets = BranchOffsets.init(0) catch unreachable;
            then_branch_offsets = BranchOffsets.init(0) catch unreachable;

            break :blk ExprContext{
                .last_jump = &last_jump,
                .else_branch_offsets = &else_branch_offsets,
                .then_branch_offsets = &then_branch_offsets,
            };
        };

        switch (expr.kind) {
            .literal => |literal| {
                switch (literal) {
                    .int => |int| switch (int) {
                        -1 => try self.chunk.writeU8(.constant_int_n1, expr.position),
                        0 => try self.chunk.writeU8(.constant_int_0, expr.position),
                        1 => try self.chunk.writeU8(.constant_int_1, expr.position),
                        2 => try self.chunk.writeU8(.constant_int_2, expr.position),
                        3 => try self.chunk.writeU8(.constant_int_3, expr.position),
                        4 => try self.chunk.writeU8(.constant_int_4, expr.position),
                        5 => try self.chunk.writeU8(.constant_int_5, expr.position),
                        else => try self.chunk.writeConstant(.{ .int = int }, expr.position),
                    },
                    .float => |float| {
                        if (float == 0) {
                            try self.chunk.writeU8(.constant_float_0, expr.position);
                        } else if (float == 1) {
                            try self.chunk.writeU8(.constant_float_1, expr.position);
                        } else if (float == 2) {
                            try self.chunk.writeU8(.constant_float_2, expr.position);
                        } else {
                            try self.chunk.writeConstant(.{ .float = float }, expr.position);
                        }
                    },
                    .bool => |bool_| switch (bool_) {
                        true => try self.chunk.writeU8(.constant_bool_true, expr.position),
                        false => try self.chunk.writeU8(.constant_bool_false, expr.position),
                    },
                    .string => |string| try self.chunk.writeConstant(
                        .{
                            .obj = &(try Obj.String.createFromCopied(
                                self.allocator,
                                self.vm_state,
                                string,
                            )).obj,
                        },
                        expr.position,
                    ),
                }
            },
            .binary => |binary| {
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
                    => try self.arithmetic(&binary, expr.position, ctx),

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
                        is_branching = true;
                        try self.comparison(&binary, expr.position, ctx);
                    },

                    .or_,
                    .and_,
                    => {
                        is_branching = true;
                        try self.logical(&binary, expr.position, ctx);
                    },
                }
            },
            .unary => |unary| {
                try self.compileExpr(unary.right, null);
                try self.chunk.writeU8(
                    switch (unary.kind) {
                        .negate_bool => OpCode.negate_bool,
                        .negate_int => OpCode.negate_int,
                        .negate_float => OpCode.negate_float,
                    },
                    expr.position,
                );
            },
            .invalid => @panic("invalid expression"),
        }

        if (!is_branching and ctx.is_child_to_logical) {
            const cmp_op_code = .compare_bool;
            const if_op_code = .if_not_equal;

            try self.chunk.writeU8(.constant_bool_true, expr.position);
            const offset, const is_inverted = try self.branchOff(
                cmp_op_code,
                if_op_code,
                expr.position,
                ctx,
            );

            if (is_inverted) {
                ctx.then_branch_offsets.append(offset) catch
                    return error.TooManyBranchJumps;
            } else {
                ctx.else_branch_offsets.append(offset) catch
                    return error.TooManyBranchJumps;
            }
        }
    }

    fn comparison(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
        ctx: ExprContext,
    ) Error!void {
        try self.compileExpr(expr.left, null);
        try self.compileExpr(expr.right, null);

        const cmp_op_code, const if_op_code = getComparisonOpCodes(expr);
        const offset, const is_inverted = try self.branchOff(
            cmp_op_code,
            if_op_code,
            position,
            ctx,
        );

        if (!ctx.is_child_to_logical or ctx.current_logical == null) {
            try self.chunk.writeU8(.constant_bool_true, position);
            const then_offset = try self.chunk.writeJump(.jump, position);
            try self.chunk.patchJump(offset);

            try self.chunk.writeU8(.constant_bool_false, position);
            try self.chunk.patchJump(then_offset);
        } else if (is_inverted) {
            ctx.then_branch_offsets.append(offset) catch
                return error.TooManyBranchJumps;
        } else {
            ctx.else_branch_offsets.append(offset) catch
                return error.TooManyBranchJumps;
        }
    }

    fn logical(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
        ctx: ExprContext,
    ) Error!void {
        const previous_logical = ctx.current_logical;
        const current_logical: LogicalOperation =
            if (expr.kind == .or_) .or_ else if (expr.kind == .and_) .and_ else unreachable;

        var new_else_branch_offsets: BranchOffsets = undefined;
        var new_then_branch_offsets: BranchOffsets = undefined;
        var else_branch_offsets = ctx.else_branch_offsets;
        var then_branch_offsets = ctx.then_branch_offsets;

        if (expr.kind == .or_) {
            new_else_branch_offsets = BranchOffsets.init(0) catch unreachable;
            else_branch_offsets = &new_else_branch_offsets;
        } else {
            new_then_branch_offsets = BranchOffsets.init(0) catch unreachable;
            then_branch_offsets = &new_then_branch_offsets;
        }

        const left_ctx = spread(ctx, .{
            .is_child_to_logical = true,
            .previous_logical = previous_logical,
            .current_logical = current_logical,
            .else_branch_offsets = else_branch_offsets,
            .then_branch_offsets = then_branch_offsets,
        });

        try self.compileExpr(expr.left, left_ctx);

        if (expr.kind == .or_) {
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

        const right_ctx = spread(left_ctx, .{
            .else_branch_offsets = ctx.else_branch_offsets,
            .then_branch_offsets = ctx.then_branch_offsets,
        });

        try self.compileExpr(expr.right, right_ctx);

        if (previous_logical == null) {
            // if the deepest branch condition was inverted, invert it back
            // as it is the end of the logical expression
            if (ctx.last_jump.* != null and ctx.last_jump.*.?.is_inverted) {
                try self.invertLastBranchJump(ctx);
            }

            try self.patchJumps(ctx.then_branch_offsets);

            try self.chunk.writeU8(.constant_bool_true, position);
            const then_offset = try self.chunk.writeJump(.jump, position);

            try self.patchJumps(ctx.else_branch_offsets);

            try self.chunk.writeU8(.constant_bool_false, position);
            try self.chunk.patchJump(then_offset);
        }
    }

    fn arithmetic(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
        ctx: ExprContext,
    ) Error!void {
        const new_ctx = spread(ctx, .{
            .is_child_to_logical = false,
            .current_logical = ctx.current_logical,
            .previous_logical = ctx.previous_logical,
            .else_branch_offsets = ctx.else_branch_offsets,
            .then_branch_offsets = ctx.then_branch_offsets,
        });

        try self.compileExpr(expr.left, new_ctx);
        try self.compileExpr(expr.right, new_ctx);

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

        try self.chunk.writeU8(op_code, position);
    }

    fn branchOff(
        self: *Self,
        cmp_op_code: OpCode,
        if_op_code: OpCode,
        position: Position,
        ctx: ExprContext,
    ) Error!struct { usize, bool } {
        const invert = ctx.current_logical == .or_ and ctx.is_child_to_logical;
        const final_if_op_code = if (invert)
            invertComparisonOpCode(if_op_code)
        else
            if_op_code;
        try self.chunk.writeU8(cmp_op_code, position);
        const offset = try self.chunk.writeJump(final_if_op_code, position);
        ctx.last_jump.* = .{ .index = offset - 1, .is_inverted = invert };

        return .{ offset, invert };
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

    fn patchJumps(self: *Self, jumps: *BranchOffsets) Error!void {
        while (jumps.popOrNull()) |jump| {
            try self.chunk.patchJump(jump);
        }
    }

    fn invertLastBranchJump(
        self: *Self,
        ctx: ExprContext,
    ) Error!void {
        const last_jump = ctx.last_jump.*.?;
        const if_op_code: OpCode = @enumFromInt(self.chunk.code.items[last_jump.index]);

        try self.chunk.updateU8(invertComparisonOpCode(if_op_code), last_jump.index);

        if (last_jump.is_inverted) {
            ctx.else_branch_offsets.append(ctx.then_branch_offsets.pop()) catch
                return error.TooManyBranchJumps;
        } else {
            ctx.then_branch_offsets.append(ctx.else_branch_offsets.pop()) catch
                return error.TooManyBranchJumps;
        }
    }
};
