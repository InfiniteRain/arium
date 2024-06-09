const std = @import("std");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_expr_mod = @import("../sema/sema_expr.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("../state/obj.zig");
const hash_table_mod = @import("../state/hash_table.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const mem = std.mem;
const Allocator = mem.Allocator;
const BoundedArray = std.BoundedArray;
const assert = std.debug.assert;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const SemaExpr = sema_expr_mod.SemaExpr;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;
const Obj = obj_mod.Obj;
const HashTable = hash_table_mod.HashTable;
const Position = tokenizer_mod.Position;

const CompilerError = error{
    OutOfMemory,
    TooManyConstants,
    TooManyBranchJumps,
    JumpTooBig,
};

const LogicalOperation = enum {
    or_,
    and_,
};

const ExprContext = struct {
    is_child_to_logical: bool = false,
    is_logical_rhs: bool = false,
};

const BranchOffsets = BoundedArray(usize, 128);

pub const Compiler = struct {
    const Self = @This();

    vm_state: *VmState,
    allocator: Allocator,
    chunk: Chunk,
    else_branch_offsets: BranchOffsets,
    then_branch_offsets: BranchOffsets,
    current_logical: ?LogicalOperation = null,
    previous_logical: ?LogicalOperation = null,
    last_jump: ?struct { index: usize, is_inverted: bool } = null,

    // todo: refactor codegen for logical jumps:
    //       * get rid of compiler state, pass around context
    //       * BranchOffsets could be declared on the stack and their pointers
    //         passed around in a context. the lifetime is in OR's and root
    //         logical expr
    //       * get rid of the ugly nestedLogicalExpr function, i can check
    //         whether an expr is a branch to a logical expr by checking previous_logical

    pub fn compile(memory: *ManagedMemory, expr: *const SemaExpr) CompilerError!void {
        const allocator = memory.allocator();

        var vm_state: VmState = undefined;

        vm_state.objs = null;
        vm_state.strings = try HashTable.init(allocator);
        vm_state.stack = try Stack.init(allocator);

        var compiler = Self{
            .vm_state = &vm_state,
            .allocator = allocator,
            .chunk = try Chunk.init(allocator),
            .else_branch_offsets = BranchOffsets.init(0) catch unreachable,
            .then_branch_offsets = BranchOffsets.init(0) catch unreachable,
        };

        try compiler.compileExpr(expr, .{});
        try compiler.chunk.writeU8(.return_, null);

        vm_state.chunk = compiler.chunk;
        vm_state.ip = @ptrCast(&compiler.chunk.code.items[0]);

        memory.vm_state = vm_state;
    }

    fn compileExpr(
        self: *Self,
        expr: *const SemaExpr,
        ctx: ExprContext,
    ) CompilerError!void {
        var is_branching = false;

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
                    => try self.arithmetic(&binary, expr.position),

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
                        try self.logical(&binary, expr.position);
                    },
                }
            },
            .unary => |unary| {
                try self.compileExpr(unary.right, .{});
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
                self.then_branch_offsets.append(offset) catch return error.TooManyBranchJumps;
            } else {
                self.else_branch_offsets.append(offset) catch return error.TooManyBranchJumps;
            }
        }
    }

    fn compilePotentiallyNestedLogicalExpr(
        self: *Self,
        expr: *const SemaExpr,
        ctx: ExprContext,
    ) CompilerError!void {
        const is_logical = expr.kind == .binary and expr.kind.binary.isLogical();

        const old_else_branch_offsets = self.else_branch_offsets;
        const old_then_branch_offsets = self.then_branch_offsets;
        const old_current_logical = self.current_logical;
        const old_previous_logical = self.previous_logical;
        const old_last_jump = self.last_jump;

        if (is_logical) {
            self.else_branch_offsets = BranchOffsets.init(0) catch unreachable;
            self.then_branch_offsets = BranchOffsets.init(0) catch unreachable;
            self.current_logical = null;
            self.previous_logical = null;
            self.last_jump = null;
        }

        try self.compileExpr(expr, ctx);

        if (is_logical) {
            self.else_branch_offsets = old_else_branch_offsets;
            self.then_branch_offsets = old_then_branch_offsets;
            self.current_logical = old_current_logical;
            self.previous_logical = old_previous_logical;
            self.last_jump = old_last_jump;
        }
    }

    fn comparison(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
        ctx: ExprContext,
    ) CompilerError!void {
        try self.compilePotentiallyNestedLogicalExpr(expr.left, .{});
        try self.compilePotentiallyNestedLogicalExpr(expr.right, .{});

        const cmp_op_code, const if_op_code = getComparisonOpCodes(expr);
        const offset, const is_inverted = try self.branchOff(
            cmp_op_code,
            if_op_code,
            position,
            ctx,
        );

        if (!ctx.is_child_to_logical or self.current_logical == null) {
            try self.chunk.writeU8(.constant_bool_true, position);
            const then_offset = try self.chunk.writeJump(.jump, position);
            try self.chunk.patchJump(offset);

            try self.chunk.writeU8(.constant_bool_false, position);
            try self.chunk.patchJump(then_offset);
        } else if (is_inverted) {
            self.then_branch_offsets.append(offset) catch return error.TooManyBranchJumps;
        } else {
            self.else_branch_offsets.append(offset) catch return error.TooManyBranchJumps;
        }
    }

    fn logical(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
    ) CompilerError!void {
        const old_previous_logical = self.previous_logical;
        self.previous_logical = self.current_logical;
        self.current_logical =
            if (expr.kind == .or_) .or_ else if (expr.kind == .and_) .and_ else unreachable;

        var old_else_branch_offsets: ?BranchOffsets = null;

        if (expr.kind == .or_) {
            old_else_branch_offsets = self.else_branch_offsets;
            self.else_branch_offsets = BranchOffsets.init(0) catch unreachable;
        }

        try self.compileExpr(expr.left, .{
            .is_child_to_logical = true,
            .is_logical_rhs = false,
        });

        if (expr.kind == .or_) {
            if (self.last_jump != null and !self.last_jump.?.is_inverted) {
                try self.invertLastBranchJump();
            }

            try self.patchJumps(&self.else_branch_offsets);

            if (old_else_branch_offsets) |old_offsets| {
                self.else_branch_offsets = old_offsets;
            }
        }

        try self.compileExpr(expr.right, .{
            .is_child_to_logical = true,
            .is_logical_rhs = true,
        });

        if (self.current_logical != null and self.previous_logical == null) {
            // if the deepest branch condition was inverted, invert it back
            // as it is the end of the logical expression
            if (self.last_jump != null and self.last_jump.?.is_inverted) {
                try self.invertLastBranchJump();
            }

            try self.patchJumps(&self.then_branch_offsets);

            try self.chunk.writeU8(.constant_bool_true, position);
            const then_offset = try self.chunk.writeJump(.jump, position);

            try self.patchJumps(&self.else_branch_offsets);

            try self.chunk.writeU8(.constant_bool_false, position);
            try self.chunk.patchJump(then_offset);
        }

        self.current_logical = self.previous_logical;
        self.previous_logical = old_previous_logical;
    }

    fn arithmetic(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
    ) CompilerError!void {
        try self.compileExpr(expr.left, .{});
        try self.compileExpr(expr.right, .{});

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
    ) CompilerError!struct { usize, bool } {
        const invert = self.current_logical == .or_ and ctx.is_child_to_logical;
        const final_if_op_code = if (invert)
            invertComparisonOpCode(if_op_code)
        else
            if_op_code;
        try self.chunk.writeU8(cmp_op_code, position);
        const offset = try self.chunk.writeJump(final_if_op_code, position);
        self.last_jump = .{ .index = offset - 1, .is_inverted = invert };

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

    fn patchJumps(self: *Self, jumps: *BranchOffsets) CompilerError!void {
        while (jumps.popOrNull()) |jump| {
            try self.chunk.patchJump(jump);
        }
    }

    fn invertLastBranchJump(self: *Self) CompilerError!void {
        const last_jump = self.last_jump.?;
        const if_op_code: OpCode = @enumFromInt(self.chunk.code.items[last_jump.index]);

        try self.chunk.updateU8(invertComparisonOpCode(if_op_code), last_jump.index);

        if (last_jump.is_inverted) {
            self.else_branch_offsets.append(self.then_branch_offsets.pop()) catch return error.TooManyBranchJumps;
        } else {
            self.then_branch_offsets.append(self.else_branch_offsets.pop()) catch return error.TooManyBranchJumps;
        }
    }
};
