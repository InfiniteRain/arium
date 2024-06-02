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
    JumpTooBig,
};

pub const Compiler = struct {
    const Self = @This();

    vm_state: *VmState,
    allocator: Allocator,
    chunk: Chunk,

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
        };

        try compiler.compileExpr(expr);
        try compiler.chunk.writeU8(.return_, null);

        vm_state.chunk = compiler.chunk;
        vm_state.ip = @ptrCast(&compiler.chunk.code.items[0]);

        memory.vm_state = vm_state;
    }

    fn compileExpr(self: *Self, expr: *const SemaExpr) CompilerError!void {
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
                    => try self.equal(&binary, expr.position),
                }
            },
            .unary => |unary| {
                try self.compileExpr(unary.right);
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
    }

    fn arithmetic(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
    ) CompilerError!void {
        try self.compileExpr(expr.left);
        try self.compileExpr(expr.right);

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

    fn equal(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
    ) CompilerError!void {
        try self.compileExpr(expr.left);
        try self.compileExpr(expr.right);

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

        try self.chunk.writeU8(cmp_op_code, position);
        const if_op_code_offset = try self.chunk.writeJump(if_op_code, position);
        try self.chunk.writeU8(.constant_bool_true, position);
        const jump_offset = try self.chunk.writeJump(.jump, position);
        try self.chunk.patchJump(if_op_code_offset);
        try self.chunk.writeU8(OpCode.constant_bool_false, position);
        try self.chunk.patchJump(jump_offset);
    }
};
