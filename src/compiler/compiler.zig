const std = @import("std");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_expr_mod = @import("../sema/sema_expr.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const object_mod = @import("../state/object.zig");
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
const Object = object_mod.Object;
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

        vm_state.objects = null;
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
                            .object = &(try Object.String.createFromCopied(
                                self.allocator,
                                self.vm_state,
                                string,
                            )).object,
                        },
                        expr.position,
                    ),
                }
            },
            .binary => |binary| {
                try self.compileExpr(binary.left);
                try self.compileExpr(binary.right);

                switch (binary.kind) {
                    .add_int => try self.chunk.writeU8(.add_int, expr.position),
                    .add_float => try self.chunk.writeU8(.add_float, expr.position),
                    .subtract_int => try self.chunk.writeU8(.subtract_int, expr.position),
                    .subtract_float => try self.chunk.writeU8(.subtract_float, expr.position),
                    .multiply_int => try self.chunk.writeU8(.multiply_int, expr.position),
                    .multiply_float => try self.chunk.writeU8(.multiply_float, expr.position),
                    .divide_int => try self.chunk.writeU8(.divide_int, expr.position),
                    .divide_float => try self.chunk.writeU8(.divide_float, expr.position),

                    .concat => try self.chunk.writeU8(.concat, expr.position),

                    .equal_int,
                    .equal_float,
                    .equal_bool,
                    .equal_obj,
                    .not_equal_int,
                    .not_equal_float,
                    .not_equal_bool,
                    .not_equal_obj,
                    => try self.equal(binary.kind, expr.position),
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

    fn equal(self: *Self, kind: SemaExpr.Kind.Binary.Kind, position: ?Position) CompilerError!void {
        var if_op_code: OpCode = undefined;
        var is_negated: bool = undefined;

        switch (kind) {
            .equal_int => {
                is_negated = false;
                if_op_code = .if_not_equal_int;
            },
            .equal_float => {
                is_negated = false;
                if_op_code = .if_not_equal_float;
            },
            .equal_bool => {
                is_negated = false;
                if_op_code = .if_not_equal_bool;
            },
            .equal_obj => {
                is_negated = false;
                if_op_code = .if_not_equal_obj;
            },
            .not_equal_int => {
                is_negated = true;
                if_op_code = .if_not_equal_int;
            },
            .not_equal_float => {
                is_negated = true;
                if_op_code = .if_not_equal_float;
            },
            .not_equal_bool => {
                is_negated = true;
                if_op_code = .if_not_equal_bool;
            },
            .not_equal_obj => {
                is_negated = true;
                if_op_code = .if_not_equal_obj;
            },
            else => unreachable,
        }

        const if_op_code_offset = try self.chunk.writeJump(if_op_code, position);

        try self.chunk.writeU8(
            if (is_negated) OpCode.constant_bool_false else OpCode.constant_bool_true,
            position,
        );

        const jump_offset = try self.chunk.writeJump(.jump, position);

        try self.chunk.patchJump(if_op_code_offset);
        try self.chunk.writeU8(
            if (is_negated) OpCode.constant_bool_true else OpCode.constant_bool_false,
            position,
        );
        try self.chunk.patchJump(jump_offset);
    }
};
