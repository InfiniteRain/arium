const std = @import("std");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_expression_mod = @import("../sema/sema_expression.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const object_mod = @import("../state/object.zig");

const mem = std.mem;
const Allocator = mem.Allocator;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const SemaExpression = sema_expression_mod.SemaExpression;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;
const Object = object_mod.Object;

const CompilerError = error{
    OutOfMemory,
    TooManyConstants,
};

pub const Compiler = struct {
    const Self = @This();

    vm_state: *VmState,
    allocator: Allocator,
    chunk: Chunk,

    pub fn compile(memory: *ManagedMemory, expression: *const SemaExpression) CompilerError!void {
        const allocator = memory.allocator();

        var vm_state: VmState = undefined;

        vm_state.objects = null;

        var compiler = Self{
            .vm_state = &vm_state,
            .allocator = allocator,
            .chunk = try Chunk.init(memory),
        };

        try compiler.compileExpression(expression);
        try compiler.chunk.writeByte(.return_, null);

        vm_state.chunk = compiler.chunk;
        vm_state.ip = @ptrCast(&compiler.chunk.code.items[0]);
        vm_state.stack = try Stack.init(allocator);

        memory.vm_state = vm_state;
    }

    fn compileExpression(self: *Self, expression: *const SemaExpression) CompilerError!void {
        switch (expression.kind) {
            .literal => |literal| {
                const value: Value = switch (literal) {
                    .int => |int| .{ .int = int },
                    .float => |float| .{ .float = float },
                    .bool => |boolean| .{ .bool = boolean },
                    .string => |string| .{
                        .object = &(try Object.String.createFromCopied(
                            self.allocator,
                            self.vm_state,
                            string,
                        )).object,
                    },
                };

                try self.chunk.writeConstant(value, expression.position);
            },
            .binary => |binary| {
                try self.compileExpression(binary.left);
                try self.compileExpression(binary.right);
                try self.chunk.writeByte(switch (binary.kind) {
                    .add_int => OpCode.add_int,
                    .add_float => OpCode.add_float,
                    .subtract_int => OpCode.subtract_int,
                    .subtract_float => OpCode.subtract_float,
                    .multiply_int => OpCode.multiply_int,
                    .multiply_float => OpCode.multiply_float,
                    .divide_int => OpCode.divide_int,
                    .divide_float => OpCode.divide_float,
                    .concat => OpCode.concat,
                    .invalid => @panic("invalid binary expression"),
                }, expression.position);
            },
            .unary => |unary| {
                try self.compileExpression(unary.right);
                try self.chunk.writeByte(
                    switch (unary.kind) {
                        .negate_bool => OpCode.negate_bool,
                        .negate_int => OpCode.negate_int,
                        .negate_float => OpCode.negate_float,
                        .invalid => @panic("invalid unary expression"),
                    },
                    expression.position,
                );
            },
        }
    }
};
