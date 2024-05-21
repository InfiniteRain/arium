const std = @import("std");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_expression_mod = @import("../sema/sema_expression.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");

const mem = std.mem;
const Allocator = mem.Allocator;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const SemaExpression = sema_expression_mod.SemaExpression;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;

const CompilerError = error{
    OutOfMemory,
    TooManyConstants,
};

pub const Compiler = struct {
    const Self = @This();

    allocator: Allocator,
    chunk: Chunk,

    pub fn compile(memory: *ManagedMemory, expression: *const SemaExpression) CompilerError!void {
        const allocator = memory.allocator();

        var compiler = Self{
            .allocator = allocator,
            .chunk = try Chunk.init(memory),
        };

        try compiler.compileExpression(expression);
        try compiler.chunk.writeByte(.return_, null);

        memory.vm_state = .{
            .chunk = compiler.chunk,
            .ip = @ptrCast(&compiler.chunk.code.items[0]),
            .stack = try Stack.init(allocator),
        };
    }

    fn compileExpression(self: *Self, expression: *const SemaExpression) CompilerError!void {
        switch (expression.kind) {
            .literal => |literal| {
                const value: Value = switch (literal) {
                    .int => |int| .{ .int = int },
                    .float => |float| .{ .float = float },
                    .bool => |boolean| .{ .bool = boolean },
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
