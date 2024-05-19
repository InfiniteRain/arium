const std = @import("std");
const managed_memory_mod = @import("./managed_memory.zig");
const chunk_mod = @import("./chunk.zig");
const expression_mod = @import("./expression.zig");
const stack_mod = @import("stack.zig");

const Allocator = std.mem.Allocator;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Expression = expression_mod.Expression;
const Stack = stack_mod.Stack;

const CompilerError = error{
    OutOfMemory,
    TooManyConstants,
};

pub const Compiler = struct {
    const Self = @This();

    allocator: Allocator,
    chunk: Chunk,

    pub fn compile(memory: *ManagedMemory, expression: *Expression) CompilerError!void {
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

    fn compileExpression(self: *Self, expression: *Expression) CompilerError!void {
        switch (expression.*) {
            .literal => |literal| {
                try self.chunk.writeConstant(std.fmt.parseInt(
                    i64,
                    literal.token.lexeme,
                    10,
                ) catch unreachable, literal.token.position);
            },
            .binary => |binary| {
                try self.compileExpression(binary.left);
                try self.compileExpression(binary.right);
                try self.chunk.writeByte(switch (binary.operator.lexeme[0]) {
                    '-' => OpCode.subtract,
                    '+' => OpCode.add,
                    '/' => OpCode.divide,
                    '*' => OpCode.multiply,
                    else => unreachable,
                }, binary.operator.position);
            },
            .unary => |unary| {
                try self.compileExpression(unary.right);
                try self.chunk.writeByte(.negate, unary.operator.position);
            },
        }
    }
};
