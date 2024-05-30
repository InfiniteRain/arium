const std = @import("std");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_expr_mod = @import("../sema/sema_expr.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const object_mod = @import("../state/object.zig");
const hash_table_mod = @import("../state/hash_table.zig");

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

const CompilerError = error{
    OutOfMemory,
    TooManyConstants,
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
        try compiler.chunk.writeByte(.return_, null);

        vm_state.chunk = compiler.chunk;
        vm_state.ip = @ptrCast(&compiler.chunk.code.items[0]);

        memory.vm_state = vm_state;
    }

    fn compileExpr(self: *Self, expr: *const SemaExpr) CompilerError!void {
        switch (expr.kind) {
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

                try self.chunk.writeConstant(value, expr.position);
            },
            .binary => |binary| {
                try self.compileExpr(binary.left);
                try self.compileExpr(binary.right);
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
                }, expr.position);
            },
            .unary => |unary| {
                try self.compileExpr(unary.right);
                try self.chunk.writeByte(
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
};
