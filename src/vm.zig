const std = @import("std");
const managed_memory_mod = @import("managed_memory.zig");
const value_mod = @import("value.zig");
const chunk_mod = @import("chunk.zig");
const stack_mod = @import("stack.zig");

const Allocator = std.mem.Allocator;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Value = value_mod.Value;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Stack = stack_mod.Stack;

const VmError = error{
    OutOfMemory,
    InterpretError,
};

pub const Vm = struct {
    const Self = @This();

    memory: *ManagedMemory,
    allocator: Allocator,
    state: VmState,

    pub fn interpret(memory: *ManagedMemory) VmError!void {
        const allocator = memory.allocator();

        var vm = Vm{
            .memory = memory,
            .allocator = allocator,
            .state = memory.vm_state.?,
        };

        try vm.run();
    }

    fn run(self: *Self) VmError!void {
        while (true) {
            const op_code = self.readOpCode();

            switch (op_code) {
                .constant => {
                    const index = self.readU8();
                    self.push(self.chunk().constants.items[index]);
                },
                .negate => {
                    self.push(-self.pop());
                },
                .add => {
                    const b = self.pop();
                    const a = self.pop();

                    self.push(a + b);
                },
                .subtract => {
                    const b = self.pop();
                    const a = self.pop();

                    self.push(a - b);
                },
                .multiply => {
                    const b = self.pop();
                    const a = self.pop();

                    self.push(a * b);
                },
                .divide => {
                    const b = self.pop();
                    const a = self.pop();

                    self.push(@divFloor(a, b));
                },
                .return_ => {
                    const value = self.pop();
                    std.debug.print("{}\n", .{value});
                    return;
                },
                .pop => {
                    _ = self.pop();
                },
                _ => return error.InterpretError,
            }
        }
    }

    fn readOpCode(self: *Self) OpCode {
        return @enumFromInt(self.readU8());
    }

    fn readU8(self: *Self) u8 {
        const byte = self.state.ip[0];
        self.state.ip += 1;
        return byte;
    }

    fn push(self: *Self, value: Value) void {
        self.state.stack.push(value);
    }

    fn peek(self: *Self, distance: usize) Value {
        return self.state.stack.peek(distance);
    }

    fn pop(self: *Self) Value {
        return self.state.stack.pop();
    }

    fn chunk(self: *Self) Chunk {
        return self.state.chunk;
    }
};
