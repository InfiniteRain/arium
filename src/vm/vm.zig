const std = @import("std");
const managed_memory_mod = @import("../state/managed_memory.zig");
const value_mod = @import("../state/value.zig");
const chunk_mod = @import("../compiler/chunk.zig");
const stack_mod = @import("../state/stack.zig");
const io_handler = @import("../io_handler.zig");
const object_mod = @import("../state/object.zig");

const Allocator = std.mem.Allocator;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Value = value_mod.Value;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Stack = stack_mod.Stack;
const IoHandler = io_handler.IoHandler;
const Object = object_mod.Object;

const VmError = error{
    OutOfMemory,
    InterpretError,
};

pub const Vm = struct {
    const Self = @This();

    memory: *ManagedMemory,
    io: *IoHandler,
    allocator: Allocator,
    state: *VmState,

    pub fn interpret(memory: *ManagedMemory, io: *IoHandler) VmError!void {
        const allocator = memory.allocator();

        var vm = Vm{
            .memory = memory,
            .io = io,
            .allocator = allocator,
            .state = &memory.vm_state.?,
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
                .negate_bool => {
                    self.push(.{ .bool = !self.pop().bool });
                },
                .negate_int => {
                    self.push(.{ .int = -self.pop().int });
                },
                .negate_float => {
                    self.push(.{ .float = -self.pop().float });
                },
                .add_int => {
                    const b = self.pop().int;
                    const a = self.pop().int;

                    self.push(.{ .int = a + b });
                },
                .add_float => {
                    const b = self.pop().float;
                    const a = self.pop().float;

                    self.push(.{ .float = a + b });
                },
                .subtract_int => {
                    const b = self.pop().int;
                    const a = self.pop().int;

                    self.push(.{ .int = a - b });
                },
                .subtract_float => {
                    const b = self.pop().float;
                    const a = self.pop().float;

                    self.push(.{ .float = a - b });
                },
                .multiply_int => {
                    const b = self.pop().int;
                    const a = self.pop().int;

                    self.push(.{ .int = a * b });
                },
                .multiply_float => {
                    const b = self.pop().float;
                    const a = self.pop().float;

                    self.push(.{ .float = a * b });
                },
                .divide_int => {
                    const b = self.pop().int;
                    const a = self.pop().int;

                    self.push(.{ .int = @divFloor(a, b) });
                },
                .divide_float => {
                    const b = self.pop().float;
                    const a = self.pop().float;

                    self.push(.{ .float = a / b });
                },
                .concat => {
                    try self.concat();
                },
                .return_ => {
                    const value = self.pop();
                    value.print(self.io);
                    self.io.out("\n");
                    return;
                },
                .pop => {
                    _ = self.pop();
                },
                _ => return error.InterpretError,
            }
        }
    }

    fn concat(self: *Self) VmError!void {
        const b = self.peek(0).object.as(Object.String);
        const a = self.peek(1).object.as(Object.String);

        const new_length = a.chars.len + b.chars.len;
        const new_chars = try self.allocator.alloc(u8, new_length);
        @memcpy(new_chars[0..a.chars.len], a.chars);
        @memcpy(new_chars[a.chars.len..(a.chars.len + b.chars.len)], b.chars);

        const result = try Object.String.createFromOwned(
            self.allocator,
            self.state,
            new_chars,
        );

        _ = self.pop();
        _ = self.pop();

        self.push(.{ .object = &result.object });
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
