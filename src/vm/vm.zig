const std = @import("std");
const managed_memory_mod = @import("../state/managed_memory.zig");
const value_mod = @import("../state/value.zig");
const chunk_mod = @import("../compiler/chunk.zig");
const stack_mod = @import("../state/stack.zig");
const io_handler = @import("../io_handler.zig");
const obj_mod = @import("../state/obj.zig");

const Allocator = std.mem.Allocator;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Value = value_mod.Value;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Stack = stack_mod.Stack;
const IoHandler = io_handler.IoHandler;
const Obj = obj_mod.Obj;

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
            self.io.out("               ");

            var slot: [*]Value = @ptrCast(&self.memory.vm_state.?.stack.items[0]);

            while (@intFromPtr(slot) < @intFromPtr(self.memory.vm_state.?.stack.top)) {
                self.io.out("[");
                slot[0].print(self.io);
                self.io.out("] ");
                slot += 1;
            }

            self.io.out("\n");

            const instruction_offset = @intFromPtr(self.state.ip) - @intFromPtr(&self.state.chunk.code.items[0]);
            _ = self.state.chunk.printInstruction(self.io, instruction_offset);

            const op_code = self.readOpCode();

            switch (op_code) {
                .constant => {
                    const index = self.readU8();
                    self.push(self.chunk().constants.items[index]);
                },
                .constant_bool_false => self.push(.{ .bool = false }),
                .constant_bool_true => self.push(.{ .bool = true }),
                .constant_int_n1 => self.push(.{ .int = -1 }),
                .constant_int_0 => self.push(.{ .int = 0 }),
                .constant_int_1 => self.push(.{ .int = 1 }),
                .constant_int_2 => self.push(.{ .int = 2 }),
                .constant_int_3 => self.push(.{ .int = 3 }),
                .constant_int_4 => self.push(.{ .int = 4 }),
                .constant_int_5 => self.push(.{ .int = 5 }),
                .constant_float_0 => self.push(.{ .float = 0 }),
                .constant_float_1 => self.push(.{ .float = 1 }),
                .constant_float_2 => self.push(.{ .float = 2 }),

                .negate_bool => self.push(.{ .bool = !self.pop().bool }),
                .negate_int => self.push(.{ .int = -self.pop().int }),
                .negate_float => self.push(.{ .float = -self.pop().float }),
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

                .concat => try self.concat(),

                .compare_int => {
                    const b = self.pop().int;
                    const a = self.pop().int;

                    self.push(.{ .int = a - b });
                },
                .compare_float => {
                    const b = self.pop().float;
                    const a = self.pop().float;

                    if (a > b) {
                        self.push(.{ .int = 1 });
                    } else if (a < b) {
                        self.push(.{ .int = -1 });
                    } else {
                        self.push(.{ .int = 0 });
                    }
                },
                .compare_bool => {
                    const b: i64 = @intFromBool(self.pop().bool);
                    const a: i64 = @intFromBool(self.pop().bool);

                    self.push(.{ .int = a - b });
                },
                .compare_obj => {
                    const b: i64 = @intCast(@intFromPtr(self.pop().obj));
                    const a: i64 = @intCast(@intFromPtr(self.pop().obj));

                    self.push(.{ .int = a - b });
                },

                .if_equal => {
                    const offset = self.readU16();
                    const a = self.pop().int;

                    if (a == 0) {
                        self.state.ip += offset;
                    }
                },
                .if_not_equal => {
                    const offset = self.readU16();
                    const a = self.pop().int;

                    if (a != 0) {
                        self.state.ip += offset;
                    }
                },
                .if_greater => {
                    const offset = self.readU16();
                    const a = self.pop().int;

                    if (a > 0) {
                        self.state.ip += offset;
                    }
                },
                .if_greater_equal => {
                    const offset = self.readU16();
                    const a = self.pop().int;

                    if (a >= 0) {
                        self.state.ip += offset;
                    }
                },
                .if_less => {
                    const offset = self.readU16();
                    const a = self.pop().int;

                    if (a < 0) {
                        self.state.ip += offset;
                    }
                },
                .if_less_equal => {
                    const offset = self.readU16();
                    const a = self.pop().int;

                    if (a <= 0) {
                        self.state.ip += offset;
                    }
                },
                .if_true => {
                    const offset = self.readU16();
                    const a = self.pop().bool;

                    if (a) {
                        self.state.ip += offset;
                    }
                },
                .if_false => {
                    const offset = self.readU16();
                    const a = self.pop().bool;

                    if (!a) {
                        self.state.ip += offset;
                    }
                },
                .jump => {
                    const offset = self.readU16();
                    self.state.ip += offset;
                },

                .return_ => {
                    const value = self.pop();
                    value.print(self.io);
                    self.io.out("\n");
                    return;
                },
                .pop => _ = self.pop(),
                _ => return error.InterpretError,
            }
        }
    }

    fn concat(self: *Self) VmError!void {
        const b = self.peek(0).obj.as(Obj.String);
        const a = self.peek(1).obj.as(Obj.String);

        const new_length = a.chars.len + b.chars.len;
        const new_chars = try self.allocator.alloc(u8, new_length);
        @memcpy(new_chars[0..a.chars.len], a.chars);
        @memcpy(new_chars[a.chars.len..(a.chars.len + b.chars.len)], b.chars);

        const result = try Obj.String.createFromOwned(
            self.allocator,
            self.state,
            new_chars,
        );

        _ = self.pop();
        _ = self.pop();

        self.push(.{ .obj = &result.obj });
    }

    fn readOpCode(self: *Self) OpCode {
        return @enumFromInt(self.readU8());
    }

    fn readU8(self: *Self) u8 {
        const byte = self.state.ip[0];
        self.state.ip += 1;
        return byte;
    }

    fn readU16(self: *Self) u16 {
        const left: u16 = self.readU8();
        const right = self.readU8();
        return (left << 8) | right;
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
