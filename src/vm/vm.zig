const std = @import("std");
const shared = @import("shared");
const managed_memory_mod = @import("../state/managed_memory.zig");
const value_mod = @import("../state/value.zig");
const chunk_mod = @import("../compiler/chunk.zig");
const stack_mod = @import("../state/stack.zig");
const io_handler = @import("../io_handler.zig");
const obj_mod = @import("../state/obj.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;
const SharedDiagnostics = shared.Diagnostics;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Value = value_mod.Value;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Stack = stack_mod.Stack;
const IoHandler = io_handler.IoHandler;
const Obj = obj_mod.Obj;
const Token = tokenizer_mod.Token;
const Position = tokenizer_mod.Position;

pub const Vm = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        Panic,
    };

    pub const DiagnosticEntry = struct {
        message: []const u8,
        position: Position,

        pub fn deinit(self: *DiagnosticEntry, allocator: Allocator) void {
            allocator.free(self.message);
        }
    };

    pub const Diagnostics = SharedDiagnostics(DiagnosticEntry);

    pub const Config = struct {
        trace_execution: bool = false,
    };

    config: Config,
    memory: *ManagedMemory,
    io: *IoHandler,
    allocator: Allocator,
    state: *VmState,
    diagnostics: ?*Diagnostics,

    pub fn interpret(
        memory: *ManagedMemory,
        io: *IoHandler,
        diagnostics: ?*Diagnostics,
        config: Config,
    ) Error!void {
        const allocator = memory.allocator();

        var vm = Vm{
            .memory = memory,
            .io = io,
            .allocator = allocator,
            .state = &memory.vm_state.?,
            .config = config,
            .diagnostics = diagnostics,
        };

        try vm.run();
    }

    fn run(self: *Self) Error!void {
        while (true) {
            const ip_offset = self.getOffset();

            if (self.config.trace_execution) {
                self.io.out("               ");

                var slot: [*]Value = @ptrCast(&self.memory.vm_state.?.stack.items[0]);

                while (@intFromPtr(slot) < @intFromPtr(self.memory.vm_state.?.stack.top)) {
                    self.io.out("[");
                    slot[0].print(self.io);
                    self.io.out("] ");
                    slot += 1;
                }

                self.io.out("\n");

                _ = self.chunk().printInstruction(self.io, ip_offset);
            }

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

                .assert => {
                    const a = self.pop().bool;

                    if (!a) {
                        try self.panic(
                            self.getPosition(ip_offset),
                            "Assertion failed.",
                            .{},
                        );
                        return error.Panic;
                    }
                },
                .print => {
                    const value = self.pop();
                    value.print(self.io);
                    self.io.out("\n");
                },
                .return_ => {
                    return;
                },
                .pop => _ = self.pop(),
                _ => @panic("invalid op code"),
            }
        }
    }

    fn concat(self: *Self) Error!void {
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

    fn getOffset(self: *Self) usize {
        return @intFromPtr(self.memory.vm_state.?.ip) - @intFromPtr(&self.chunk().code.items[0]);
    }

    fn getPosition(self: *Self, offset: usize) Position {
        return self.chunk().positions.items[offset].?;
    }

    fn panic(
        self: *Self,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
    ) Error!void {
        if (self.diagnostics) |diagnostics| {
            // allocating with diagnostic's allocator, for an edge case found
            // in lang-tests, where new allocator is created for each test
            // to detect memory leaks; this makes line it so that diagnostics
            // could be created with the base allocator instead of an allocator
            // local to the test.
            const message = try allocPrint(diagnostics.allocator, fmt, args);

            try diagnostics.add(.{
                .position = position,
                .message = message,
            });
        }
    }
};
