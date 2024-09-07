const std = @import("std");
const shared = @import("shared");
const managed_memory_mod = @import("../state/managed_memory.zig");
const value_mod = @import("../state/value.zig");
const chunk_mod = @import("../compiler/chunk.zig");
const stack_mod = @import("../state/stack.zig");
const obj_mod = @import("../state/obj.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const value_reporter = @import("../reporter/value_reporter.zig");
const debug_reporter = @import("../reporter/debug_reporter.zig");

const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;
const assert = std.debug.assert;
const SharedDiags = shared.Diags;
const Writer = shared.Writer;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Value = value_mod.Value;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Stack = stack_mod.Stack;
const Obj = obj_mod.Obj;
const Token = tokenizer_mod.Token;
const Position = tokenizer_mod.Position;

pub const Vm = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        Panic,
    };

    pub const DiagEntry = struct {
        pub const Kind = enum {
            assertion_fail,
        };

        kind: Kind,
        position: Position,
    };

    pub const Diags = SharedDiags(DiagEntry);

    pub const Config = struct {
        debug_writer: ?*const Writer = null,
        debugExecutionIteration: ?*const fn (
            writer: *const Writer,
            values: []const Value,
            chunk: *const Chunk,
            ip_offset: usize,
        ) void = null,
    };

    config: Config,
    memory: *ManagedMemory,
    out_writer: *const Writer,
    allocator: Allocator,
    state: *VmState,
    diags: ?*Diags,

    pub fn interpret(
        memory: *ManagedMemory,
        out_writer: *const Writer,
        diags: ?*Diags,
        config: Config,
    ) Error!void {
        const allocator = memory.allocator();

        var vm = Vm{
            .memory = memory,
            .allocator = allocator,
            .out_writer = out_writer,
            .state = &memory.vm_state.?,
            .config = config,
            .diags = diags,
        };

        try vm.run();
    }

    fn run(self: *Self) Error!void {
        while (true) {
            const ip_offset = self.getOffset();

            if (self.config.debugExecutionIteration != null and
                self.config.debug_writer != null)
            {
                const callback = self.config.debugExecutionIteration.?;
                const writer = self.config.debug_writer.?;
                const vm_state = self.memory.vm_state.?;
                const stack = vm_state.stack;
                const size = (@intFromPtr(stack.top) - @intFromPtr(&stack.items[0])) / @sizeOf(Value);
                const values = stack.items[0..size];

                callback(writer, values, &vm_state.chunk, ip_offset);
            }

            const op_code = self.readOpCode();

            switch (op_code) {
                .constant => {
                    const index = self.readU8();
                    self.push(self.chunk().constants.items[index]);
                },
                .constant_unit => self.push(.unit),
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

                .store_local => {
                    const index = self.readU8();
                    const a = self.pop();

                    self.storeLocal(index, a);
                },
                .store_local_0 => self.storeLocal(0, self.pop()),
                .store_local_1 => self.storeLocal(1, self.pop()),
                .store_local_2 => self.storeLocal(2, self.pop()),
                .store_local_3 => self.storeLocal(3, self.pop()),
                .store_local_4 => self.storeLocal(4, self.pop()),
                .load_local => {
                    const index = self.readU8();
                    const value = self.loadLocal(index);

                    self.push(value);
                },
                .load_local_0 => self.push(self.loadLocal(0)),
                .load_local_1 => self.push(self.loadLocal(1)),
                .load_local_2 => self.push(self.loadLocal(2)),
                .load_local_3 => self.push(self.loadLocal(3)),
                .load_local_4 => self.push(self.loadLocal(4)),

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
                .negative_jump => {
                    const offset = self.readU16();
                    self.state.ip -= offset;
                },

                .assert => {
                    const a = self.pop().bool;

                    if (!a) {
                        try self.panic(
                            .assertion_fail,
                            self.getPosition(ip_offset),
                        );
                        return error.Panic;
                    }
                },
                .print => {
                    const value = self.pop();
                    value_reporter.printValue(value, self.out_writer);
                    self.out_writer.print("\n");
                },
                .@"return" => {
                    assert(self.state.stack.top == @as([*]Value, @ptrCast(&self.state.stack.items[0])));
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

    fn storeLocal(self: *Self, index: usize, value: Value) void {
        self.chunk().locals[index] = value;
    }

    fn loadLocal(self: *Self, index: usize) Value {
        return self.chunk().locals[index];
    }

    fn chunk(self: *Self) *Chunk {
        return &self.state.chunk;
    }

    fn getOffset(self: *Self) usize {
        return @intFromPtr(self.memory.vm_state.?.ip) - @intFromPtr(&self.chunk().code.items[0]);
    }

    fn getPosition(self: *Self, offset: usize) Position {
        return self.chunk().positions.items[offset];
    }

    fn panic(
        self: *Self,
        diag_kind: DiagEntry.Kind,
        position: Position,
    ) Error!void {
        if (self.diags) |diags| {
            // in case of ever needing to alloc something in here, make sure to
            // use diags.allocator instead of self.allocator. this is
            // necessary for lang-tests where a new allocator is created for
            // each test to detect memory leaks. that allocator then gets
            // deinited while diags are owned by the tests.
            try diags.add(.{
                .position = position,
                .kind = diag_kind,
            });
        }
    }
};
