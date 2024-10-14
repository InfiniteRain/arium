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
const CallFrame = managed_memory_mod.CallFrame;
const VmState = managed_memory_mod.VmState;
const ManagedMemory = managed_memory_mod.ManagedMemory;
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
        var frame = self.getCallFrame();

        while (true) {
            const ip_offset = getOffset(frame);

            // todo: extract it so that the check isnt executed every cycle
            if (self.config.debugExecutionIteration != null and
                self.config.debug_writer != null)
            {
                const callback = self.config.debugExecutionIteration.?;
                const writer = self.config.debug_writer.?;
                const vm_state = self.memory.vm_state.?;
                const stack = vm_state.stack;
                const end = (@intFromPtr(stack.top) - @intFromPtr(frame.stack)) / @sizeOf(Value);
                const values = frame.stack[frame.@"fn".locals_count..end];

                callback(writer, values, &frame.@"fn".chunk, ip_offset);
            }

            const op_code = readOpCode(frame);

            switch (op_code) {
                .constant => {
                    const index = readU8(frame);
                    self.push(chunk(frame).constants.items[index]);
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
                    const index = readU8(frame);
                    const a = self.pop();

                    storeLocal(frame, index, a);
                },
                .store_local_0 => storeLocal(frame, 0, self.pop()),
                .store_local_1 => storeLocal(frame, 1, self.pop()),
                .store_local_2 => storeLocal(frame, 2, self.pop()),
                .store_local_3 => storeLocal(frame, 3, self.pop()),
                .store_local_4 => storeLocal(frame, 4, self.pop()),
                .load_local => {
                    const index = readU8(frame);
                    const value = loadLocal(frame, index);

                    self.push(value);
                },
                .load_local_0 => self.push(loadLocal(frame, 0)),
                .load_local_1 => self.push(loadLocal(frame, 1)),
                .load_local_2 => self.push(loadLocal(frame, 2)),
                .load_local_3 => self.push(loadLocal(frame, 3)),
                .load_local_4 => self.push(loadLocal(frame, 4)),

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
                    const offset = readU16(frame);
                    const a = self.pop().int;

                    if (a == 0) {
                        frame.ip += offset;
                    }
                },
                .if_not_equal => {
                    const offset = readU16(frame);
                    const a = self.pop().int;

                    if (a != 0) {
                        frame.ip += offset;
                    }
                },
                .if_greater => {
                    const offset = readU16(frame);
                    const a = self.pop().int;

                    if (a > 0) {
                        frame.ip += offset;
                    }
                },
                .if_greater_equal => {
                    const offset = readU16(frame);
                    const a = self.pop().int;

                    if (a >= 0) {
                        frame.ip += offset;
                    }
                },
                .if_less => {
                    const offset = readU16(frame);
                    const a = self.pop().int;

                    if (a < 0) {
                        frame.ip += offset;
                    }
                },
                .if_less_equal => {
                    const offset = readU16(frame);
                    const a = self.pop().int;

                    if (a <= 0) {
                        frame.ip += offset;
                    }
                },
                .if_true => {
                    const offset = readU16(frame);
                    const a = self.pop().bool;

                    if (a) {
                        frame.ip += offset;
                    }
                },
                .if_false => {
                    const offset = readU16(frame);
                    const a = self.pop().bool;

                    if (!a) {
                        frame.ip += offset;
                    }
                },
                .jump => {
                    const offset = readU16(frame);
                    frame.ip += offset;
                },
                .negative_jump => {
                    const offset = readU16(frame);
                    frame.ip -= offset;
                },

                .assert => {
                    const a = self.pop().bool;

                    if (!a) {
                        try self.panic(
                            .assertion_fail,
                            getPosition(frame, ip_offset),
                        );
                        return error.Panic;
                    }
                },
                .print => {
                    const value = self.pop();
                    value_reporter.printValue(value, self.out_writer);
                    self.out_writer.print("\n");
                },
                .call => {
                    const arg_count = readU8(frame);
                    const callee = (self.state.stack.top - arg_count - 1)[0];
                    const @"fn" = callee.obj.as(Obj.Fn);

                    // todo: stack overflow

                    self.addCallFrame(.{
                        .@"fn" = @"fn",
                        .ip = @ptrCast(&@"fn".chunk.code.items[0]),
                        .stack = self.state.stack.top - arg_count - 1,
                    });

                    self.state.stack.top += @"fn".locals_count - arg_count - 1;
                    frame = self.getCallFrame();
                },
                .@"return" => {
                    assert(self.state.stack.top - (frame.@"fn".locals_count + 1) == self.getCallFrame().stack);

                    const value = self.pop();
                    const locals_count = frame.@"fn".locals_count;

                    if (self.popCallFrame() == .is_last) {
                        return;
                    }

                    self.state.stack.top -= locals_count;
                    frame = self.getCallFrame();
                    self.push(value);
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

    fn readOpCode(frame: *CallFrame) OpCode {
        return @enumFromInt(readU8(frame));
    }

    fn readU8(frame: *CallFrame) u8 {
        const byte = frame.ip[0];
        frame.ip += 1;
        return byte;
    }

    fn readU16(frame: *CallFrame) u16 {
        const left: u16 = readU8(frame);
        const right = readU8(frame);
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

    fn storeLocal(frame: *CallFrame, index: usize, value: Value) void {
        frame.stack[index] = value;
    }

    fn loadLocal(frame: *CallFrame, index: usize) Value {
        return frame.stack[index];
    }

    fn chunk(frame: *CallFrame) *Chunk {
        return &frame.@"fn".chunk;
    }

    fn getCallFrame(self: *Self) *CallFrame {
        var call_frames = &self.memory.vm_state.?.call_frames;
        return &call_frames.slice()[call_frames.len - 1];
    }

    fn addCallFrame(self: *Self, frame: CallFrame) void {
        self.memory.vm_state.?.call_frames.append(frame) catch unreachable;
        // todo: check out of bounds
    }

    fn popCallFrame(self: *Self) enum { is_last, is_not_last } {
        const call_frames = &self.memory.vm_state.?.call_frames;

        _ = call_frames.pop();

        return if (call_frames.len == 0)
            .is_last
        else
            .is_not_last;
    }

    fn getOffset(frame: *CallFrame) usize {
        return @intFromPtr(frame.ip) - @intFromPtr(&frame.@"fn".chunk.code.items[0]);
    }

    fn getPosition(frame: *CallFrame, offset: usize) Position {
        return chunk(frame).positions.items[offset];
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
