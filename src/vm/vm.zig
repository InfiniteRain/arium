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
const ArrayList = std.ArrayList;
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
            stack_overflow,
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

        const should_trace_execution =
            config.debugExecutionIteration != null and
            config.debug_writer != null;

        if (should_trace_execution) {
            try vm.run(true);
        } else {
            try vm.run(false);
        }
    }

    fn run(self: *Self, comptime trace_execution: bool) Error!void {
        var frames = ArrayList(CallFrame).init(self.allocator);
        defer frames.clearAndFree();

        try frames.append(.{
            .ip = 0,
            .stack_bottom = 0,
        });

        var stack = try Stack.init(
            self.allocator,
            self.state.@"fn".locals_count,
        );
        defer stack.deinit();

        stack.setAt(0, .{ .obj = &self.state.@"fn".obj });

        self.state.stack = &stack;
        defer self.state.stack = null;

        var frame, var @"fn" = getCallFrame(&stack, &frames);

        while (true) {
            const inst_ip = frame.ip;

            if (trace_execution) {
                const callback = self.config.debugExecutionIteration.?;
                const writer = self.config.debug_writer.?;

                callback(
                    writer,
                    stack.values.items[frame.stack_bottom +
                        @"fn".locals_count .. stack.top],
                    &@"fn".chunk,
                    inst_ip,
                );
            }

            const op_code = readOpCode(frame, @"fn");

            switch (op_code) {
                .constant,
                => {
                    const index = readU8(frame, @"fn");
                    try self.push(
                        &stack,
                        &frames,
                        @"fn".chunk.constants.items[index],
                    );
                },

                .constant_unit,
                => try self.push(&stack, &frames, .unit),

                .constant_bool_false,
                => try self.push(&stack, &frames, .{ .bool = false }),

                .constant_bool_true,
                => try self.push(&stack, &frames, .{ .bool = true }),

                .constant_int_n1,
                => try self.push(&stack, &frames, .{ .int = -1 }),

                .constant_int_0,
                => try self.push(&stack, &frames, .{ .int = 0 }),

                .constant_int_1,
                => try self.push(&stack, &frames, .{ .int = 1 }),

                .constant_int_2,
                => try self.push(&stack, &frames, .{ .int = 2 }),

                .constant_int_3,
                => try self.push(&stack, &frames, .{ .int = 3 }),

                .constant_int_4,
                => try self.push(&stack, &frames, .{ .int = 4 }),

                .constant_int_5,
                => try self.push(&stack, &frames, .{ .int = 5 }),

                .constant_float_0,
                => try self.push(&stack, &frames, .{ .float = 0 }),

                .constant_float_1,
                => try self.push(&stack, &frames, .{ .float = 1 }),

                .constant_float_2,
                => try self.push(&stack, &frames, .{ .float = 2 }),

                .store_local => {
                    const index = readU8(frame, @"fn");
                    const a = pop(&stack);

                    storeLocal(&stack, frame, index, a);
                },

                .store_local_0,
                => storeLocal(&stack, frame, 0, pop(&stack)),

                .store_local_1,
                => storeLocal(&stack, frame, 1, pop(&stack)),

                .store_local_2,
                => storeLocal(&stack, frame, 2, pop(&stack)),

                .store_local_3,
                => storeLocal(&stack, frame, 3, pop(&stack)),

                .store_local_4,
                => storeLocal(&stack, frame, 4, pop(&stack)),

                .load_local => {
                    const index = readU8(frame, @"fn");
                    const value = loadLocal(&stack, frame, index);

                    try self.push(&stack, &frames, value);
                },

                .load_local_0,
                => try self.push(&stack, &frames, loadLocal(&stack, frame, 0)),

                .load_local_1,
                => try self.push(&stack, &frames, loadLocal(&stack, frame, 1)),

                .load_local_2,
                => try self.push(&stack, &frames, loadLocal(&stack, frame, 2)),

                .load_local_3,
                => try self.push(&stack, &frames, loadLocal(&stack, frame, 3)),

                .load_local_4,
                => try self.push(&stack, &frames, loadLocal(&stack, frame, 4)),

                .negate_bool,
                => try self.push(
                    &stack,
                    &frames,
                    .{ .bool = !pop(&stack).bool },
                ),

                .negate_int,
                => try self.push(
                    &stack,
                    &frames,
                    .{ .int = -pop(&stack).int },
                ),

                .negate_float,
                => try self.push(
                    &stack,
                    &frames,
                    .{ .float = -pop(&stack).float },
                ),

                .add_int => {
                    const b = pop(&stack).int;
                    const a = pop(&stack).int;

                    try self.push(&stack, &frames, .{ .int = a + b });
                },

                .add_float => {
                    const b = pop(&stack).float;
                    const a = pop(&stack).float;

                    try self.push(&stack, &frames, .{ .float = a + b });
                },

                .subtract_int => {
                    const b = pop(&stack).int;
                    const a = pop(&stack).int;

                    try self.push(&stack, &frames, .{ .int = a - b });
                },

                .subtract_float => {
                    const b = pop(&stack).float;
                    const a = pop(&stack).float;

                    try self.push(&stack, &frames, .{ .float = a - b });
                },

                .multiply_int => {
                    const b = pop(&stack).int;
                    const a = pop(&stack).int;

                    try self.push(&stack, &frames, .{ .int = a * b });
                },

                .multiply_float => {
                    const b = pop(&stack).float;
                    const a = pop(&stack).float;

                    try self.push(&stack, &frames, .{ .float = a * b });
                },

                .divide_int => {
                    const b = pop(&stack).int;
                    const a = pop(&stack).int;

                    try self.push(&stack, &frames, .{ .int = @divFloor(a, b) });
                },

                .divide_float => {
                    const b = pop(&stack).float;
                    const a = pop(&stack).float;

                    try self.push(&stack, &frames, .{ .float = a / b });
                },

                .concat => try self.concat(&stack, &frames),

                .compare_int => {
                    const b = pop(&stack).int;
                    const a = pop(&stack).int;

                    try self.push(&stack, &frames, .{ .int = a - b });
                },

                .compare_float => {
                    const b = pop(&stack).float;
                    const a = pop(&stack).float;

                    if (a > b) {
                        try self.push(&stack, &frames, .{ .int = 1 });
                    } else if (a < b) {
                        try self.push(&stack, &frames, .{ .int = -1 });
                    } else {
                        try self.push(&stack, &frames, .{ .int = 0 });
                    }
                },

                .compare_bool => {
                    const b: i64 = @intFromBool(pop(&stack).bool);
                    const a: i64 = @intFromBool(pop(&stack).bool);

                    try self.push(&stack, &frames, .{ .int = a - b });
                },

                .compare_obj => {
                    const b: i64 = @intCast(@intFromPtr(pop(&stack).obj));
                    const a: i64 = @intCast(@intFromPtr(pop(&stack).obj));

                    try self.push(&stack, &frames, .{ .int = a - b });
                },

                .if_equal => {
                    const offset = readU16(frame, @"fn");
                    const a = pop(&stack).int;

                    if (a == 0) {
                        frame.ip += offset;
                    }
                },

                .if_not_equal => {
                    const offset = readU16(frame, @"fn");
                    const a = pop(&stack).int;

                    if (a != 0) {
                        frame.ip += offset;
                    }
                },

                .if_greater => {
                    const offset = readU16(frame, @"fn");
                    const a = pop(&stack).int;

                    if (a > 0) {
                        frame.ip += offset;
                    }
                },

                .if_greater_equal => {
                    const offset = readU16(frame, @"fn");
                    const a = pop(&stack).int;

                    if (a >= 0) {
                        frame.ip += offset;
                    }
                },

                .if_less => {
                    const offset = readU16(frame, @"fn");
                    const a = pop(&stack).int;

                    if (a < 0) {
                        frame.ip += offset;
                    }
                },

                .if_less_equal => {
                    const offset = readU16(frame, @"fn");
                    const a = pop(&stack).int;

                    if (a <= 0) {
                        frame.ip += offset;
                    }
                },

                .if_true => {
                    const offset = readU16(frame, @"fn");
                    const a = pop(&stack).bool;

                    if (a) {
                        frame.ip += offset;
                    }
                },

                .if_false => {
                    const offset = readU16(frame, @"fn");
                    const a = pop(&stack).bool;

                    if (!a) {
                        frame.ip += offset;
                    }
                },

                .jump => {
                    const offset = readU16(frame, @"fn");
                    frame.ip += offset;
                },

                .negative_jump => {
                    const offset = readU16(frame, @"fn");
                    frame.ip -= offset;
                },

                .assert => {
                    const a = pop(&stack).bool;

                    if (!a) {
                        return self.panic(
                            .assertion_fail,
                            getPosition(@"fn", inst_ip),
                        );
                    }
                },

                .print => {
                    const value = pop(&stack);
                    value_reporter.printValue(value, self.out_writer);
                    self.out_writer.print("\n");
                },

                .call => {
                    const arg_count = readU8(frame, @"fn");
                    const callee = stack.peek(arg_count);
                    const new_fn = callee.obj.as(Obj.Fn);

                    try addCallFrame(.{
                        .ip = 0,
                        .stack_bottom = stack.top - arg_count - 1,
                    }, &frames);

                    stack.increaseTop(
                        new_fn.locals_count - arg_count - 1,
                    ) catch |err| switch (err) {
                        error.StackOverflow,
                        => return self.stackOverflowPanic(&frames, &stack),

                        error.OutOfMemory,
                        => return error.OutOfMemory,
                    };
                    frame, @"fn" = getCallFrame(&stack, &frames);
                },

                .@"return" => {
                    assert(stack.top - (@"fn".locals_count + 1) ==
                        getCallFrame(&stack, &frames)[0].stack_bottom);

                    const value = pop(&stack);
                    const locals_count = @"fn".locals_count;

                    if (popCallFrame(&frames) == .is_last) {
                        return;
                    }

                    stack.decreaseTop(locals_count);
                    frame, @"fn" = getCallFrame(&stack, &frames);
                    try self.push(&stack, &frames, value);
                },

                .pop => _ = pop(&stack),

                _ => @panic("invalid op code"),
            }
        }
    }

    fn concat(
        self: *Self,
        stack: *Stack,
        frames: *ArrayList(CallFrame),
    ) Error!void {
        const b = peek(stack, 0).obj.as(Obj.String);
        const a = peek(stack, 1).obj.as(Obj.String);

        const new_length = a.chars.len + b.chars.len;
        const new_chars = try self.allocator.alloc(u8, new_length);
        @memcpy(new_chars[0..a.chars.len], a.chars);
        @memcpy(new_chars[a.chars.len..(a.chars.len + b.chars.len)], b.chars);

        const result = try Obj.String.createFromOwned(
            self.allocator,
            self.state,
            new_chars,
        );

        _ = pop(stack);
        _ = pop(stack);

        try self.push(stack, frames, .{ .obj = &result.obj });
    }

    fn readOpCode(frame: *CallFrame, @"fn": *Obj.Fn) OpCode {
        return @enumFromInt(readU8(frame, @"fn"));
    }

    fn readU8(frame: *CallFrame, @"fn": *Obj.Fn) u8 {
        const byte = @"fn".chunk.code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readU16(frame: *CallFrame, @"fn": *Obj.Fn) u16 {
        const left: u16 = readU8(frame, @"fn");
        const right = readU8(frame, @"fn");
        return (left << 8) | right;
    }

    fn push(
        self: *Self,
        stack: *Stack,
        frames: *ArrayList(CallFrame),
        value: Value,
    ) Error!void {
        stack.push(value) catch |err| switch (err) {
            error.StackOverflow => {
                return self.stackOverflowPanic(frames, stack);
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
    }

    fn peek(stack: *Stack, distance: u32) Value {
        return stack.peek(distance);
    }

    fn pop(stack: *Stack) Value {
        return stack.pop();
    }

    fn storeLocal(
        stack: *Stack,
        frame: *CallFrame,
        index: u32,
        value: Value,
    ) void {
        stack.setAt(frame.stack_bottom + index, value);
    }

    fn loadLocal(stack: *Stack, frame: *CallFrame, index: u32) Value {
        return stack.getAt(frame.stack_bottom + index);
    }

    fn getCallFrame(
        stack: *Stack,
        frames: *ArrayList(CallFrame),
    ) struct { *CallFrame, *Obj.Fn } {
        const call_frame = &frames.items[frames.items.len - 1];
        const @"fn" = stack.getAt(call_frame.stack_bottom).obj.as(Obj.Fn);

        return .{ call_frame, @"fn" };
    }

    fn addCallFrame(
        frame: CallFrame,
        frames: *ArrayList(CallFrame),
    ) Allocator.Error!void {
        try frames.append(frame);
    }

    fn popCallFrame(
        frames: *ArrayList(CallFrame),
    ) enum { is_last, is_not_last } {
        _ = frames.pop();

        return if (frames.items.len == 0)
            .is_last
        else
            .is_not_last;
    }

    fn getPosition(@"fn": *Obj.Fn, offset: usize) Position {
        return @"fn".chunk.positions.items[offset];
    }

    fn panic(
        self: *Self,
        diag_kind: DiagEntry.Kind,
        position: Position,
    ) error{ Panic, OutOfMemory } {
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
        return error.Panic;
    }

    fn stackOverflowPanic(
        self: *Self,
        frames: *ArrayList(CallFrame),
        stack: *Stack,
    ) error{ Panic, OutOfMemory } {
        if (frames.items.len == 0) {
            @panic("call frames array is empty");
        }

        const frame = frames.items[frames.items.len - 1];
        const @"fn" = stack.getAt(frame.stack_bottom).obj.as(Obj.Fn);
        const positions = @"fn".chunk.positions.items;

        return self.panic(.stack_overflow, positions[frame.ip]);
    }
};
