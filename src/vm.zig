const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;
const debug = std.debug;
const assert = debug.assert;

const ExecutionMode = @import("debug.zig").ExecutionMode;
const limits = @import("limits.zig");
const Span = @import("span.zig").Span;
const memory_mod = @import("memory.zig");
const Value = memory_mod.Value;
const Object = memory_mod.Object;
const Memory = memory_mod.Memory;
const module_mod = @import("module.zig");
const Module = module_mod.Module;
const OpCode = module_mod.OpCode;
const Output = @import("output.zig").Output;

// todo: underflow/overflow checks
pub fn Vm(comptime mode: ExecutionMode) type {
    return struct {
        memory: *Memory(mode),
        module: *Module(mode),
        ip: usize,
        lv: usize,
        lv_len: usize,
        st: ArrayList(Value(mode)),
        diags: *Diags,
        debug_tracer: ?DebugTracer,

        const Self = @This();

        pub const Error = error{Panic} || Allocator.Error;

        pub const Diags = struct {
            entry: ?Entry,

            pub const Entry = struct {
                tag: Tag,
                loc: Span(u8),

                pub const Tag = enum {
                    stack_overflow,
                };
            };

            pub const empty: Diags = .{
                .entry = null,
            };

            pub fn deinit(self: *Diags, allocator: Allocator) void {
                _ = self;
                _ = allocator;
            }
        };

        pub const DebugTracer = struct {
            ptr: *const anyopaque,
            vtable: *const VTable,

            pub const VTable = struct {
                step: *const fn (*const anyopaque, vm: *const Self) void,
            };
        };

        pub fn interpret(
            memory: *Memory(mode),
            module: *Module(mode),
            output: *const Output,
            diags: *Diags,
            debug_tracer_opt: ?DebugTracer,
        ) Error!void {
            const allocator = memory.allocator();

            var vm = Self{
                .memory = memory,
                .module = module,
                .ip = undefined,
                .lv = undefined,
                .lv_len = undefined,
                .st = .empty,
                .diags = diags,
                .debug_tracer = debug_tracer_opt orelse undefined,
            };
            defer {
                vm.st.deinit(allocator);
            }

            try vm.run(output);
        }

        fn run(self: *Self, output: *const Output) Error!void {
            const allocator = self.memory.allocator();
            const main = self.module.main.?;

            self.ip = main + 8;
            self.lv = 0;
            self.lv_len = mem.bytesToValue(
                u32,
                self.module.code.items[main..][0..4],
            );
            self.st = try .initCapacity(allocator, limits.max_constants);

            try self.push(.{ .@"fn" = main });
            try self.pushNTimes(.{ .int = 0 }, self.lv_len - 1);

            while (true) {
                if (mode == .debug) {
                    if (self.debug_tracer) |debug_tracer| {
                        debug_tracer.vtable.step(debug_tracer.ptr, self);
                    }
                }

                const op_code = self.readOpCode();

                switch (op_code) {
                    .constant_u8 => {
                        const index = self.readU8();
                        try self.push(self.constant(index));
                    },

                    .constant_u16 => {
                        const index = self.readU16();
                        try self.push(self.constant(index));
                    },

                    .constant_int_n1 => {
                        try self.push(.{ .int = -1 });
                    },

                    .constant_int_0 => {
                        try self.push(.{ .int = 0 });
                    },

                    .constant_int_1 => {
                        try self.push(.{ .int = 1 });
                    },

                    .constant_int_2 => {
                        try self.push(.{ .int = 2 });
                    },

                    .constant_int_3 => {
                        try self.push(.{ .int = 3 });
                    },

                    .constant_int_4 => {
                        try self.push(.{ .int = 4 });
                    },

                    .constant_int_5 => {
                        try self.push(.{ .int = 5 });
                    },

                    .constant_float_0 => {
                        try self.push(.{ .float = 0 });
                    },

                    .constant_float_1 => {
                        try self.push(.{ .float = 1 });
                    },

                    .constant_float_2 => {
                        try self.push(.{ .float = 2 });
                    },

                    .store_local_u8 => {
                        const index = self.readU8();
                        const a = self.pop();

                        self.storeLocal(index, a);
                    },

                    .store_local_0 => {
                        self.storeLocal(0, self.pop());
                    },

                    .store_local_1 => {
                        self.storeLocal(1, self.pop());
                    },

                    .store_local_2 => {
                        self.storeLocal(2, self.pop());
                    },

                    .store_local_3 => {
                        self.storeLocal(3, self.pop());
                    },

                    .store_local_4 => {
                        self.storeLocal(4, self.pop());
                    },

                    .load_local_u8 => {
                        const index = self.readU8();
                        const value = self.loadLocal(index);

                        try self.push(value);
                    },

                    .load_local_0 => {
                        try self.push(self.loadLocal(0));
                    },

                    .load_local_1 => {
                        try self.push(self.loadLocal(1));
                    },

                    .load_local_2 => {
                        try self.push(self.loadLocal(2));
                    },

                    .load_local_3 => {
                        try self.push(self.loadLocal(3));
                    },

                    .load_local_4 => {
                        try self.push(self.loadLocal(4));
                    },

                    .negate_bool => {
                        try self.push(.{ .bool = !self.pop().bool });
                    },

                    .negate_int => {
                        try self.push(.{ .int = -self.pop().int });
                    },

                    .negate_float => {
                        try self.push(.{ .float = -self.pop().float });
                    },

                    .add_int => {
                        const b = self.pop().int;
                        const a = self.pop().int;

                        try self.push(.{ .int = a + b });
                    },

                    .add_float => {
                        const b = self.pop().float;
                        const a = self.pop().float;

                        try self.push(.{ .float = a + b });
                    },

                    .subtract_int => {
                        const b = self.pop().int;
                        const a = self.pop().int;

                        try self.push(.{ .int = a - b });
                    },

                    .subtract_float => {
                        const b = self.pop().float;
                        const a = self.pop().float;

                        try self.push(.{ .float = a - b });
                    },

                    .multiply_int => {
                        const b = self.pop().int;
                        const a = self.pop().int;

                        try self.push(.{ .int = a * b });
                    },

                    .multiply_float => {
                        const b = self.pop().float;
                        const a = self.pop().float;

                        try self.push(.{ .float = a * b });
                    },

                    .divide_int => {
                        const b = self.pop().int;
                        const a = self.pop().int;

                        try self.push(.{ .int = @divFloor(a, b) });
                    },

                    .divide_float => {
                        const b = self.pop().float;
                        const a = self.pop().float;

                        try self.push(.{ .float = a / b });
                    },

                    .concat => {
                        const b = self.peek(0).object.as(Object(mode).String);
                        const a = self.peek(1).object.as(Object(mode).String);

                        const new_length = a.chars.len + b.chars.len;
                        const new_chars = try allocator.alloc(u8, new_length);
                        @memcpy(new_chars[0..a.chars.len], a.chars);
                        @memcpy(
                            new_chars[a.chars.len..][0..b.chars.len],
                            b.chars,
                        );

                        const string =
                            try Object(mode).String
                                .createTakeOwnership(self.memory, new_chars);

                        _ = self.pop();
                        _ = self.pop();

                        try self.push(.{ .object = &string.object });
                    },

                    .compare_int => {
                        const b = self.pop().int;
                        const a = self.pop().int;

                        try self.push(.{ .int = a - b });
                    },

                    .compare_float => {
                        const b = self.pop().float;
                        const a = self.pop().float;

                        if (a > b) {
                            try self.push(.{ .int = 1 });
                        } else if (a < b) {
                            try self.push(.{ .int = -1 });
                        } else {
                            try self.push(.{ .int = 0 });
                        }
                    },

                    .compare_fn => {
                        const b = self.pop().@"fn";
                        const a = self.pop().@"fn";

                        try self.push(.{ .int = if (a == b) 0 else 1 });
                    },

                    .compare_object => {
                        const b = @intFromPtr(self.pop().object);
                        const a = @intFromPtr(self.pop().object);

                        try self.push(.{ .int = if (a == b) 0 else 1 });
                    },

                    .if_equal => {
                        const offset = self.readU16();
                        const a = self.pop().int;

                        if (a == 0) {
                            self.ip += offset;
                        }
                    },

                    .if_not_equal => {
                        const offset = self.readU16();
                        const a = self.pop().int;

                        if (a != 0) {
                            self.ip += offset;
                        }
                    },

                    .if_greater => {
                        const offset = self.readU16();
                        const a = self.pop().int;

                        if (a > 0) {
                            self.ip += offset;
                        }
                    },

                    .if_greater_equal => {
                        const offset = self.readU16();
                        const a = self.pop().int;

                        if (a >= 0) {
                            self.ip += offset;
                        }
                    },

                    .if_less => {
                        const offset = self.readU16();
                        const a = self.pop().int;

                        if (a < 0) {
                            self.ip += offset;
                        }
                    },

                    .if_less_equal => {
                        const offset = self.readU16();
                        const a = self.pop().int;

                        if (a <= 0) {
                            self.ip += offset;
                        }
                    },

                    .if_true => {
                        const offset = self.readU16();
                        const a = self.pop().bool;

                        if (a) {
                            self.ip += offset;
                        }
                    },

                    .if_false => {
                        const offset = self.readU16();
                        const a = self.pop().bool;

                        if (!a) {
                            self.ip += offset;
                        }
                    },

                    .jump => {
                        const offset = self.readU16();
                        self.ip += offset;
                    },

                    .negative_jump => {
                        const offset = self.readU16();
                        self.ip -= offset;
                    },

                    .assert => {
                        const a = self.pop().bool;

                        if (!a) {
                            return error.Panic;
                        }
                    },

                    .print_unit => {
                        _ = self.pop();
                        output.print("unit\n");
                    },

                    .print_bool => {
                        const value = self.pop();
                        output.printf(
                            "{s}\n",
                            .{if (value.int == 0) "false" else "true"},
                        );
                    },

                    .print_int => {
                        const value = self.pop();
                        output.printf("{}\n", .{value.int});
                    },

                    .print_float => {
                        const value = self.pop();
                        output.printf("{d}\n", .{value.float});
                    },

                    .print_fn => {
                        const value = self.pop();
                        output.printf("<fn {}>\n", .{value.@"fn"});
                    },

                    .print_object => {
                        const value = self.pop();

                        switch (value.object.tag) {
                            .string => {
                                output.printf(
                                    "{s}\n",
                                    .{
                                        value.object.as(
                                            Object(mode).String,
                                        ).chars,
                                    },
                                );
                            },
                        }
                    },

                    .@"return" => {
                        const value = self.pop();

                        if (self.lv == 0) {
                            assert(self.st.items.len == self.lv_len);
                            return;
                        }

                        const link = self.lv + self.lv_len;
                        const to_pop = self.lv_len + 3;

                        self.ip = @intCast(self.st.items[link].int);
                        self.lv = @intCast(self.st.items[link + 1].int);
                        self.lv_len = @intCast(self.st.items[link + 2].int);

                        self.shrink(self.st.items.len - to_pop);

                        try self.push(value);
                    },

                    .call => {
                        const args_count = self.readU8();
                        const value =
                            self.st.items[self.st.items.len - 1 - args_count];
                        const locals_count = mem.bytesToValue(
                            u32,
                            self.module.code.items[value.@"fn"..][0..4],
                        );

                        const prev_ip = self.ip;
                        const prev_lv = self.lv;
                        const prev_lv_len = self.lv_len;

                        self.ip = value.@"fn" + 8;
                        self.lv = self.st.items.len - 1 - args_count;
                        self.lv_len = locals_count;

                        try self.ensureUnusedCapacity(
                            (locals_count - args_count - 1) + 3,
                        );

                        self.pushNTimesAssumeCapacity(
                            .{ .int = 0 },
                            locals_count - args_count - 1,
                        );

                        self.pushAssumeCapacity(.{ .int = @intCast(prev_ip) });
                        self.pushAssumeCapacity(.{ .int = @intCast(prev_lv) });
                        self.pushAssumeCapacity(
                            .{ .int = @intCast(prev_lv_len) },
                        );
                    },

                    .pop => {
                        _ = self.pop();
                    },

                    _ => unreachable,
                }
            }
        }

        fn peek(self: *Self, distance: usize) Value(mode) {
            return self.st.items[self.st.items.len - (distance + 1)];
        }

        fn push(self: *Self, value: Value(mode)) Error!void {
            if (self.st.items.len == self.st.capacity) {
                return self.panic(.stack_overflow);
            }

            self.pushAssumeCapacity(value);
        }

        fn pushAssumeCapacity(self: *Self, value: Value(mode)) void {
            self.st.appendAssumeCapacity(value);
        }

        fn pushNTimes(self: *Self, value: Value(mode), n: usize) Error!void {
            if (self.st.items.len + n > self.st.capacity) {
                return self.panic(.stack_overflow);
            }

            self.pushNTimesAssumeCapacity(value, n);
        }

        fn pushNTimesAssumeCapacity(
            self: *Self,
            value: Value(mode),
            n: usize,
        ) void {
            self.st.appendNTimesAssumeCapacity(value, n);
        }

        fn pop(self: *Self) Value(mode) {
            return self.st.pop().?;
        }

        fn shrink(self: *Self, new_len: usize) void {
            self.st.shrinkRetainingCapacity(new_len);
        }

        fn ensureUnusedCapacity(
            self: *Self,
            additional_count: usize,
        ) Error!void {
            if (self.st.items.len + additional_count > self.st.capacity) {
                return self.panic(.stack_overflow);
            }
        }

        fn storeLocal(self: *Self, index: u32, value: Value(mode)) void {
            self.st.items[self.lv + index] = value;
        }

        fn loadLocal(self: *Self, index: u32) Value(mode) {
            return self.st.items[self.lv + index];
        }

        fn constant(self: *Self, index: u32) Value(mode) {
            return self.module.constants.items[index];
        }

        fn readOpCode(self: *Self) OpCode {
            return @enumFromInt(self.readU8());
        }

        fn readU8(self: *Self) u8 {
            const byte = self.module.code.items[self.ip];
            self.ip += 1;
            return byte;
        }

        fn readU16(self: *Self) u16 {
            const bytes = [_]u8{
                self.module.code.items[self.ip],
                self.module.code.items[self.ip + 1],
            };
            self.ip += 2;
            return @bitCast(bytes);
        }

        fn panic(self: *Self, diag: Diags.Entry.Tag) error{Panic} {
            self.diags.entry = .{
                .tag = diag,
                .loc = self.module.locs.items[self.ip],
            };
            return error.Panic;
        }
    };
}
