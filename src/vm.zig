const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;
const debug = std.debug;
const assert = debug.assert;

const debug_mod = @import("debug.zig");
const Mode = debug_mod.BuildMode;
const limits = @import("limits.zig");
const Loc = @import("tokenizer.zig").Loc;
const memory_mod = @import("memory.zig");
const Value = memory_mod.Value;
const TaggedValue = memory_mod.TaggedValue;
const Object = memory_mod.Object;
const Memory = memory_mod.Memory;
const module_mod = @import("module.zig");
const Module = module_mod.Module;
const OpCode = module_mod.OpCode;
const Output = @import("output.zig").Output;

fn ModeValue(comptime mode: Mode) type {
    return if (mode == .debug) TaggedValue else Value;
}

// todo: underflow/overflow checks
pub const Vm = struct {
    memory: *Memory,
    module: *Module,
    ip: usize,
    lv: usize,
    lv_len: usize,
    st: ArrayList(Value),
    st_tags: ArrayList(Value.DebugTag),
    diags: *Diags,
    debug_tracer: DebugTracer,

    pub const Error = error{Panic} || Allocator.Error;

    pub const Diags = struct {
        entry: ?Entry,

        pub const Entry = struct {
            tag: Tag,
            loc: Loc,

            pub const Tag = union(enum) {
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
            step: *const fn (*const anyopaque, vm: *const Vm) void,
        };
    };

    pub fn interpret(
        memory: *Memory,
        module: *Module,
        output: *const Output,
        diags: *Diags,
        debug_tracer_opt: ?DebugTracer,
    ) Error!void {
        const allocator = memory.allocator();

        var vm = Vm{
            .memory = memory,
            .module = module,
            .ip = undefined,
            .lv = undefined,
            .lv_len = undefined,
            .st = .empty,
            .st_tags = .empty,
            .diags = diags,
            .debug_tracer = debug_tracer_opt orelse undefined,
        };
        defer {
            vm.st.deinit(allocator);
            vm.st_tags.deinit(allocator);
        }

        if (debug_tracer_opt != null) {
            try vm.run(output, .debug);
        } else {
            try vm.run(output, .release);
        }
    }

    fn run(
        self: *Vm,
        output: *const Output,
        comptime mode: Mode,
    ) Error!void {
        const allocator = self.memory.allocator();
        const main = self.module.main.?;

        self.ip = main + 8;
        self.lv = 0;
        self.lv_len = mem.bytesToValue(
            u32,
            self.module.code.items[main..][0..4],
        );
        self.st = try .initCapacity(allocator, limits.max_constants);

        if (mode == .debug) {
            self.st_tags = try .initCapacity(allocator, self.st.capacity);
        }

        try self.push(mode, .{ .@"fn" = main });
        try self.pushNTimes(mode, .{ .int = 0 }, self.lv_len - 1);

        while (true) {
            if (mode == .debug) {
                assert(self.st.items.len == self.st_tags.items.len);
                self.debug_tracer.vtable.step(self.debug_tracer.ptr, self);
            }

            const op_code = self.readOpCode();

            switch (op_code) {
                .constant_u8 => {
                    const index = self.readU8();
                    try self.push(mode, self.constant(mode, index));
                },

                .constant_u16 => {
                    const index = self.readU16();
                    try self.push(mode, self.constant(mode, index));
                },

                .constant_int_n1 => {
                    try self.push(mode, .{ .int = -1 });
                },

                .constant_int_0 => {
                    try self.push(mode, .{ .int = 0 });
                },

                .constant_int_1 => {
                    try self.push(mode, .{ .int = 1 });
                },

                .constant_int_2 => {
                    try self.push(mode, .{ .int = 2 });
                },

                .constant_int_3 => {
                    try self.push(mode, .{ .int = 3 });
                },

                .constant_int_4 => {
                    try self.push(mode, .{ .int = 4 });
                },

                .constant_int_5 => {
                    try self.push(mode, .{ .int = 5 });
                },

                .constant_float_0 => {
                    try self.push(mode, .{ .float = 0 });
                },

                .constant_float_1 => {
                    try self.push(mode, .{ .float = 1 });
                },

                .constant_float_2 => {
                    try self.push(mode, .{ .float = 2 });
                },

                .store_local_u8 => {
                    const index = self.readU8();
                    const a = self.pop(mode);

                    self.storeLocal(mode, index, a);
                },

                .store_local_0 => {
                    self.storeLocal(mode, 0, self.pop(mode));
                },

                .store_local_1 => {
                    self.storeLocal(mode, 1, self.pop(mode));
                },

                .store_local_2 => {
                    self.storeLocal(mode, 2, self.pop(mode));
                },

                .store_local_3 => {
                    self.storeLocal(mode, 3, self.pop(mode));
                },

                .store_local_4 => {
                    self.storeLocal(mode, 4, self.pop(mode));
                },

                .load_local_u8 => {
                    const index = self.readU8();
                    const value = self.loadLocal(mode, index);

                    try self.push(mode, value);
                },

                .load_local_0 => {
                    try self.push(mode, self.loadLocal(mode, 0));
                },

                .load_local_1 => {
                    try self.push(mode, self.loadLocal(mode, 1));
                },

                .load_local_2 => {
                    try self.push(mode, self.loadLocal(mode, 2));
                },

                .load_local_3 => {
                    try self.push(mode, self.loadLocal(mode, 3));
                },

                .load_local_4 => {
                    try self.push(mode, self.loadLocal(mode, 4));
                },

                .negate_bool => {
                    try self.push(mode, .{ .bool = !self.pop(mode).bool });
                },

                .negate_int => {
                    try self.push(mode, .{ .int = -self.pop(mode).int });
                },

                .negate_float => {
                    try self.push(mode, .{ .float = -self.pop(mode).float });
                },

                .add_int => {
                    const b = self.pop(mode).int;
                    const a = self.pop(mode).int;

                    try self.push(mode, .{ .int = a + b });
                },

                .add_float => {
                    const b = self.pop(mode).float;
                    const a = self.pop(mode).float;

                    try self.push(mode, .{ .float = a + b });
                },

                .subtract_int => {
                    const b = self.pop(mode).int;
                    const a = self.pop(mode).int;

                    try self.push(mode, .{ .int = a - b });
                },

                .subtract_float => {
                    const b = self.pop(mode).float;
                    const a = self.pop(mode).float;

                    try self.push(mode, .{ .float = a - b });
                },

                .multiply_int => {
                    const b = self.pop(mode).int;
                    const a = self.pop(mode).int;

                    try self.push(mode, .{ .int = a * b });
                },

                .multiply_float => {
                    const b = self.pop(mode).float;
                    const a = self.pop(mode).float;

                    try self.push(mode, .{ .float = a * b });
                },

                .divide_int => {
                    const b = self.pop(mode).int;
                    const a = self.pop(mode).int;

                    try self.push(mode, .{ .int = @divFloor(a, b) });
                },

                .divide_float => {
                    const b = self.pop(mode).float;
                    const a = self.pop(mode).float;

                    try self.push(mode, .{ .float = a / b });
                },

                .concat => {
                    const b = self.peek(0).object.as(Object.String);
                    const a = self.peek(1).object.as(Object.String);

                    const new_length = a.chars.len + b.chars.len;
                    const new_chars = try allocator.alloc(u8, new_length);
                    @memcpy(new_chars[0..a.chars.len], a.chars);
                    @memcpy(new_chars[a.chars.len..][0..b.chars.len], b.chars);

                    const string = try Object.String.createTakeOwnership(
                        self.memory,
                        new_chars,
                    );

                    _ = self.pop(mode);
                    _ = self.pop(mode);

                    try self.push(mode, .{ .object = &string.object });
                },

                .compare_int => {
                    const b = self.pop(mode).int;
                    const a = self.pop(mode).int;

                    try self.push(mode, .{ .int = a - b });
                },

                .compare_float => {
                    const b = self.pop(mode).float;
                    const a = self.pop(mode).float;

                    if (a > b) {
                        try self.push(mode, .{ .int = 1 });
                    } else if (a < b) {
                        try self.push(mode, .{ .int = -1 });
                    } else {
                        try self.push(mode, .{ .int = 0 });
                    }
                },

                .compare_fn => {
                    const b = self.pop(mode).@"fn";
                    const a = self.pop(mode).@"fn";

                    try self.push(mode, .{ .int = if (a == b) 0 else 1 });
                },

                .compare_object => {
                    const b = @intFromPtr(self.pop(mode).object);
                    const a = @intFromPtr(self.pop(mode).object);

                    try self.push(mode, .{ .int = if (a == b) 0 else 1 });
                },

                .if_equal => {
                    const offset = self.readU16();
                    const a = self.pop(mode).int;

                    if (a == 0) {
                        self.ip += offset;
                    }
                },

                .if_not_equal => {
                    const offset = self.readU16();
                    const a = self.pop(mode).int;

                    if (a != 0) {
                        self.ip += offset;
                    }
                },

                .if_greater => {
                    const offset = self.readU16();
                    const a = self.pop(mode).int;

                    if (a > 0) {
                        self.ip += offset;
                    }
                },

                .if_greater_equal => {
                    const offset = self.readU16();
                    const a = self.pop(mode).int;

                    if (a >= 0) {
                        self.ip += offset;
                    }
                },

                .if_less => {
                    const offset = self.readU16();
                    const a = self.pop(mode).int;

                    if (a < 0) {
                        self.ip += offset;
                    }
                },

                .if_less_equal => {
                    const offset = self.readU16();
                    const a = self.pop(mode).int;

                    if (a <= 0) {
                        self.ip += offset;
                    }
                },

                .if_true => {
                    const offset = self.readU16();
                    const a = self.pop(mode).bool;

                    if (a) {
                        self.ip += offset;
                    }
                },

                .if_false => {
                    const offset = self.readU16();
                    const a = self.pop(mode).bool;

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
                    const a = self.pop(mode).bool;

                    if (!a) {
                        return error.Panic;
                    }
                },

                .print_unit => {
                    _ = self.pop(mode);
                    output.print("unit");
                },

                .print_bool => {
                    const value = self.pop(mode);
                    output.printf(
                        "{s}\n",
                        .{if (value.int == 0) "false" else "true"},
                    );
                },

                .print_int => {
                    const value = self.pop(mode);
                    output.printf("{}\n", .{value.int});
                },

                .print_float => {
                    const value = self.pop(mode);
                    output.printf("{d}\n", .{value.float});
                },

                .print_fn => {
                    const value = self.pop(mode);
                    output.printf("<fn {}>", .{value.@"fn"});
                },

                .print_object => {
                    const value = self.pop(mode);

                    switch (value.object.tag) {
                        .string => {
                            output.printf(
                                "{s}\n",
                                .{value.object.as(Object.String).chars},
                            );
                        },
                    }
                },

                .@"return" => {
                    const value = self.pop(mode);

                    if (self.lv == 0) {
                        assert(self.st.items.len == self.lv_len);
                        return;
                    }

                    const link = self.lv + self.lv_len;
                    const to_pop = self.lv_len + 3;

                    self.ip = @intCast(self.st.items[link].int);
                    self.lv = @intCast(self.st.items[link + 1].int);
                    self.lv_len = @intCast(self.st.items[link + 2].int);

                    self.shrink(mode, self.st.items.len - to_pop);

                    try self.push(mode, value);
                },

                .call => {
                    const args_count = self.readU8();
                    const value = self.st.items[self.st.items.len - 1 - args_count];
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
                        mode,
                        .{ .int = 0 },
                        locals_count - args_count - 1,
                    );

                    self.pushAssumeCapacity(
                        mode,
                        .{ .int = @intCast(prev_ip) },
                    );
                    self.pushAssumeCapacity(
                        mode,
                        .{ .int = @intCast(prev_lv) },
                    );
                    self.pushAssumeCapacity(
                        mode,
                        .{ .int = @intCast(prev_lv_len) },
                    );
                },

                .pop => {
                    _ = self.pop(mode);
                },

                _ => unreachable,
            }
        }
    }

    fn peek(self: *Vm, distance: usize) Value {
        return self.st.items[self.st.items.len - (distance + 1)];
    }

    fn push(
        self: *Vm,
        comptime mode: Mode,
        value: ModeValue(mode),
    ) Error!void {
        if (self.st.items.len == self.st.capacity) {
            return self.panic(.stack_overflow);
        }

        self.pushAssumeCapacity(mode, value);
    }

    fn pushAssumeCapacity(
        self: *Vm,
        comptime mode: Mode,
        value: ModeValue(mode),
    ) void {
        if (mode == .debug) {
            const sep_value, const tag = value.separate();
            self.st.appendAssumeCapacity(sep_value);
            self.st_tags.appendAssumeCapacity(tag);
        } else {
            self.st.appendAssumeCapacity(value);
        }
    }

    fn pushNTimes(
        self: *Vm,
        comptime mode: Mode,
        value: ModeValue(mode),
        n: usize,
    ) Error!void {
        if (self.st.items.len + n > self.st.capacity) {
            return self.panic(.stack_overflow);
        }

        self.pushNTimesAssumeCapacity(mode, value, n);
    }

    fn pushNTimesAssumeCapacity(
        self: *Vm,
        comptime mode: Mode,
        value: ModeValue(mode),
        n: usize,
    ) void {
        if (mode == .debug) {
            const sep_value, const tag = value.separate();
            self.st.appendNTimesAssumeCapacity(sep_value, n);
            self.st_tags.appendNTimesAssumeCapacity(tag, n);
        } else {
            self.st.appendNTimesAssumeCapacity(value, n);
        }
    }

    fn pop(self: *Vm, comptime mode: Mode) ModeValue(mode) {
        if (mode == .debug) {
            return .from(self.st.pop().?, self.st_tags.pop().?);
        } else {
            return self.st.pop().?;
        }
    }

    fn shrink(self: *Vm, comptime mode: Mode, new_len: usize) void {
        self.st.shrinkRetainingCapacity(new_len);

        if (mode == .debug) {
            self.st_tags.shrinkRetainingCapacity(new_len);
        }
    }

    fn ensureUnusedCapacity(self: *Vm, additional_count: usize) Error!void {
        if (self.st.items.len + additional_count > self.st.capacity) {
            return self.panic(.stack_overflow);
        }
    }

    fn storeLocal(
        self: *Vm,
        comptime mode: Mode,
        index: u32,
        value: ModeValue(mode),
    ) void {
        const new_index = self.lv + index;

        if (mode == .debug) {
            const sep_value, const tag = value.separate();
            self.st.items[new_index] = sep_value;
            self.st_tags.items[new_index] = tag;
        } else {
            self.st.items[new_index] = value;
        }
    }

    fn loadLocal(self: *Vm, comptime mode: Mode, index: u32) ModeValue(mode) {
        const new_index = self.lv + index;

        return if (mode == .debug)
            .from(
                self.st.items[new_index],
                self.st_tags.items[new_index],
            )
        else
            return self.st.items[new_index];
    }

    fn constant(self: *Vm, comptime mode: Mode, index: u32) ModeValue(mode) {
        return if (mode == .debug)
            .from(
                self.module.constants.items[index],
                self.module.constant_tags.items[index],
            )
        else
            self.module.constants.items[index];
    }

    fn readOpCode(self: *Vm) OpCode {
        return @enumFromInt(self.readU8());
    }

    fn readU8(self: *Vm) u8 {
        const byte = self.module.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    fn readU16(self: *Vm) u16 {
        const bytes = [_]u8{
            self.module.code.items[self.ip],
            self.module.code.items[self.ip + 1],
        };
        self.ip += 2;
        return @bitCast(bytes);
    }

    fn panic(self: *Vm, diag: Diags.Entry.Tag) error{Panic} {
        self.diags.entry = .{
            .tag = diag,
            .loc = self.module.locs.items[self.ip],
        };
        return error.Panic;
    }
};
