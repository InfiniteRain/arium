const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const math = std.math;

const debug_mod = @import("debug.zig");
const Mode = debug_mod.Mode;
const limits = @import("limits.zig");
const Loc = @import("tokenizer.zig").Loc;
const memory_mod = @import("memory.zig");
const Value = memory_mod.Value;
const TaggedValue = memory_mod.TaggedValue;
const Object = memory_mod.Object;
const Memory = memory_mod.Memory;

pub const OpCode = enum(u8) {
    constant_u8,
    constant_u16,
    constant_int_n1,
    constant_int_0,
    constant_int_1,
    constant_int_2,
    constant_int_3,
    constant_int_4,
    constant_int_5,
    constant_float_0,
    constant_float_1,
    constant_float_2,

    store_local_u8,
    store_local_0,
    store_local_1,
    store_local_2,
    store_local_3,
    store_local_4,
    load_local_u8,
    load_local_0,
    load_local_1,
    load_local_2,
    load_local_3,
    load_local_4,

    negate_bool,
    negate_int,
    negate_float,
    add_int,
    add_float,
    subtract_int,
    subtract_float,
    multiply_int,
    multiply_float,
    divide_int,
    divide_float,

    concat,

    compare_int,
    compare_float,
    compare_fn,
    compare_object,
    if_equal,
    if_not_equal,
    if_greater,
    if_greater_equal,
    if_less,
    if_less_equal,
    if_true,
    if_false,
    jump,
    negative_jump,

    assert,
    print_unit,
    print_int,
    print_float,
    print_bool,
    print_fn,
    print_object,
    call,
    @"return",
    pop,
    _,
};

pub const Module = struct {
    constants: ArrayListUnmanaged(Value),
    constant_tags: ArrayListUnmanaged(Value.DebugTag),
    code: ArrayListUnmanaged(u8),
    locs: ArrayListUnmanaged(Loc),
    main: ?u64,

    pub const empty: Module = .{
        .constants = .empty,
        .constant_tags = .empty,
        .code = .empty,
        .locs = .empty,
        .main = 0,
    };

    pub fn deinit(self: *Module, allocator: Allocator) void {
        self.constants.deinit(allocator);
        self.code.deinit(allocator);
        self.locs.deinit(allocator);
    }

    pub fn writeConstant(
        self: *Module,
        comptime mode: Mode,
        allocator: Allocator,
        constant: if (mode == .debug) TaggedValue else Value,
    ) (error{TooManyConstants} || Allocator.Error)!usize {
        if (self.constants.items.len == limits.max_constants) {
            return error.TooManyConstants;
        }

        if (mode == .debug) {
            const value, const tag = constant.separate();
            try self.constants.append(allocator, value);
            try self.constant_tags.append(allocator, tag);
        } else {
            try self.constants.append(allocator, constant);
        }

        return self.constants.items.len - 1;
    }

    pub fn writeFn(
        self: *Module,
        allocator: Allocator,
        locals_count: u32,
        body: []const u8,
        locs: []const Loc,
    ) (Allocator.Error || error{BodyTooBig})!u64 {
        assert(body.len == locs.len);

        if (body.len > math.maxInt(u32)) {
            return error.BodyTooBig;
        }

        const locals_count_bytes: [4]u8 = @bitCast(locals_count);
        const len: u32 = @intCast(body.len);
        const size_bytes: [4]u8 = @bitCast(len);
        const top = self.code.items.len;
        const additional_capacity =
            @sizeOf(@TypeOf(locals_count_bytes)) +
            @sizeOf(@TypeOf(size_bytes)) +
            body.len;

        try self.code.ensureUnusedCapacity(allocator, additional_capacity);
        try self.locs.ensureUnusedCapacity(allocator, additional_capacity);

        self.code.appendSliceAssumeCapacity(
            &(locals_count_bytes ++ size_bytes),
        );
        self.code.appendSliceAssumeCapacity(body);

        self.locs.appendSliceAssumeCapacity(&([_]Loc{.zero} ** 8));
        self.locs.appendSliceAssumeCapacity(locs);

        return @intCast(top);
    }
};
