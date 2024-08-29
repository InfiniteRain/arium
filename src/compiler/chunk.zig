const std = @import("std");
const shared = @import("shared");
const managed_memory_mod = @import("../state/managed_memory.zig");
const value_mod = @import("../state/value.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const value_reporter = @import("../reporter/value_reporter.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const math = std.math;
const expect = std.testing.expect;
const assert = std.debug.assert;
const Writer = shared.Writer;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const Value = value_mod.Value;
const Position = tokenizer_mod.Position;

pub const OpCode = enum(u8) {
    constant,
    constant_unit,
    constant_bool_false,
    constant_bool_true,
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
    compare_bool,
    compare_obj,
    if_equal,
    if_not_equal,
    if_greater,
    if_greater_equal,
    if_less,
    if_less_equal,
    if_true,
    if_false,
    jump,

    assert,
    print,
    return_,
    pop,
    _,
};

pub const Chunk = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
    };

    allocator: Allocator,
    code: ArrayList(u8),
    positions: ArrayList(Position), // todo: replace with RLE
    constants: ArrayList(Value),
    last_code_len: usize = 0,
    current_op_code: OpCode = undefined,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .code = ArrayList(u8).init(allocator),
            .positions = ArrayList(Position).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.positions.deinit();
        self.constants.deinit();
    }

    pub fn writeU8(self: *Self, data: anytype, position: Position) Error!void {
        const byte, const is_op_code = resolveU8(data);

        if (is_op_code) {
            self.last_code_len = self.code.items.len;
            self.current_op_code = @enumFromInt(byte);
        }

        try self.code.append(byte);
        try self.positions.append(position);
    }

    pub fn updateU8(self: *Self, data: anytype, index: usize) Error!void {
        self.code.items[index] = resolveU8(data)[0];
    }

    pub fn writeU16(self: *Self, data: u16, position: Position) Error!void {
        try self.writeU8(@as(u8, @intCast((data >> 8) & 0xFF)), position);
        try self.writeU8(@as(u8, @intCast(data & 0xFF)), position);
    }

    pub fn writeJump(
        self: *Self,
        op_code: OpCode,
        position: Position,
    ) Error!usize {
        try self.writeU8(op_code, position);
        try self.writeU16(0, position);

        return self.code.items.len - 2;
    }

    pub fn patchJump(self: *Self, offset: usize) error{JumpTooBig}!void {
        const jump = self.code.items.len - offset - 2;

        if (offset > math.maxInt(u16)) {
            return error.JumpTooBig;
        }

        const jump_converted: u16 = @intCast(jump);

        self.code.items[offset] = @intCast((jump_converted >> 8) & 0xFF);
        self.code.items[offset + 1] = @intCast(jump_converted & 0xFF);
    }

    pub fn writeConstant(self: *Self, value: Value, position: Position) error{
        TooManyConstants,
        OutOfMemory,
    }!void {
        if (self.constants.items.len == 256) {
            return error.TooManyConstants;
        }

        const index = self.constants.items.len;

        try self.constants.append(value);
        try self.writeU8(.constant, position);
        try self.writeU8(@as(u8, @intCast(index)), position);
    }

    pub fn eraseLast(self: *Self) error{OutOfMemory}!void {
        assert(self.code.items.len != self.last_code_len); // called twice in a row

        if (self.current_op_code == .constant) {
            _ = self.constants.pop();
        }

        try self.code.resize(self.last_code_len);
        try self.positions.resize(self.last_code_len);
    }

    pub fn readU8(self: *const Self, offset: usize) u8 {
        return self.code.items[offset];
    }

    pub fn readU16(self: *const Self, offset: usize) u16 {
        const left: u16 = self.code.items[offset];
        const right = self.code.items[offset + 1];
        return (left << 8) | right;
    }

    fn resolveU8(data: anytype) struct { u8, bool } {
        const ByteType = @TypeOf(data);

        switch (ByteType) {
            @TypeOf(.enum_literal), OpCode => {
                if (ByteType == @TypeOf(.enum_literal) and !@hasField(OpCode, @tagName(data))) {
                    @compileError("expected valid OpCode");
                }

                return .{ @intFromEnum(@as(OpCode, data)), true };
            },
            comptime_int, u8 => {
                return .{ data, false };
            },
            else => {
                @compileError("expected byte to be of type OpCode or u8, found " ++ @typeName(ByteType));
            },
        }
    }
};

test "writeByte works for all supported types" {
    // GIVEN
    var memory = ManagedMemory.init(std.testing.allocator);
    var chunk = Chunk.init(memory.allocator());
    defer chunk.deinit();

    // WHEN
    try chunk.writeU8(0, .{ .line = 1, .column = 1 });
    try chunk.writeU8(@as(u8, 1), .{ .line = 2, .column = 2 });
    try chunk.writeU8(.return_, .{ .line = 3, .column = 3 });
    try chunk.writeU8(.pop, .{ .line = 4, .column = 4 });

    // THEN
    try expect(chunk.code.items[0] == 0);
    try expect(chunk.positions.items[0].?.line == 1);
    try expect(chunk.positions.items[0].?.column == 1);
    try expect(chunk.code.items[1] == 1);
    try expect(chunk.positions.items[1].?.line == 2);
    try expect(chunk.positions.items[1].?.column == 2);
    try expect(chunk.code.items[2] == @intFromEnum(OpCode.return_));
    try expect(chunk.positions.items[2].?.line == 3);
    try expect(chunk.positions.items[2].?.column == 3);
    try expect(chunk.code.items[3] == @intFromEnum(OpCode.pop));
    try expect(chunk.positions.items[3].?.line == 4);
    try expect(chunk.positions.items[3].?.column == 4);
}

test "writeConstant should work" {
    // GIVEN
    var memory = ManagedMemory.init(std.testing.allocator);
    var chunk = Chunk.init(memory.allocator());
    defer chunk.deinit();

    // WHEN
    try chunk.writeConstant(.{ .int = 10 }, .{ .line = 1, .column = 1 });
    try chunk.writeConstant(.{ .int = 20 }, .{ .line = 2, .column = 2 });

    // THEN
    try expect(chunk.constants.items.len == 2);

    try expect(chunk.constants.items[0].int == 10);
    try expect(chunk.code.items[0] == @intFromEnum(OpCode.constant));
    try expect(chunk.code.items[1] == 0);
    try expect(chunk.positions.items[0].?.line == 1);
    try expect(chunk.positions.items[0].?.column == 1);
    try expect(chunk.positions.items[1].?.line == 1);
    try expect(chunk.positions.items[1].?.column == 1);

    try expect(chunk.constants.items[1].int == 20);
    try expect(chunk.code.items[2] == @intFromEnum(OpCode.constant));
    try expect(chunk.code.items[3] == 1);
    try expect(chunk.positions.items[2].?.line == 2);
    try expect(chunk.positions.items[2].?.column == 2);
    try expect(chunk.positions.items[3].?.line == 2);
    try expect(chunk.positions.items[3].?.column == 2);
}
