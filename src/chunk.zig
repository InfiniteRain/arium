const std = @import("std");
const managed_memory_mod = @import("managed_memory.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const expect = std.testing.expect;
const ManagedMemory = managed_memory_mod.ManagedMemory;

const ChunkError = error{
    OutOfMemory,
};

pub const OpCode = enum(u8) {
    negate,
    add,
    subtract,
    multiply,
    divide,
    return_,
    pop,
    _,
};

pub const Chunk = struct {
    const Self = @This();

    memory: *ManagedMemory,
    allocator: Allocator,
    code: ArrayList(u8),
    lines: ArrayList(u64), // todo: replace with RLE

    pub fn init(memory: *ManagedMemory) ChunkError!Self {
        const allocator = memory.allocator();

        const code = ArrayList(u8).init(allocator);
        const lines = ArrayList(u64).init(allocator);

        return .{
            .memory = memory,
            .allocator = allocator,
            .code = code,
            .lines = lines,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
    }

    pub fn writeByte(self: *Self, byte: anytype, line: u64) ChunkError!void {
        const ByteType = @TypeOf(byte);

        switch (ByteType) {
            @TypeOf(.enum_literal), OpCode => {
                if (ByteType == @TypeOf(.enum_literal) and !@hasField(OpCode, @tagName(byte))) {
                    @compileError("expected valid OpCode");
                }

                try self.code.append(@intFromEnum(@as(OpCode, byte)));
            },
            comptime_int, u8 => {
                try self.code.append(byte);
            },
            else => {
                @compileError("expected byte to be of type OpCode or u8, found " ++ @typeName(ByteType));
            },
        }

        try self.lines.append(line);
    }
};

test "writeByte works for all supported types" {
    // GIVEN
    var memory = ManagedMemory.init(std.testing.allocator);
    var chunk = try Chunk.init(&memory);
    defer chunk.deinit();

    // WHEN
    try chunk.writeByte(0, 0);
    try chunk.writeByte(@as(u8, 1), 1);
    try chunk.writeByte(.return_, 2);
    try chunk.writeByte(.pop, 3);

    // THEN
    try expect(chunk.code.items[0] == 0);
    try expect(chunk.lines.items[0] == 0);
    try expect(chunk.code.items[1] == 1);
    try expect(chunk.lines.items[1] == 1);
    try expect(chunk.code.items[2] == @intFromEnum(OpCode.return_));
    try expect(chunk.lines.items[2] == 2);
    try expect(chunk.code.items[3] == @intFromEnum(OpCode.pop));
    try expect(chunk.lines.items[3] == 3);
}
