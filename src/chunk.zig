const std = @import("std");
const managed_memory_mod = @import("managed_memory.zig");
const value_mod = @import("value.zig");
const tokenizer_mod = @import("tokenizer.zig");
const io_handler_mod = @import("io_handler.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ascii = std.ascii;
const expect = std.testing.expect;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const Value = value_mod.Value;
const Position = tokenizer_mod.Position;
const IoHandler = io_handler_mod.IoHandler;

const ChunkError = error{
    OutOfMemory,
    TooManyConstants,
};

pub const OpCode = enum(u8) {
    constant,
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
    positions: ArrayList(?Position), // todo: replace with RLE
    constants: ArrayList(Value),

    pub fn init(memory: *ManagedMemory) ChunkError!Self {
        const allocator = memory.allocator();

        return .{
            .memory = memory,
            .allocator = allocator,
            .code = ArrayList(u8).init(allocator),
            .positions = ArrayList(?Position).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.positions.deinit();
        self.constants.deinit();
    }

    pub fn writeByte(self: *Self, byte: anytype, position: ?Position) ChunkError!void {
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

        try self.positions.append(position);
    }

    pub fn writeConstant(self: *Self, value: Value, position: ?Position) ChunkError!void {
        if (self.constants.items.len == 256) {
            return error.TooManyConstants;
        }

        const index = self.constants.items.len;

        try self.constants.append(value);
        try self.writeByte(.constant, position);
        try self.writeByte(@as(u8, @intCast(index)), position);
    }

    pub fn print(self: *Self, io: *IoHandler) void {
        var index: usize = 0;

        while (index < self.code.items.len) {
            io.outf("{:0>4} ", .{index});

            if (self.positions.items[index]) |position| {
                io.outf("{: >4}:{: <4} ", .{ position.line, position.column });
            } else {
                io.out("          ");
            }

            const byte = self.code.items[index];
            const op_code = @as(OpCode, @enumFromInt(byte));

            index += switch (op_code) {
                .constant => self.printConstantInstruction(io, index),
                .negate,
                .add,
                .subtract,
                .multiply,
                .divide,
                .return_,
                .pop,
                => self.printInstruction(io, index),
                _ => @panic("unknown instruction"),
            };
        }
    }

    fn printInstruction(self: *const Self, io: *IoHandler, offset: usize) usize {
        const byte = self.code.items[offset];
        const op_code = @as(OpCode, @enumFromInt(byte));

        Self.printOpCode(op_code, io);
        io.out("\n");

        return 1;
    }

    fn printConstantInstruction(self: *const Self, io: *IoHandler, offset: usize) usize {
        const byte = self.code.items[offset];
        const op_code = @as(OpCode, @enumFromInt(byte));
        const index = self.code.items[offset + 1];

        Self.printOpCode(op_code, io);
        io.outf(" {: <4} '", .{index});
        self.constants.items[index].print(io);
        io.out("'\n");

        return 2;
    }

    fn printOpCode(op_code: OpCode, io: *IoHandler) void {
        const tag_name = switch (op_code) {
            .return_ => "return",
            else => @tagName(op_code),
        };

        var fill: u8 = 16;

        for (tag_name) |char| {
            io.outf("{c}", .{ascii.toUpper(char)});

            if (fill > 0) {
                fill -= 1;
            }
        }

        for (0..fill) |_| {
            io.out(" ");
        }
    }
};

test "writeByte works for all supported types" {
    // GIVEN
    var memory = ManagedMemory.init(std.testing.allocator);
    var chunk = try Chunk.init(&memory);
    defer chunk.deinit();

    // WHEN
    try chunk.writeByte(0, .{ .line = 1, .column = 1 });
    try chunk.writeByte(@as(u8, 1), .{ .line = 2, .column = 2 });
    try chunk.writeByte(.return_, .{ .line = 3, .column = 3 });
    try chunk.writeByte(.pop, .{ .line = 4, .column = 4 });

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
    var chunk = try Chunk.init(&memory);
    defer chunk.deinit();

    // WHEN
    try chunk.writeConstant(10, .{ .line = 1, .column = 1 });
    try chunk.writeConstant(20, .{ .line = 2, .column = 2 });

    // THEN
    try expect(chunk.constants.items.len == 2);

    try expect(chunk.constants.items[0] == 10);
    try expect(chunk.code.items[0] == @intFromEnum(OpCode.constant));
    try expect(chunk.code.items[1] == 0);
    try expect(chunk.positions.items[0].?.line == 1);
    try expect(chunk.positions.items[0].?.column == 1);
    try expect(chunk.positions.items[1].?.line == 1);
    try expect(chunk.positions.items[1].?.column == 1);

    try expect(chunk.constants.items[1] == 20);
    try expect(chunk.code.items[2] == @intFromEnum(OpCode.constant));
    try expect(chunk.code.items[3] == 1);
    try expect(chunk.positions.items[2].?.line == 2);
    try expect(chunk.positions.items[2].?.column == 2);
    try expect(chunk.positions.items[3].?.line == 2);
    try expect(chunk.positions.items[3].?.column == 2);
}
