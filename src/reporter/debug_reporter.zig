const std = @import("std");
const shared = @import("shared");
const chunk_mod = @import("../compiler/chunk.zig");
const value_reporter = @import("../reporter/value_reporter.zig");

const Writer = shared.Writer;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;

pub fn reportChunk(chunk: *const Chunk, writer: *const Writer) void {
    var index: usize = 0;

    while (index < chunk.code.items.len) {
        index += reportInstruction(chunk, writer, index);
    }
}

pub fn reportInstructionName(
    chunk: *const Chunk,
    writer: *const Writer,
    offset: usize,
) usize {
    const byte = chunk.readU8(offset);
    const op_code: OpCode = @enumFromInt(byte);

    reportOpCode(op_code, writer);
    writer.print("\n");

    return 1;
}

pub fn reportConstantInstructionName(
    chunk: *const Chunk,
    writer: *const Writer,
    offset: usize,
) usize {
    const byte = chunk.readU8(offset);
    const op_code: OpCode = @enumFromInt(byte);
    const index = chunk.readU8(offset + 1);

    reportOpCode(op_code, writer);
    writer.printf(" {: <4} '", .{index});
    value_reporter.printValue(chunk.constants.items[index], writer);
    writer.print("'\n");

    return 2;
}

pub fn reportJumpInstructionName(
    chunk: *const Chunk,
    writer: *const Writer,
    offset: usize,
) usize {
    const byte = chunk.readU8(offset);
    const op_code: OpCode = @enumFromInt(byte);
    const jump_offset = chunk.readU16(offset + 1);

    reportOpCode(op_code, writer);
    writer.printf(" to {}\n", .{offset + jump_offset + 3});

    return 3;
}

pub fn reportOpCode(op_code: OpCode, writer: *const Writer) void {
    const tag_name = switch (op_code) {
        .return_ => "return",
        else => @tagName(op_code),
    };

    var fill: u8 = 24;

    for (tag_name) |char| {
        writer.printf("{c}", .{std.ascii.toUpper(char)});

        if (fill > 0) {
            fill -= 1;
        }
    }

    for (0..fill) |_| {
        writer.print(" ");
    }
}

pub fn reportInstruction(
    chunk: *const Chunk,
    writer: *const Writer,
    offset: usize,
) usize {
    writer.printf("{:0>4} ", .{offset});

    if (chunk.positions.items[offset]) |position| {
        writer.printf("{: >4}:{: <4} ", .{ position.line, position.column });
    } else {
        writer.print("          ");
    }

    const byte = chunk.code.items[offset];
    const op_code = @as(OpCode, @enumFromInt(byte));

    return switch (op_code) {
        .constant => reportConstantInstructionName(chunk, writer, offset),

        .constant_bool_false,
        .constant_bool_true,
        .constant_int_n1,
        .constant_int_0,
        .constant_int_1,
        .constant_int_2,
        .constant_int_3,
        .constant_int_4,
        .constant_int_5,
        .constant_float_0,
        .constant_float_1,
        .constant_float_2,
        .negate_bool,
        .negate_int,
        .negate_float,
        .add_int,
        .add_float,
        .subtract_int,
        .subtract_float,
        .multiply_int,
        .multiply_float,
        .divide_int,
        .divide_float,
        .concat,
        .compare_int,
        .compare_float,
        .compare_bool,
        .compare_obj,
        .assert,
        .print,
        .return_,
        .pop,
        => reportInstructionName(chunk, writer, offset),

        .if_equal,
        .if_not_equal,
        .if_greater,
        .if_greater_equal,
        .if_less,
        .if_less_equal,
        .if_true,
        .if_false,
        .jump,
        => reportJumpInstructionName(chunk, writer, offset),

        _ => @panic("unknown instruction"),
    };
}
