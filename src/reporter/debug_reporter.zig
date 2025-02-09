const std = @import("std");
const shared = @import("shared");
const chunk_mod = @import("../compiler/chunk.zig");
const value_mod = @import("../state/value.zig");
const value_reporter = @import("../reporter/value_reporter.zig");

const Writer = shared.Writer;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Value = value_mod.Value;

pub fn reportChunk(
    chunk: *const Chunk,
    source: []const u8,
    writer: *const Writer,
) void {
    var index: usize = 0;

    while (index < chunk.code.items.len) {
        index += reportInstruction(chunk, index, source, writer);
    }
}

pub fn reportInstructionName(
    chunk: *const Chunk,
    offset: usize,
    writer: *const Writer,
) usize {
    const byte = chunk.readU8(offset);
    const op_code: OpCode = @enumFromInt(byte);

    reportOpCode(op_code, writer);
    writer.print("\n");

    return 1;
}

pub fn reportByteInstruction(
    chunk: *const Chunk,
    offset: usize,
    writer: *const Writer,
) usize {
    const byte = chunk.readU8(offset);
    const op_code: OpCode = @enumFromInt(byte);
    const arg = chunk.readU8(offset + 1);

    reportOpCode(op_code, writer);
    writer.printf(" {: <4}\n", .{arg});

    return 2;
}

pub fn reportConstantInstructionName(
    chunk: *const Chunk,
    offset: usize,
    writer: *const Writer,
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
    offset: usize,
    negative: bool,
    writer: *const Writer,
) usize {
    const byte = chunk.readU8(offset);
    const op_code: OpCode = @enumFromInt(byte);
    const jump_offset = chunk.readU16(offset + 1);

    reportOpCode(op_code, writer);

    if (negative) {
        writer.printf(" to {}\n", .{offset + 3 - jump_offset});
    } else {
        writer.printf(" to {}\n", .{offset + jump_offset + 3});
    }

    return 3;
}

pub fn reportOpCode(op_code: OpCode, writer: *const Writer) void {
    var fill: u8 = 24;

    for (@tagName(op_code)) |char| {
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
    offset: usize,
    source: []const u8,
    writer: *const Writer,
) usize {
    const position = chunk.positions.items[offset];
    const line, const column = position.toLineCol(source);

    writer.printf("{:0>4} {: >4}:{: <4} ", .{
        offset,
        line,
        column,
    });

    const byte = chunk.code.items[offset];
    const op_code = @as(OpCode, @enumFromInt(byte));

    return switch (op_code) {
        .constant => reportConstantInstructionName(chunk, offset, writer),

        .constant_unit,
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
        .store_local_0,
        .store_local_1,
        .store_local_2,
        .store_local_3,
        .store_local_4,
        .load_local_0,
        .load_local_1,
        .load_local_2,
        .load_local_3,
        .load_local_4,
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
        .@"return",
        .pop,
        => reportInstructionName(chunk, offset, writer),

        .if_equal,
        .if_not_equal,
        .if_greater,
        .if_greater_equal,
        .if_less,
        .if_less_equal,
        .if_true,
        .if_false,
        .jump,
        => reportJumpInstructionName(chunk, offset, false, writer),

        .negative_jump,
        => reportJumpInstructionName(chunk, offset, true, writer),

        .store_local,
        .load_local,
        .call,
        => reportByteInstruction(chunk, offset, writer),

        _ => @panic("unknown instruction"),
    };
}

pub fn reportExecutionIteration(
    values: []const Value,
    chunk: *const Chunk,
    ip_offset: usize,
    source: []const u8,
    writer: *const Writer,
) void {
    writer.print("               ");

    for (values) |value| {
        writer.print("[");
        value_reporter.printValue(value, writer);
        writer.print("] ");
    }

    writer.print("\n");

    _ = reportInstruction(chunk, ip_offset, source, writer);
}
