const std = @import("std");
const mem = std.mem;

const shared = @import("shared");
const Output = shared.Output;

const module_mod = @import("../module.zig");
const OpCode = module_mod.OpCode;
const Module = module_mod.Module;
const compiler_mod = @import("../compiler.zig");
const Compiler = compiler_mod.Compiler;
const memory_mod = @import("../memory.zig");
const Object = memory_mod.Object;

pub fn printModule(
    module: *const Module,
    output: *const Output,
) void {
    var index: usize = 0;

    while (index < module.code.items.len) {
        const locals_count = mem.bytesToValue(
            u32,
            module.code.items[index..][0..4],
        );
        const size = mem.bytesToValue(
            u32,
            module.code.items[index + 4 ..][0..4],
        );
        defer index += size + 8;

        output.printf(
            "=== fn: {}, locals: {}, size: {} ===\n",
            .{ index, locals_count, size },
        );

        const offset_start = index + 8;
        var ip = offset_start;

        while (ip < offset_start + size) {
            ip += printInstruction(
                module,
                output,
                offset_start,
                ip,
            );
        }

        output.print("\n");
    }
}

pub fn printInstruction(
    module: *const Module,
    output: *const Output,
    offset_start: usize,
    offset: usize,
) usize {
    output.printf("{:0>4} ", .{offset - offset_start});

    const op_code: OpCode = @enumFromInt(module.code.items[offset]);

    return switch (op_code) {
        .constant_u8 => printConstantInstructionName(
            u8,
            module,
            output,
            offset,
        ),
        .constant_u16 => printConstantInstructionName(
            u16,
            module,
            output,
            offset,
        ),

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
        .compare_fn,
        .compare_object,
        .assert,
        .print_unit,
        .print_int,
        .print_float,
        .print_bool,
        .print_fn,
        .print_object,
        .@"return",
        .pop,
        => printInstructionName(module, output, offset),

        .if_equal,
        .if_not_equal,
        .if_greater,
        .if_greater_equal,
        .if_less,
        .if_less_equal,
        .if_true,
        .if_false,
        .jump,
        => printJumpInstructionName(
            module,
            output,
            offset_start,
            offset,
            .positive,
        ),

        .negative_jump,
        => printJumpInstructionName(
            module,
            output,
            offset_start,
            offset,
            .negative,
        ),

        .store_local_u8,
        .load_local_u8,
        .call,
        => printInstructionWithArgName(u8, module, output, offset),

        _ => @panic("unknown instruction"),
    };
}

fn printConstantInstructionName(
    T: type,
    module: *const Module,
    output: *const Output,
    offset: usize,
) usize {
    const op_code: OpCode = @enumFromInt(module.code.items[offset]);
    const index_bytes = module.code.items[offset + 1 ..][0..@sizeOf(T)];
    const index = mem.bytesToValue(T, index_bytes);

    printOpCode(op_code, output);
    output.printf(" {: <4} '", .{index});

    const value = module.constants.items[index];

    if (module.constant_tags.items.len > 0) {
        switch (module.constant_tags.items[index]) {
            .int => output.printf("{}", .{value.int}),
            .float => output.printf("{d}", .{value.float}),
            .bool => output.printf("{}", .{value.bool}),
            .@"fn" => output.printf("<fn {}>", .{value.@"fn"}),
            .object => switch (value.object.tag) {
                .string => output.printf(
                    "{s}",
                    .{value.object.as(Object.String).chars},
                ),
            },
        }
    } else {
        output.printf("{X}", .{value.int});
    }

    output.print("'\n");

    return 1 + @sizeOf(T);
}

fn printInstructionName(
    module: *const Module,
    output: *const Output,
    offset: usize,
) usize {
    const op_code: OpCode = @enumFromInt(module.code.items[offset]);

    printOpCode(op_code, output);
    output.print("\n");

    return 1;
}

fn printJumpInstructionName(
    module: *const Module,
    output: *const Output,
    offset_start: usize,
    offset: usize,
    direction: enum { positive, negative },
) usize {
    const op_code: OpCode = @enumFromInt(module.code.items[offset]);
    const jump_offset = mem.bytesToValue(
        u16,
        module.code.items[offset + 1 ..][0..2],
    );

    printOpCode(op_code, output);

    if (direction == .positive) {
        output.printf(" to {}\n", .{(offset - offset_start) + jump_offset + 3});
    } else {
        output.printf(" to {}\n", .{(offset - offset_start) + 3 - jump_offset});
    }

    return 3;
}

fn printInstructionWithArgName(
    T: type,
    module: *const Module,
    output: *const Output,
    offset: usize,
) usize {
    const op_code: OpCode = @enumFromInt(module.code.items[offset]);
    const arg_bytes = module.code.items[offset + 1 ..][0..@sizeOf(T)];
    const arg = mem.bytesToValue(T, arg_bytes);

    printOpCode(op_code, output);
    output.printf(" {: <4}\n", .{arg});

    return 1 + @sizeOf(T);
}

fn printOpCode(op_code: OpCode, output: *const Output) void {
    var fill: u8 = 24;

    for (@tagName(op_code)) |char| {
        output.printf("{c}", .{std.ascii.toUpper(char)});

        if (fill > 0) {
            fill -= 1;
        }
    }

    for (0..fill) |_| {
        output.print(" ");
    }
}
