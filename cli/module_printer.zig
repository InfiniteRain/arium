const std = @import("std");
const mem = std.mem;

const arium = @import("arium");
const Object = arium.Object;
const Module = arium.Module;
const OpCode = arium.OpCode;
const Output = arium.Output;
const Value = arium.Value;

pub const ModulePrinter = struct {
    module: *const Module(.debug),
    output: *const Output,

    pub fn print(
        module: *const Module(.debug),
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
        module: *const Module(.debug),
        output: *const Output,
        offset_start: usize,
        offset: usize,
    ) usize {
        output.printf("{:0>4} ", .{offset - offset_start});

        const op_code: OpCode = @enumFromInt(module.code.items[offset]);

        const module_printer: ModulePrinter = .{
            .module = module,
            .output = output,
        };

        return switch (op_code) {
            .constant_u8,
            => module_printer.printConstantInstructionName(u8, offset),

            .constant_u16,
            => module_printer.printConstantInstructionName(u16, offset),

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
            => module_printer.printInstructionName(offset),

            .if_equal,
            .if_not_equal,
            .if_greater,
            .if_greater_equal,
            .if_less,
            .if_less_equal,
            .if_true,
            .if_false,
            .jump,
            => module_printer.printJumpInstructionName(
                offset_start,
                offset,
                .positive,
            ),

            .negative_jump,
            => module_printer.printJumpInstructionName(
                offset_start,
                offset,
                .negative,
            ),

            .store_local_u8,
            .load_local_u8,
            .call,
            .close_u8,
            => module_printer.printInstructionWithArgName(u8, offset),

            _ => @panic("unknown instruction"),
        };
    }

    fn printConstantInstructionName(
        self: *const ModulePrinter,
        T: type,
        offset: usize,
    ) usize {
        const op_code: OpCode = @enumFromInt(self.module.code.items[offset]);
        const index_bytes =
            self.module.code.items[offset + 1 ..][0..@sizeOf(T)];
        const index = mem.bytesToValue(T, index_bytes);

        self.printOpCode(op_code);
        self.output.printf(" {: <4} '", .{index});
        self.printValue(self.module.constants.items[index]);
        self.output.print("'\n");

        return 1 + @sizeOf(T);
    }

    fn printInstructionName(
        self: *const ModulePrinter,
        offset: usize,
    ) usize {
        const op_code: OpCode = @enumFromInt(self.module.code.items[offset]);

        self.printOpCode(op_code);
        self.output.print("\n");

        return 1;
    }

    fn printJumpInstructionName(
        self: *const ModulePrinter,
        offset_start: usize,
        offset: usize,
        direction: enum { positive, negative },
    ) usize {
        const op_code: OpCode = @enumFromInt(self.module.code.items[offset]);
        const jump_offset = mem.bytesToValue(
            u16,
            self.module.code.items[offset + 1 ..][0..2],
        );

        self.printOpCode(op_code);

        if (direction == .positive) {
            self.output.printf(
                " to {}\n",
                .{(offset - offset_start) + jump_offset + 3},
            );
        } else {
            self.output.printf(
                " to {}\n",
                .{(offset - offset_start) + 3 - jump_offset},
            );
        }

        return 3;
    }

    fn printInstructionWithArgName(
        self: *const ModulePrinter,
        T: type,
        offset: usize,
    ) usize {
        const op_code: OpCode = @enumFromInt(self.module.code.items[offset]);
        const arg_bytes = self.module.code.items[offset + 1 ..][0..@sizeOf(T)];
        const arg = mem.bytesToValue(T, arg_bytes);

        self.printOpCode(op_code);
        self.output.printf(" {: <4}\n", .{arg});

        return 1 + @sizeOf(T);
    }

    fn printOpCode(self: *const ModulePrinter, op_code: OpCode) void {
        var fill: u8 = 24;

        for (@tagName(op_code)) |char| {
            self.output.printf("{c}", .{std.ascii.toUpper(char)});

            if (fill > 0) {
                fill -= 1;
            }
        }

        for (0..fill) |_| {
            self.output.print(" ");
        }
    }

    fn printValue(self: *const ModulePrinter, value: Value(.debug)) void {
        switch (value) {
            .int => |int| self.output.printf("{}", .{int}),
            .float => |float| self.output.printf("{d}", .{float}),
            .bool => |@"bool"| self.output.printf("{}", .{@"bool"}),
            .@"fn" => |@"fn"| self.output.printf("<fn {}>", .{@"fn"}),
            .object => |object| switch (object.tag) {
                .string => self.output.printf(
                    "{s}",
                    .{object.as(Object(.debug).String).chars},
                ),
            },
        }
    }
};
