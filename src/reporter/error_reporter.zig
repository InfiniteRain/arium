const std = @import("std");
const shared = @import("shared");
const sema_mod = @import("../sema/sema.zig");

const Sema = sema_mod.Sema;
const Writer = shared.Writer;

pub fn reportSemaDiags(
    diags: *const Sema.Diagnostics,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportSemaDiag(diag, writer);
        writer.print("\n");
    }
}

pub fn reportSemaDiag(
    diag: *const Sema.DiagnosticEntry,
    writer: *const Writer,
) void {
    writer.printf("Error at {}:{}: ", .{
        diag.position.line,
        diag.position.column,
    });

    switch (diag.kind) {
        .expected_expr_type,
        => |eval_type| writer.printf(
            "Expected expression to be {s}.",
            .{eval_type.stringify()},
        ),

        .unexpected_arithmetic_type,
        => |eval_type| writer.printf(
            "Can't perform arithmetic operation on {s}.",
            .{eval_type.stringify()},
        ),

        .unexpected_operand_type,
        => |eval_types| writer.printf(
            "Operand is expected to be of type {s}, got {s}.",
            .{ eval_types[0].stringify(), eval_types[1].stringify() },
        ),

        .unexpected_concat_type,
        => |eval_types| writer.printf(
            "Can't perform concatenation on values of types {s} and {s}.",
            .{ eval_types[0].stringify(), eval_types[1].stringify() },
        ),

        .unexpected_equality_type,
        => |eval_types| writer.printf(
            "Can't perform equality between values of types {s} and {s}.",
            .{ eval_types[0].stringify(), eval_types[1].stringify() },
        ),

        .unexpected_comparison_type,
        => |eval_type| writer.printf(
            "Can't perform comparison operation on value of type {s}.",
            .{eval_type.stringify()},
        ),

        .unexpected_logical_type,
        => |eval_type| writer.printf(
            "Can't perform logical operation on value of type {s}.",
            .{eval_type.stringify()},
        ),

        .unexpected_logical_negation_type,
        => |eval_type| writer.printf(
            "Can't perform logical negation on value of type {s}.",
            .{eval_type.stringify()},
        ),

        .unexpected_arithmetic_negation_type,
        => |eval_type| writer.printf(
            "Can't perform arithmetic negation on value of type {s}.",
            .{eval_type.stringify()},
        ),
    }
}
