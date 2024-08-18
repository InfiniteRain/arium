const std = @import("std");
const shared = @import("shared");
const parser_mod = @import("../parser/parser.zig");
const sema_mod = @import("../sema/sema.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const Writer = shared.Writer;
const Parser = parser_mod.Parser;
const Sema = sema_mod.Sema;
const Token = tokenizer_mod.Token;

pub fn reportParserDiags(
    diags: *const Parser.Diagnostics,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportParserDiag(diag, writer);
        writer.print("\n");
    }
}

pub fn reportParserDiag(
    diag: *const Parser.DiagnosticEntry,
    writer: *const Writer,
) void {
    writer.printf("Error at {}:{}: ", .{
        diag.position.line,
        diag.position.column,
    });

    switch (diag.kind) {
        .expected_end_token,
        => |token_kind| {
            writer.print("Expected ");
            reportParserDiagTokenQuoted(token_kind, writer);
            writer.print(", line break or ';'.");
        },

        .invalid_token,
        => |message| writer.print(message),

        .expected_statement,
        => writer.print("Expected statement."),

        .expected_expression,
        => writer.print("Expected expression."),

        .expected_left_paren_before_expr,
        => writer.print("Expected '(' before expression."),

        .expected_right_paren_after_expr,
        => writer.print("Expected ')' after expression."),

        .int_literal_overflows,
        => writer.print("Integer literal value overflows."),
    }
}

pub fn reportParserDiagTokenQuoted(
    token_kind: Token.Kind,
    writer: *const Writer,
) void {
    switch (token_kind) {
        .eof => writer.print("end of file"),
        else => @panic("token kind not implemented"),
    }
}

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
