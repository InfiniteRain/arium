const std = @import("std");
const shared = @import("shared");
const parser_mod = @import("../parser/parser.zig");
const sema_mod = @import("../sema/sema.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const compiler_mod = @import("../compiler/compiler.zig");
const vm_mod = @import("../vm/vm.zig");

const Writer = shared.Writer;
const Parser = parser_mod.Parser;
const Sema = sema_mod.Sema;
const Token = tokenizer_mod.Token;
const Compiler = compiler_mod.Compiler;
const Vm = vm_mod.Vm;

pub fn reportParserDiags(
    diags: *const Parser.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportParserDiag(diag, writer);
        writer.print("\n");
    }
}

pub fn reportParserDiag(
    diag: *const Parser.DiagEntry,
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

        .expected_expression,
        => writer.print("Expected expression."),

        .expected_left_paren_before_expr,
        => writer.print("Expected '(' before expression."),

        .expected_right_paren_after_expr,
        => writer.print("Expected ')' after expression."),

        .int_literal_overflows,
        => writer.print("Integer literal value overflows."),

        .expected_name,
        => writer.print("Expected name after 'let'."),

        .expected_equal_after_name,
        => writer.print("Expected '=' after name."),

        .invalid_assignment_target,
        => writer.print("Invalid assignment target."),
    }
}

pub fn reportParserDiagTokenQuoted(
    token_kind: Token.Kind,
    writer: *const Writer,
) void {
    switch (token_kind) {
        .eof => writer.print("end of file"),
        .end => writer.print("'end'"),

        .left_paren,
        .right_paren,
        .minus,
        .plus,
        .slash,
        .star,
        .plus_plus,
        .bang_equal,
        .equal,
        .equal_equal,
        .greater,
        .greater_equal,
        .less,
        .less_equal,
        .identifier,
        .int,
        .float,
        .string,
        .true_,
        .false_,
        .not,
        .and_,
        .or_,
        .do,
        .mut,
        .let,
        .assert,
        .print,
        .new_line,
        .semicolon,
        .comment,
        .invalid,
        => @panic("token kind not implemented"),
    }
}

pub fn reportSemaDiags(
    diags: *const Sema.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportSemaDiag(diag, writer);
        writer.print("\n");
    }
}

pub fn reportSemaDiag(
    diag: *const Sema.DiagEntry,
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

        .too_many_locals,
        => writer.print("Too many locals declared in chunk."),

        .value_not_found,
        => |name| writer.printf("Can't find value '{s}' in this scope.", .{name}),

        .unexpected_assignment_type,
        => |eval_types| writer.printf(
            "Can't assign value of type {s} to variable of type {s}.",
            .{ eval_types[1].stringify(), eval_types[0].stringify() },
        ),

        .immutable_mutation,
        => |name| writer.printf("Can't assign twice to immutable variable '{s}'.", .{name}),
    }
}

pub fn reportCompilerDiags(
    diags: *const Compiler.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportCompilerDiag(diag, writer);
        writer.print("\n");
    }
}

pub fn reportCompilerDiag(
    diag: *const Compiler.DiagEntry,
    writer: *const Writer,
) void {
    writer.printf("Error at {}:{}: ", .{
        diag.position.line,
        diag.position.column,
    });

    switch (diag.kind) {
        .too_many_branch_jumps,
        => writer.print("Too many branching jumps is required to be generated for this expression."),

        .jump_too_big,
        => writer.print("Required jump is too big."),

        .too_many_constants,
        => writer.print("Too many constants declared in chunk."),
    }
}

pub fn reportVmDiags(
    diags: *const Vm.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportVmDiag(diag, writer);
        writer.print("\n");
    }
}

pub fn reportVmDiag(
    diag: *const Vm.DiagEntry,
    writer: *const Writer,
) void {
    writer.printf("Panic at {}:{}: ", .{
        diag.position.line,
        diag.position.column,
    });

    switch (diag.kind) {
        .assertion_fail,
        => writer.print("Assertion failed."),
    }
}
