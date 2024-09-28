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
        => |token_list| {
            writer.print("Expected ");

            for (token_list.items) |token_kind| {
                reportParserDiagTokenQuoted(token_kind, writer);
                writer.print(", ");
            }
            writer.print("line break or ';'.");
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

        .expected_type,
        => writer.print("Expected type."),

        .variable_name_not_lower_case,
        => |name| writer.printf(
            "Expected variable '{s}' to start with a lower-case letter.",
            .{name},
        ),

        .expected_token_after_condition,
        => |token_kind| {
            writer.print("Expected ");
            reportParserDiagTokenQuoted(token_kind, writer);
            writer.print(" after the if condition.");
        },
    }
}

pub fn reportParserDiagTokenQuoted(
    token_kind: Token.Kind,
    writer: *const Writer,
) void {
    switch (token_kind) {
        .eof => writer.print("end of file"),
        .end => writer.print("'end'"),
        .@"else" => writer.print("'else'"),
        .then => writer.print("'then'"),
        .do => writer.print("'do'"),

        else => @panic("token kind not implemented"),
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
        => |sema_type| writer.printf(
            "Expected expression to be {s}.",
            .{sema_type.stringify()},
        ),

        .unexpected_arithmetic_type,
        => |sema_type| writer.printf(
            "Can't perform arithmetic operation on {s}.",
            .{sema_type.stringify()},
        ),

        .unexpected_operand_type,
        => |sema_types| writer.printf(
            "Operand is expected to be of type {s}, got {s}.",
            .{ sema_types[0].stringify(), sema_types[1].stringify() },
        ),

        .unexpected_concat_type,
        => |sema_types| writer.printf(
            "Can't perform concatenation on values of types {s} and {s}.",
            .{ sema_types[0].stringify(), sema_types[1].stringify() },
        ),

        .unexpected_equality_type,
        => |sema_types| writer.printf(
            "Can't perform equality between values of types {s} and {s}.",
            .{ sema_types[0].stringify(), sema_types[1].stringify() },
        ),

        .unexpected_comparison_type,
        => |sema_type| writer.printf(
            "Can't perform comparison operation on value of type {s}.",
            .{sema_type.stringify()},
        ),

        .unexpected_logical_type,
        => |sema_type| writer.printf(
            "Can't perform logical operation on value of type {s}.",
            .{sema_type.stringify()},
        ),

        .unexpected_logical_negation_type,
        => |sema_type| writer.printf(
            "Can't perform logical negation on value of type {s}.",
            .{sema_type.stringify()},
        ),

        .unexpected_arithmetic_negation_type,
        => |sema_type| writer.printf(
            "Can't perform arithmetic negation on value of type {s}.",
            .{sema_type.stringify()},
        ),

        .too_many_locals,
        => writer.print("Too many locals declared in chunk."),

        .value_not_found,
        => |name| writer.printf("Can't find value '{s}' in this scope.", .{name}),

        .unexpected_assignment_type,
        => |sema_types| writer.printf(
            "Can't assign value of type {s} to variable of type {s}.",
            .{ sema_types[1].stringify(), sema_types[0].stringify() },
        ),

        .immutable_mutation,
        => |name| writer.printf("Can't assign twice to immutable variable '{s}'.", .{name}),

        .type_not_found,
        => |name| writer.printf("Can't find type '{s}' in this scope.", .{name}),

        .value_not_assigned,
        => |name| writer.printf("Can't use variable '{s}' before assigning it a value.", .{name}),

        .unexpected_else_type,
        => |sema_types| writer.printf(
            "Else branch is expected to be of type {s}, got {s}.",
            .{ sema_types[0].stringify(), sema_types[1].stringify() },
        ),

        .break_outside_loop,
        => writer.print("Cannot use break outside of a loop."),

        .continue_outside_loop,
        => writer.print("Cannot use continue outside of a loop."),
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
