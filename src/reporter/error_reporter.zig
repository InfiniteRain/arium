const std = @import("std");
const shared = @import("shared");
const parser_mod = @import("../parser/parser.zig");
const sema_mod = @import("../sema/sema.zig");
const tokenizer_mod = @import("../tokenizer.zig");
const compiler_mod = @import("../compiler/compiler.zig");
const vm_mod = @import("../vm/vm.zig");
const type_reporter = @import("type_reporter.zig");

const Writer = shared.Writer;
const Parser = parser_mod.Parser;
const Sema = sema_mod.Sema;
const Token = tokenizer_mod.Token;
const Loc = tokenizer_mod.Loc;
const Compiler = compiler_mod.Compiler;
const Vm = vm_mod.Vm;

pub fn reportParserDiags(
    diags: *const Parser.Diags,
    source: []const u8,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportParserDiag(diag, source, writer);
        writer.print("\n");
    }
}

pub fn reportParserDiag(
    diag: *const Parser.DiagEntry,
    source: []const u8,
    writer: *const Writer,
) void {
    const line, const column = diag.position.toLineCol(source);

    writer.printf("Error at {}:{}: ", .{
        line,
        column,
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

        .expected_name_after_let,
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
            writer.print(" after the condition.");
        },

        .expected_name_after_fn,
        => writer.print("Expected name after 'let'."),

        .expected_left_paren_before_args,
        => writer.print("Expected '(' before argument list."),

        .expected_right_paren_after_args,
        => writer.print("Expected ')' after argument list."),

        .expected_colon_after_arg,
        => writer.print("Expected ':' after argument."),

        .expected_colon_after_args,
        => writer.print("Expected ':' after argument list."),
    }
}

pub fn reportParserDiagTokenQuoted(
    token_kind: Token.Tag,
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
    source: []const u8,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportSemaDiag(diag, source, writer);
        writer.print("\n");
    }
}

pub fn reportSemaDiag(
    diag: *const Sema.DiagEntry,
    source: []const u8,
    writer: *const Writer,
) void {
    const line, const column = diag.position.toLineCol(source);

    writer.printf("Error at {}:{}: ", .{
        line,
        column,
    });

    switch (diag.kind) {
        .expected_expr_type,
        => |sema_type| {
            writer.print("Expected expression to be ");
            type_reporter.printType(sema_type, writer);
            writer.print(".");
        },

        .unexpected_arithmetic_type,
        => |sema_type| {
            writer.print("Can't perform arithmetic operation on ");
            type_reporter.printType(sema_type, writer);
            writer.print(".");
        },

        .unexpected_operand_type,
        => |sema_types| {
            writer.print("Operand is expected to be of type ");
            type_reporter.printType(sema_types[0], writer);
            writer.print(", got ");
            type_reporter.printType(sema_types[1], writer);
            writer.print(".");
        },

        .unexpected_concat_type,
        => |sema_types| {
            writer.print("Can't perform concatenation on values of types ");
            type_reporter.printType(sema_types[0], writer);
            writer.print(" and ");
            type_reporter.printType(sema_types[1], writer);
            writer.print(".");
        },

        .unexpected_equality_type,
        => |sema_types| {
            writer.print("Can't perform equality between values of types ");
            type_reporter.printType(sema_types[0], writer);
            writer.print(" and ");
            type_reporter.printType(sema_types[1], writer);
            writer.print(".");
        },

        .unexpected_comparison_type,
        => |sema_type| {
            writer.print("Can't perform comparison operation on value of type ");
            type_reporter.printType(sema_type, writer);
            writer.print(".");
        },

        .unexpected_logical_type,
        => |sema_type| {
            writer.print("Can't perform logical operation on value of type ");
            type_reporter.printType(sema_type, writer);
            writer.print(".");
        },

        .unexpected_logical_negation_type,
        => |sema_type| {
            writer.print("Can't perform logical negation on value of type ");
            type_reporter.printType(sema_type, writer);
            writer.print(".");
        },

        .unexpected_arithmetic_negation_type,
        => |sema_type| {
            writer.print("Can't perform arithmetic negation on value of type ");
            type_reporter.printType(sema_type, writer);
            writer.print(".");
        },

        .too_many_locals,
        => writer.print("Too many locals declared in chunk."),

        .value_not_found,
        => |name| writer.printf("Can't find value '{s}' in this scope.", .{name}),

        .unexpected_assignment_type,
        => |sema_types| {
            writer.print("Can't assign value of type ");
            type_reporter.printType(sema_types[1], writer);
            writer.print(" to variable of type ");
            type_reporter.printType(sema_types[0], writer);
            writer.print(".");
        },

        .immutable_mutation,
        => |name| writer.printf("Can't assign twice to immutable variable '{s}'.", .{name}),

        .type_not_found,
        => |name| writer.printf("Can't find type '{s}' in this scope.", .{name}),

        .value_not_assigned,
        => |name| writer.printf("Can't use variable '{s}' before assigning it a value.", .{name}),

        .unexpected_else_type,
        => |sema_types| {
            writer.print("Else branch is expected to be of type ");
            type_reporter.printType(sema_types[0], writer);
            writer.print(", got ");
            type_reporter.printType(sema_types[1], writer);
            writer.print(".");
        },

        .break_outside_loop,
        => writer.print("Cannot use break outside of a loop."),

        .continue_outside_loop,
        => writer.print("Cannot use continue outside of a loop."),

        .unexpected_elseif_type,
        => |sema_types| {
            writer.print("Else-if branch is expected to be of type ");
            type_reporter.printType(sema_types[0], writer);
            writer.print(", got ");
            type_reporter.printType(sema_types[1], writer);
            writer.print(".");
        },

        .unexpected_return_type,
        => |sema_types| {
            writer.print("Return value is expected to be of type ");
            type_reporter.printType(sema_types[0], writer);
            writer.print(", got ");
            type_reporter.printType(sema_types[1], writer);
            writer.print(".");
        },

        .not_callable,
        => writer.print("Expression is not callable."),

        .arity_mismatch,
        => |info| writer.printf(
            "Callable expects {} arguments, got {}.",
            .{ info[0], info[1] },
        ),

        .unexpected_arg_type,
        => |info| {
            writer.printf("Argument {} is expected to be of type ", .{info[0]});
            type_reporter.printType(info[1], writer);
            writer.print(", got ");
            type_reporter.printType(info[2], writer);
            writer.print(".");
        },

        .not_all_branches_return,
        => writer.print("Not all branches return from this function."),
    }
}

pub fn reportCompilerDiags(
    diags: *const Compiler.Diags,
    source: []const u8,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportCompilerDiag(diag, source, writer);
        writer.print("\n");
    }
}

pub fn reportCompilerDiag(
    diag: *const Compiler.DiagEntry,
    source: []const u8,
    writer: *const Writer,
) void {
    const line, const column = diag.position.toLineCol(source);

    writer.printf("Error at {}:{}: ", .{
        line,
        column,
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
    source: []const u8,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        reportVmDiag(diag, source, writer);
        writer.print("\n");
    }
}

pub fn reportVmDiag(
    diag: *const Vm.DiagEntry,
    source: []const u8,
    writer: *const Writer,
) void {
    const line, const column = diag.position.toLineCol(source);

    writer.printf("Panic at {}:{}: ", .{
        line,
        column,
    });

    switch (diag.kind) {
        .assertion_fail,
        => writer.print("Assertion failed."),

        .stack_overflow,
        => writer.print("Stack overflow."),
    }
}
