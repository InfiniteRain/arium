const std = @import("std");

const arium = @import("arium");
const Span = arium.Span;
const Output = arium.Output;
const Token = arium.Token;
const Parser = arium.Parser;
const InternPool = arium.InternPool;
const Sema = arium.Sema;
const Compiler = arium.Compiler;
const Vm = arium.Vm;

pub const DiagsPrinter = struct {
    source: []const u8,
    output: *const Output,
    intern_pool: *const InternPool,

    pub fn printParserDiags(
        source: []const u8,
        diags: *const Parser.Diags,
        output: *const Output,
    ) void {
        const diags_printer: DiagsPrinter = .{
            .source = source,
            .output = output,
            .intern_pool = undefined,
        };

        for (diags.entries.items) |entry| {
            diags_printer.printParserDiagEntry(entry);
            output.print("\n");
        }
    }

    fn printParserDiagEntry(
        self: *const DiagsPrinter,
        diag: Parser.Diags.Entry,
    ) void {
        self.printErrorHeader(diag.loc);

        switch (diag.tag) {
            .expected_end_token,
            => |token_list| {
                self.output.print("Expected ");

                for (token_list.slice()) |token_tag| {
                    self.printTokenQuoted(token_tag);
                    self.output.print(", ");
                }

                self.output.print("line break or ';'.");
            },

            .expected_expression,
            => self.output.print("Expected expression."),

            .expected_token_after_condition,
            => |token_tag| {
                self.output.print("Expected ");
                self.printTokenQuoted(token_tag);
                self.output.print(" after the condition.");
            },

            .expected_identifier,
            => self.output.print("Expected identifier."),

            .expected_right_paren_after_args,
            => self.output.print("Expected ')' after argument list."),

            .expected_left_paren_before_params,
            => self.output.print("Expected '(' before parameter list."),

            .expected_right_paren_after_params,
            => self.output.print("Expected ')' after parameter list."),

            .expected_colon_after_params,
            => self.output.print("Expected ':' after parameter list."),

            .expected_colon_after_param,
            => self.output.print("Expected ':' after parameter."),

            .expected_right_paren_after_expr,
            => self.output.print("Expected ')' after expression."),

            .invalid_assignment_target,
            => self.output.print("Invalid assignment target."),

            .invalid_token,
            => self.output.print("Invalid token."),
        }
    }

    pub fn printSemaDiags(
        source: []const u8,
        diags: *const Sema.Diags,
        intern_pool: *const InternPool,
        output: *const Output,
    ) void {
        const diags_printer: DiagsPrinter = .{
            .source = source,
            .output = output,
            .intern_pool = intern_pool,
        };

        for (diags.entries.items) |entry| {
            diags_printer.printSemaDiagEntry(entry);
            output.print("\n");
        }
    }

    fn printSemaDiagEntry(
        self: *const DiagsPrinter,
        diag: Sema.Diags.Entry,
    ) void {
        self.printErrorHeader(diag.loc);

        switch (diag.tag) {
            .unexpected_expr_type,
            => |mismatch| {
                self.output.print("Expected expression of type ");

                const expected = mismatch.expected.slice();

                for (expected, 0..) |@"type", index| {
                    if (index > 0) {
                        if (index == expected.len - 1) {
                            self.output.print(" or ");
                        } else {
                            self.output.print(", ");
                        }
                    }

                    self.printType(@"type");
                }

                self.output.print(", got ");
                self.printType(mismatch.actual);
                self.output.print(".");
            },

            .integer_overflow,
            => self.output.print("Integer value overflows."),

            .undeclared_identifier,
            => self.output.print("Undeclared identifier."),

            .too_many_locals,
            => self.output.print("Too many locals declared."),

            .unassigned_variable,
            => self.output.printf(
                "Variable '{s}' was not assigned a value.",
                .{diag.loc.toSlice(self.source)},
            ),

            .immutable_mutation,
            => |loc| self.output.printf(
                "Variable '{s}' is immutable.",
                .{loc.toSlice(self.source)},
            ),

            .unreachable_stmt,
            => self.output.print("Unreachable statement."),

            .break_outside_loop,
            => self.output.print("Usage of `break' outside of a loop."),

            .continue_outside_loop,
            => self.output.print("Usage of `continue` outside of a loop."),

            .not_all_branches_return,
            => self.output.print("Not all branches return a value."),

            .non_callable_call,
            => self.output.print("Value is not a callable."),

            .arity_mismatch,
            => |mismatch| self.output.printf(
                "Function expects {} arguments, got {}.",
                .{ mismatch.expected, mismatch.actual },
            ),

            .unexpected_arg_type,
            => |mismatch| {
                self.output.printf(
                    "Function expects argument {} to be of type ",
                    .{mismatch.index + 1},
                );
                self.printType(mismatch.expected);
                self.output.print(", got ");
                self.printType(mismatch.actual);
                self.output.print(".");
            },
        }
    }

    pub fn printCompilerDiags(
        source: []const u8,
        diags: *const Compiler.Diags,
        output: *const Output,
    ) void {
        const diags_printer: DiagsPrinter = .{
            .source = source,
            .output = output,
            .intern_pool = undefined,
        };

        for (diags.entries.items) |entry| {
            diags_printer.printCompilerDiagEntry(entry);
            output.print("\n");
        }
    }

    fn printCompilerDiagEntry(
        self: *const DiagsPrinter,
        diag: Compiler.Diags.Entry,
    ) void {
        self.printErrorHeader(diag.loc);

        switch (diag.tag) {
            .too_many_constants,
            => self.output.print("Constants limit exceeded."),

            .jump_too_big,
            => self.output.print("Jump offset limit exceeded."),

            .fn_body_too_big,
            => self.output.print("Function body size limit exceeded."),
        }
    }

    pub fn printVmDiags(
        source: []const u8,
        diags: *const Vm.Diags,
        output: *const Output,
    ) void {
        const diags_printer: DiagsPrinter = .{
            .source = source,
            .output = output,
            .intern_pool = undefined,
        };

        if (diags.entry) |entry| {
            diags_printer.printVmDiagEntry(entry);
            output.print("\n");
        }
    }

    fn printVmDiagEntry(
        self: *const DiagsPrinter,
        diag: Vm.Diags.Entry,
    ) void {
        self.printErrorHeader(diag.loc);

        switch (diag.tag) {
            .stack_overflow,
            => self.output.print("Stack overflow."),
        }
    }

    fn printTokenQuoted(
        self: *const DiagsPrinter,
        token_tag: Token.Tag,
    ) void {
        switch (token_tag) {
            .eof => self.output.print("end of file"),
            .end => self.output.print("'end'"),
            .@"else" => self.output.print("'else'"),
            .then => self.output.print("'then'"),
            .do => self.output.print("'do'"),

            else => @panic("token kind not implemented"),
        }
    }

    fn printType(
        self: *const DiagsPrinter,
        @"type": InternPool.Index,
    ) void {
        switch (@"type") {
            .type_int => self.output.print("Int"),
            .type_float => self.output.print("Float"),
            .type_bool => self.output.print("Bool"),
            .type_string => self.output.print("String"),
            .type_unit => self.output.print("Unit"),
            .type_never => self.output.print("Never"),
            .type_type => self.output.print("Type"),

            .none,
            .value_unit,
            .value_bool_true,
            .value_bool_false,
            .invalid,
            => unreachable,

            _ => {
                switch (@"type".toKey(self.intern_pool)) {
                    .type_fn => |type_fn| {
                        self.output.print("Fn(");

                        for (type_fn.arg_types, 0..) |arg, index| {
                            self.printType(arg);

                            if (index != type_fn.arg_types.len - 1) {
                                self.output.print(", ");
                            }
                        }

                        self.output.print("): ");
                        self.printType(type_fn.return_type);
                    },

                    .none,
                    .type_simple,
                    .value_simple,
                    .value_int,
                    .value_float,
                    .value_fn,
                    .value_string,
                    .invalid,
                    => unreachable,
                }
            },
        }
    }

    fn printErrorHeader(self: *const DiagsPrinter, loc: Span(u8)) void {
        const line, const column = loc.toLineCol(self.source);

        self.output.printf("Error at {}:{}: ", .{
            line,
            column,
        });
    }
};
