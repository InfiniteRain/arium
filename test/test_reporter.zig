const shared = @import("shared");
const arium = @import("arium");
const runner_mod = @import("runner.zig");
const config_mod = @import("config.zig");

const Writer = shared.Writer;
const Runner = runner_mod.Runner;
const Parser = arium.Parser;
const Sema = arium.Sema;
const SemaExpr = arium.SemaExpr;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const error_reporter = arium.error_reporter;
const Config = config_mod.Config;

pub fn reportConfigDiags(
    path: []const u8,
    diags: *const Config.Diags,
    writer: *const Writer,
) void {
    writer.printf("Test configuration diags for '{s}':\n", .{path});

    for (diags.getEntries()) |*diag| {
        reportConfigDiag(diag, writer);
        writer.print("\n");
    }

    writer.print("\n");
}

pub fn reportConfigDiag(
    diag: *const Config.DiagEntry,
    writer: *const Writer,
) void {
    writer.printf("Line {}: {s}", .{
        diag.position.line,
        diag.message,
    });
}

pub fn reportRunnerDiags(
    diags: *const Runner.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        writer.printf("\nDiags for '{s}':\n", .{
            diag.path,
        });
        reportRunnerDiag(diag, writer);
    }
    writer.print("\n");
}

pub fn reportRunnerDiag(
    diag: *const Runner.DiagEntry,
    writer: *const Writer,
) void {
    for (diag.failures.items) |info| {
        switch (info) {
            .parser => |*diags| {
                error_reporter.reportParserDiags(diags, writer);
            },
            .sema => |*diags| {
                error_reporter.reportSemaDiags(diags, writer);
            },
            .compiler => |*diags| {
                error_reporter.reportCompilerDiags(diags, writer);
            },
            .vm => |*diags| {
                error_reporter.reportVmDiags(diags, writer);
            },
            .out_mismatch => |*mismatch| {
                reportOutMismatch(mismatch, writer);
            },
            .err_parser_mismatch => |*mismatch| {
                reportErrParserMismatch(mismatch, writer);
            },
            .err_sema_mismatch => |*mismatch| {
                reportErrSemaMismatch(mismatch, writer);
            },
            .err_compiler_mismatch => |*mismatch| {
                reportErrCompilerMismatch(mismatch, writer);
            },
            .err_vm_mismatch => |*mismatch| {
                reportErrVmMismatch(mismatch, writer);
            },
            .memory_leak => {
                writer.print("Memory leak.");
            },
        }
    }
}

pub fn reportErrParserMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Parser.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected parser error(s).\nExpected:\n");
    reportParserDiags(&mismatch.expected, writer);
    writer.print("\nActual:\n");
    reportParserDiags(&mismatch.actual, writer);
}

pub fn reportErrSemaMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Sema.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected sema error(s).\nExpected:\n");
    reportSemaDiags(&mismatch.expected, writer);
    writer.print("\nActual:\n");
    reportSemaDiags(&mismatch.actual, writer);
}

pub fn reportErrCompilerMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Compiler.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected compiler error.\nExpected:\n");
    reportCompilerDiags(&mismatch.expected, writer);
    writer.print("\nActual:\n");
    reportCompilerDiags(&mismatch.actual, writer);
}

pub fn reportErrVmMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Vm.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected vm error.\nExpected:\n");
    reportVmDiags(&mismatch.expected, writer);
    writer.print("\nActual:\n");
    reportVmDiags(&mismatch.actual, writer);
}

pub fn reportOutMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch([]const u8),
    writer: *const Writer,
) void {
    writer.printf("Unexpected stdout.\nExpected:\n{s}\nActual:\n{s}", .{
        mismatch.expected,
        mismatch.actual,
    });
}

pub fn reportParserDiags(
    diags: *const Parser.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |entry| {
        writer.printf(
            "'{s}' on line {}",
            .{ @tagName(entry.kind), entry.position.line },
        );

        switch (entry.kind) {
            .expected_end_token => |token| writer.printf(
                " with token {s}",
                .{@tagName(token)},
            ),

            .invalid_token => |msg| writer.printf(
                " '{s}'",
                .{msg},
            ),

            .expected_expression,
            .expected_left_paren_before_expr,
            .expected_right_paren_after_expr,
            .int_literal_overflows,
            .expected_name,
            .expected_equal_after_name,
            => {},
        }

        writer.print(".\n");
    }
}

pub fn reportSemaDiags(
    diags: *const Sema.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |entry| {
        writer.printf(
            "'{s}' on line {}",
            .{ @tagName(entry.kind), entry.position.line },
        );

        switch (entry.kind) {
            .expected_expr_type,
            .unexpected_arithmetic_type,
            .unexpected_comparison_type,
            .unexpected_logical_type,
            .unexpected_logical_negation_type,
            .unexpected_arithmetic_negation_type,
            => |eval_type| {
                writer.print(" with eval type ");
                reportEvalType(eval_type, writer);
            },

            .unexpected_operand_type,
            .unexpected_concat_type,
            .unexpected_equality_type,
            => |eval_type| {
                const left, const right = eval_type;

                writer.print(" with eval types ");
                reportEvalType(left, writer);
                writer.print(" and ");
                reportEvalType(right, writer);
            },

            .value_not_found,
            => |name| {
                writer.printf(" with name {s}", .{name});
            },

            .too_many_locals,
            => {},
        }

        writer.print(".\n");
    }
}

pub fn reportCompilerDiags(
    diags: *const Compiler.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |entry| {
        writer.printf(
            "'{s}' on line {}\n",
            .{ @tagName(entry.kind), entry.position.line },
        );
    }
}

pub fn reportVmDiags(
    diags: *const Vm.Diags,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |entry| {
        writer.printf(
            "'{s}' on line {}\n",
            .{ @tagName(entry.kind), entry.position.line },
        );
    }
}

pub fn reportEvalType(
    eval_type: SemaExpr.EvalType,
    writer: *const Writer,
) void {
    writer.printf("{s}", .{@tagName(eval_type)});

    switch (eval_type) {
        .obj => |kind| {
            writer.printf(" {s}", .{@tagName(kind)});
        },

        .unit,
        .int,
        .float,
        .bool,
        .invalid,
        => {},
    }
}
