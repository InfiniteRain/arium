const shared = @import("shared");
const arium = @import("arium");
const runner_mod = @import("runner.zig");
const config_mod = @import("config.zig");

const Writer = shared.Writer;
const Runner = runner_mod.Runner;
const Parser = arium.Parser;
const error_reporter = arium.error_reporter;
const Config = config_mod.Config;

pub fn reportConfigDiagnostics(
    path: []const u8,
    diags: *const Config.Diagnostics,
    writer: *const Writer,
) void {
    writer.printf("Test configuration diagnostics for '{s}':\n", .{path});

    for (diags.getEntries()) |*diag| {
        reportConfigDiagnostic(diag, writer);
        writer.print("\n");
    }

    writer.print("\n");
}

pub fn reportConfigDiagnostic(
    diag: *const Config.DiagnosticEntry,
    writer: *const Writer,
) void {
    writer.printf("Line {}: {s}", .{
        diag.position.line,
        diag.message,
    });
}

pub fn reportRunnerDiagnostics(
    diags: *const Runner.Diagnostics,
    writer: *const Writer,
) void {
    for (diags.getEntries()) |*diag| {
        writer.printf("\nDiagnostics for '{s}':\n", .{
            diag.path,
        });
        reportRunnerDiagnostic(diag, writer);
    }
    writer.print("\n");
}

pub fn reportRunnerDiagnostic(
    diag: *const Runner.DiagnosticEntry,
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
            .memory_leak => {
                writer.print("Memory leak.");
            },
        }
    }
}

pub fn reportOutMismatch(
    mismatch: *const Runner.DiagnosticEntry.Mismatch([]const u8),
    writer: *const Writer,
) void {
    writer.printf("Unexpected stdout.\nExpected:\n{s}\nActual:\n{s}", .{
        mismatch.expected,
        mismatch.actual,
    });
}

pub fn reportErrParserMismatch(
    mismatch: *const Runner.DiagnosticEntry.Mismatch(Parser.Diagnostics),
    writer: *const Writer,
) void {
    writer.print("Unexpected parser error.\nExpected:\n");
    reportParserDiags(&mismatch.expected, writer);
    writer.print("\nActual:\n");
    reportParserDiags(&mismatch.actual, writer);
}

pub fn reportParserDiags(
    diags: *const Parser.Diagnostics,
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
            else => {},
        }

        writer.print(".\n");
    }
}
