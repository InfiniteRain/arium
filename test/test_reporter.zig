const std = @import("std");
const shared = @import("shared");
const arium = @import("arium");
const runner_mod = @import("runner.zig");
const config_mod = @import("config.zig");

const comptimePrint = std.fmt.comptimePrint;
const Writer = shared.Writer;
const meta = shared.meta;
const Runner = runner_mod.Runner;
const Token = arium.Token;
const Parser = arium.Parser;
const Position = arium.Position;
const Sema = arium.Sema;
const SemaExpr = arium.SemaExpr;
const SemaType = arium.SemaType;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const error_reporter = arium.error_reporter;
const Config = config_mod.Config;

const ReportableTypes = .{
    Token.Kind,
    Parser.DiagEntry,
    Parser.DiagEntry.Kind,
    Sema.DiagEntry,
    Sema.DiagEntry.Kind,
    SemaType,
    Sema.DiagEntry.SemaTypeTuple,
    Compiler.DiagEntry,
    Compiler.DiagEntry.Kind,
    Vm.DiagEntry,
    Vm.DiagEntry.Kind,
};

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
    reportValue(mismatch.expected.entries, 0, writer);
    writer.print("\nActual:\n");
    reportValue(mismatch.actual.entries, 0, writer);
}

pub fn reportErrSemaMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Sema.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected sema error(s).\nExpected:\n");
    reportValue(mismatch.expected.entries, 0, writer);
    writer.print("\nActual:\n");
    reportValue(mismatch.actual.entries, 0, writer);
}

pub fn reportErrCompilerMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Compiler.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected compiler error.\nExpected:\n");
    reportValue(mismatch.expected.entries, 0, writer);
    writer.print("\nActual:\n");
    reportValue(mismatch.actual.entries, 0, writer);
}

pub fn reportErrVmMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Vm.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected vm error.\nExpected:\n");
    reportValue(mismatch.expected.entries, 0, writer);
    writer.print("\nActual:\n");
    reportValue(mismatch.actual.entries, 0, writer);
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

pub fn reportValue(
    value: anytype,
    indent: u8,
    writer: *const Writer,
) void {
    const Type = @TypeOf(value);
    const type_info = @typeInfo(Type);
    const type_name = @typeName(Type);

    if (Type == Position) {
        // special case here as we ignore the column for now.
        writer.printf("Position{{ .line = {} }}", .{value.line});
        return;
    }

    if (Type == []u8 or Type == []const u8) {
        writer.print(value);
        return;
    }

    if (comptime meta.isArrayList(Type)) {
        reportArrayList(value, indent, writer);
        return;
    }

    switch (type_info) {
        .Void,
        => writer.print("void"),

        .Int,
        => writer.printf("{}", .{value}),

        .Enum,
        => if (comptime meta.typeInTuple(Type, ReportableTypes)) {
            reportEnum(value, writer);
        } else {
            @compileError(comptimePrint(
                "enum {s} isn't marked as reportable",
                .{type_name},
            ));
        },

        .Union,
        => if (comptime meta.typeInTuple(Type, ReportableTypes)) {
            reportUnion(value, indent, writer);
        } else {
            @compileError(comptimePrint(
                "union {s} isn't marked as reportable",
                .{type_name},
            ));
        },

        .Struct,
        => if (comptime meta.typeInTuple(Type, ReportableTypes)) {
            reportStruct(value, indent, writer);
        } else {
            @compileError(comptimePrint(
                "struct {s} isn't marked as reportable",
                .{type_name},
            ));
        },

        else => @compileError(comptimePrint(
            "no reporting exists for {s} / {s}",
            .{ type_name, @tagName(type_info) },
        )),
    }
}

pub fn reportStruct(value: anytype, indent: u8, writer: *const Writer) void {
    const Type = @TypeOf(value);
    const type_info = @typeInfo(Type);

    writer.printf("{s}{{", .{meta.typeName(Type)});

    inline for (type_info.Struct.fields, 0..) |field, index| {
        writer.printf(" .{s} = ", .{field.name});
        reportValue(@field(value, field.name), indent, writer);

        if (index != type_info.Struct.fields.len - 1) {
            writer.print(",");
        }
    }

    writer.print(" }");
}

pub fn reportUnion(value: anytype, indent: u8, writer: *const Writer) void {
    writer.printf(
        "{s}{{ .{s} = ",
        .{ meta.typeName(@TypeOf(value)), @tagName(value) },
    );

    inline for (@typeInfo(@TypeOf(value)).Union.fields) |field| {
        if (std.mem.eql(u8, field.name, @tagName(value))) {
            reportValue(@field(value, field.name), indent, writer);
        }
    }

    writer.print(" }");
}

pub fn reportEnum(value: anytype, writer: *const Writer) void {
    writer.printf("{s}.{s}", .{ meta.typeName(@TypeOf(value)), @tagName(value) });
}

pub fn reportArrayList(
    value: anytype,
    indent: u8,
    writer: *const Writer,
) void {
    writeIndent(indent, writer);
    writer.print("[\n");

    for (value.items, 0..) |item, index| {
        writeIndent(indent + 1, writer);
        reportValue(item, indent, writer);

        if (index != value.items.len - 1) {
            writer.print(",");
        }
        writer.print("\n");
    }

    writeIndent(indent, writer);
    writer.print("]");
}

pub fn writeIndent(indent: u8, writer: *const Writer) void {
    for (0..indent * 2) |_| {
        writer.print(" ");
    }
}
