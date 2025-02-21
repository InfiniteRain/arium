const std = @import("std");
const shared = @import("shared");
const arium = @import("arium");
const runner_mod = @import("runner.zig");
const config_mod = @import("config.zig");

const ArrayListUnmanaged = std.ArrayListUnmanaged;
const comptimePrint = std.fmt.comptimePrint;
const Writer = shared.Writer;
const meta = shared.meta;
const Runner = runner_mod.Runner;
const Token = arium.Token;
const Parser = arium.Parser;
const Loc = arium.Loc;
const Sema = arium.Sema;
const SemaExpr = arium.SemaExpr;
const SemaType = arium.SemaType;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const error_reporter = arium.error_reporter;
const Config = config_mod.Config;

const ReportableTypes = .{
    Token.Tag,
    Parser.Diag,
    Parser.Diag.Tag,
    Sema.DiagEntry,
    Sema.DiagEntry.Kind,
    Sema.DiagEntry.ArityMismatch,
    Sema.DiagEntry.ArgTypeMismatch,
    SemaType,
    SemaType.Fn,
    Sema.DiagEntry.SemaTypeTuple,
    Compiler.DiagEntry,
    Compiler.DiagEntry.Kind,
    Vm.DiagEntry,
    Vm.DiagEntry.Kind,
};

pub fn reportConfigDiags(
    path: []const u8,
    diags: *const Config.Diags,
    source: []const u8,
    writer: *const Writer,
) void {
    writer.printf("Test configuration diags for '{s}':\n", .{path});

    for (diags.getEntries()) |*diag| {
        reportConfigDiag(diag, source, writer);
        writer.print("\n");
    }

    writer.print("\n");
}

pub fn reportConfigDiag(
    diag: *const Config.DiagEntry,
    source: []const u8,
    writer: *const Writer,
) void {
    const line, _ = diag.position.toLineCol(source);
    writer.printf("Line {}: {s}", .{
        line,
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
    for (diag.failures.items) |failure| {
        switch (failure) {
            .parser => |*parse_failure| {
                error_reporter.reportParserDiags(
                    &parse_failure.diags,
                    parse_failure.source,
                    writer,
                );
            },
            .sema => |*sema_failure| {
                error_reporter.reportSemaDiags(
                    &sema_failure.diags,
                    sema_failure.source,
                    writer,
                );
            },
            .compiler => |*compiler_failure| {
                error_reporter.reportCompilerDiags(
                    &compiler_failure.diags,
                    compiler_failure.source,
                    writer,
                );
            },
            .vm => |*vm_failure| {
                error_reporter.reportVmDiags(
                    &vm_failure.diags,
                    vm_failure.source,
                    writer,
                );
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
    mismatch: *const Runner.DiagEntry.Mismatch(ArrayListUnmanaged(Parser.Diag)),
    writer: *const Writer,
) void {
    writer.print("Unexpected parser error(s).\nExpected:\n");
    reportValueMultiline(mismatch.expected.items, 0, null, writer);
    writer.print("\nActual:\n");
    reportValueMultiline(
        mismatch.actual,
        0,
        mismatch.source,
        writer,
    );
}

pub fn reportErrSemaMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Sema.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected sema error(s).\nExpected:\n");
    reportValueMultiline(mismatch.expected.entries, 0, null, writer);
    writer.print("\nActual:\n");
    reportValueMultiline(
        mismatch.actual.entries,
        0,
        mismatch.source,
        writer,
    );
}

pub fn reportErrCompilerMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Compiler.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected compiler error.\nExpected:\n");
    reportValueMultiline(mismatch.expected.entries, 0, null, writer);
    writer.print("\nActual:\n");
    reportValueMultiline(
        mismatch.actual.entries,
        0,
        mismatch.source,
        writer,
    );
}

pub fn reportErrVmMismatch(
    mismatch: *const Runner.DiagEntry.Mismatch(Vm.Diags),
    writer: *const Writer,
) void {
    writer.print("Unexpected vm error.\nExpected:\n");
    reportValueMultiline(mismatch.expected.entries, 0, null, writer);
    writer.print("\nActual:\n");
    reportValueMultiline(
        mismatch.actual.entries,
        0,
        mismatch.source,
        writer,
    );
}

pub fn reportOutMismatch(
    mismatch: *const Runner.DiagEntry.OutMismatch,
    writer: *const Writer,
) void {
    writer.printf("Unexpected stdout.\nExpected:\n{s}\nActual:\n{s}", .{
        mismatch.expected,
        mismatch.actual,
    });
}

fn reportValueMultiline(
    value: anytype,
    indent: u8,
    source_opt: ?[]const u8,
    writer: *const Writer,
) void {
    reportValueAux(value, indent, true, source_opt, writer);
}

fn reportValue(
    value: anytype,
    indent: u8,
    source_opt: ?[]const u8,
    writer: *const Writer,
) void {
    reportValueAux(value, indent, false, source_opt, writer);
}

fn reportValueAux(
    value: anytype,
    indent: u8,
    multiline: bool,
    source_opt: ?[]const u8,
    writer: *const Writer,
) void {
    const Type = @TypeOf(value);
    const type_info = @typeInfo(Type);
    const type_name = @typeName(Type);

    if (Type == Loc) {
        // hack: to be removed after rewrite
        // when no source provided, treat .index as a literal line number
        // when source is present, treat .index as actual position and
        // extrapolate line number from that
        var line = value.index;

        if (source_opt) |source| {
            line, _ = value.toLineCol(source);
        }

        writer.printf("Loc{{ .line = {} }}", .{line});
        return;
    }

    if (Type == []u8 or Type == []const u8) {
        writer.print(value);
        return;
    }

    if (comptime meta.isArrayList(Type)) {
        reportArrayList(value, indent + 1, multiline, source_opt, writer);
        return;
    }

    if (type_info == .pointer and type_info.pointer.size == .one) {
        writer.print("*");
        reportValue(value.*, indent, source_opt, writer);
        return;
    }

    if (type_info == .pointer and type_info.pointer.size == .slice) {
        writer.print("[]");
        reportArray(value, indent, multiline, source_opt, writer);
        return;
    }

    switch (type_info) {
        .void,
        => writer.print("void"),

        .int,
        => writer.printf("{}", .{value}),

        .array,
        => reportArray(value, indent, multiline, source_opt, writer),

        .optional,
        => {
            writer.print("?");
            if (value) |unwrapped| {
                reportValue(unwrapped, indent, source_opt, writer);
            } else {
                writer.print("null");
            }
        },

        .@"enum",
        => if (comptime meta.valueInTuple(Type, ReportableTypes)) {
            reportEnum(value, writer);
        } else {
            @compileError(comptimePrint(
                "enum {s} isn't marked as reportable",
                .{type_name},
            ));
        },

        .@"union",
        => if (comptime meta.valueInTuple(Type, ReportableTypes)) {
            reportUnion(value, indent, source_opt, writer);
        } else {
            @compileError(comptimePrint(
                "union {s} isn't marked as reportable",
                .{type_name},
            ));
        },

        .@"struct",
        => if (comptime meta.valueInTuple(Type, ReportableTypes)) {
            reportStruct(value, indent, source_opt, writer);
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

pub fn reportStruct(
    value: anytype,
    indent: u8,
    source_opt: ?[]const u8,
    writer: *const Writer,
) void {
    const Type = @TypeOf(value);
    const type_info = @typeInfo(Type);

    writer.printf("{s}{{", .{meta.typeName(Type)});

    inline for (type_info.@"struct".fields, 0..) |field, index| {
        writer.printf(" .{s} = ", .{field.name});
        reportValue(@field(value, field.name), indent, source_opt, writer);

        if (index != type_info.@"struct".fields.len - 1) {
            writer.print(",");
        }
    }

    writer.print(" }");
}

pub fn reportUnion(
    value: anytype,
    indent: u8,
    source_opt: ?[]const u8,
    writer: *const Writer,
) void {
    writer.printf(
        "{s}{{ .{s} = ",
        .{ meta.typeName(@TypeOf(value)), @tagName(value) },
    );

    inline for (@typeInfo(@TypeOf(value)).@"union".fields) |field| {
        if (std.mem.eql(u8, field.name, @tagName(value))) {
            reportValue(@field(value, field.name), indent, source_opt, writer);
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
    multiline: bool,
    source_opt: ?[]const u8,
    writer: *const Writer,
) void {
    reportArray(value.items, indent, multiline, source_opt, writer);
}

pub fn reportArray(
    value: anytype,
    indent: u8,
    multiline: bool,
    source_opt: ?[]const u8,
    writer: *const Writer,
) void {
    writer.print("[");

    if (multiline) {
        writer.print("\n");
    }

    for (value, 0..) |item, index| {
        if (multiline) {
            writeIndent(indent, writer);
        }

        reportValue(item, indent, source_opt, writer);

        if (index != value.len - 1) {
            writer.print(",");
        }

        if (multiline) {
            writer.print("\n");
        }
    }

    if (indent > 1 and multiline) {
        writeIndent(indent - 1, writer);
    }

    writer.print("]");
}

pub fn writeIndent(indent: u8, writer: *const Writer) void {
    for (0..indent * 2) |_| {
        writer.print(" ");
    }
}
