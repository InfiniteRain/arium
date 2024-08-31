const std = @import("std");
const shared = @import("shared");
const clap = @import("clap");
const tokenizer_mod = @import("parser/tokenizer.zig");
const parser_mod = @import("parser/parser.zig");
const sema_mod = @import("sema/sema.zig");
const managed_memory_mod = @import("state/managed_memory.zig");
const compiler_mod = @import("compiler/compiler.zig");
const vm_mod = @import("vm/vm.zig");
const error_reporter = @import("reporter/error_reporter.zig");
const debug_reporter = @import("reporter/debug_reporter.zig");

const Allocator = std.mem.Allocator;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const Writer = shared.Writer;
const Tokenizer = tokenizer_mod.Tokenizer;
const Parser = parser_mod.Parser;
const Sema = sema_mod.Sema;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const Compiler = compiler_mod.Compiler;
const Vm = vm_mod.Vm;

pub fn runCli() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut().writer().any();
    const stderr = std.io.getStdErr().writer().any();

    const stdout_writer = Writer.init(&stdout);
    const stderr_writer = Writer.init(&stderr);

    const params = comptime clap.parseParamsComptime(
        \\-h, --help         Display help and exit.
        \\--dprint-byte-code Print compiled bytecode.
        \\--dtrace-execution Trace code execution.
        \\<file>             Script file to execute.
        \\
    );

    const parsers = comptime .{
        .file = clap.parsers.string,
    };

    var diag = clap.Diagnostic{};
    const res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        diag.report(stderr, err) catch {};
        try usage(&stderr_writer, &params);
        std.posix.exit(64);
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try usage(&stdout_writer, &params);
        stdout_writer.print("\n\n");
        try options(&stdout_writer, &params);
        return;
    }

    if (res.positionals.len == 0) {
        try usage(&stderr_writer, &params);
        stderr_writer.print("\n");
        return;
    }

    try runFile(
        allocator,
        &stdout_writer,
        &stderr_writer,
        res.positionals[0],
        res.args,
    );
}

fn runFile(
    allocator: Allocator,
    out_writer: *const Writer,
    err_writer: *const Writer,
    file_path: []const u8,
    args: anytype,
) !void {
    const source = try readFileAlloc(allocator, file_path, out_writer);

    var tokenizer = Tokenizer.init(source);

    var parser = Parser.init(allocator);
    var parser_diags = Parser.Diags.init(allocator);
    defer parser_diags.deinit();

    const sema_expr = parser.parse(&tokenizer, &parser_diags) catch |err| switch (err) {
        error.ParseFailure => {
            error_reporter.reportParserDiags(&parser_diags, err_writer);
            std.posix.exit(65);
        },
        else => return err,
    };
    defer sema_expr.destroy(allocator);

    var sema = Sema.init(allocator);
    var sema_diags = Sema.Diags.init(allocator);
    defer sema_diags.deinit();

    var sema_block = sema.analyze(sema_expr, &sema_diags) catch |err| switch (err) {
        error.SemaFailure => {
            error_reporter.reportSemaDiags(&sema_diags, err_writer);
            std.posix.exit(65);
        },
        else => return err,
    };
    defer sema_block.destroy(allocator);

    var memory = ManagedMemory.init(allocator);
    defer memory.deinit();

    var compiler_diags = Compiler.Diags.init(allocator);
    defer compiler_diags.deinit();

    Compiler.compile(&memory, sema_block, &compiler_diags) catch |err| switch (err) {
        error.CompileFailure => {
            error_reporter.reportCompilerDiags(&compiler_diags, err_writer);
            std.posix.exit(65);
        },
        else => return err,
    };

    if (args.@"dprint-byte-code" > 0) {
        out_writer.print("== CHUNK ==\n");
        debug_reporter.reportChunk(&memory.vm_state.?.chunk, out_writer);
    }

    var vm_diags = Vm.Diags.init(allocator);
    defer vm_diags.deinit();

    if (args.@"dtrace-execution" > 0) {
        out_writer.print("\n== EXECUTION ==\n");
    }

    Vm.interpret(&memory, out_writer, &vm_diags, .{
        .debug_writer = out_writer,
        .debugExecutionIteration = if (args.@"dtrace-execution" > 0)
            debug_reporter.reportExecutionIteration
        else
            null,
    }) catch |err| switch (err) {
        error.Panic => {
            error_reporter.reportVmDiags(&vm_diags, err_writer);
            std.posix.exit(65);
        },
        else => return err,
    };
}

fn readFileAlloc(
    allocator: Allocator,
    file_path: []const u8,
    writer: *const Writer,
) ![]u8 {
    const file = std.fs.cwd().openFile(file_path, .{ .mode = .read_only }) catch {
        writer.printf("Could not open file '{s}'.\n", .{file_path});
        std.posix.exit(74);
    };
    defer file.close();

    try file.seekFromEnd(0);
    const end = try file.getPos();
    try file.seekTo(0);

    return file.reader().readAllAlloc(allocator, end) catch {
        writer.printf("Not enough memory to read '{s}'.\n", .{file_path});
        std.posix.exit(74);
    };
}

fn usage(writer: *const Writer, params: anytype) !void {
    writer.print("Usage: ");
    try clap.usage(writer.backing_writer.*, clap.Help, params);
}

fn options(writer: *const Writer, params: anytype) !void {
    writer.print("Options:\n");
    try clap.help(writer.backing_writer.*, clap.Help, params, .{});
}
