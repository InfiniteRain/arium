const std = @import("std");
const fs = std.fs;
const Allocator = std.mem.Allocator;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArrayList = std.ArrayList;
const OpenError = fs.File.OpenError;

const arium = @import("arium");
const Output = arium.Output;
const Tokenizer = arium.Tokenizer;
const Parser = arium.Parser;
const InternPool = arium.InternPool;
const Sema = arium.Sema;
const Memory = arium.Memory;
const Compiler = arium.Compiler;
const BuildMode = arium.BuildMode;
const Vm = arium.Vm;
const clap = @import("clap");

const DiagsPrinter = @import("diags_printer.zig").DiagsPrinter;
const ModulePrinter = @import("module_printer.zig").ModulePrinter;
const TreePrinter = @import("tree_printer.zig").TreePrinter;
const VmTracer = @import("vm_tracer.zig").VmTracer;

pub const Error = Allocator.Error || OpenError || error{ReadFailed};

const CliArgs = struct {
    print_byte_code: bool,
    trace_execution: bool,
    print_ast: bool,
    print_air: bool,
    file_name: []const u8,
};

pub fn main() !void {
    var gpa: GeneralPurposeAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = fs.File.stdout().writer(&stdout_buffer);
    const stdout = Output.init(&stdout_writer.interface);

    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer = fs.File.stderr().writer(&stderr_buffer);
    const stderr = Output.init(&stderr_writer.interface);

    const params = comptime clap.parseParamsComptime(
        \\-h, --help          Display help and exit.
        \\--dprint-byte-code  Print compiled bytecode.
        \\--dtrace-execution  Trace code execution.
        \\--dprint-ast        Print parsed AST.
        \\--dprint-air        Print analyzed AIR.
        \\<file>              Script file to execute.
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
        try diag.report(stderr.writer, err);
        try usage(&stderr, &params);
        std.posix.exit(64);
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try usage(&stdout, &params);
        stdout.print("\n\n");
        try options(&stdout, &params);
        return;
    }

    const file_name = res.positionals[0] orelse {
        try usage(&stderr, &params);
        stderr.print("\n");
        return;
    };

    runFile(
        allocator,
        &stdout,
        &stderr,
        .{
            .print_byte_code = res.args.@"dprint-byte-code" > 0,
            .trace_execution = res.args.@"dtrace-execution" > 0,
            .print_ast = res.args.@"dprint-ast" > 0,
            .print_air = res.args.@"dprint-air" > 0,
            .file_name = file_name,
        },
    ) catch |err| switch (err) {
        error.OutOfMemory => {
            stderr.print("Out of memory.\n");
            std.posix.exit(1);
        },
    };
}

fn runFile(
    allocator: Allocator,
    stdout: *const Output,
    stderr: *const Output,
    args: CliArgs,
) Allocator.Error!void {
    const source = readFileAlloc(allocator, args.file_name) catch |err|
        switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                stderr.printf("Failed to read file: {s}.", .{@errorName(err)});
                std.posix.exit(1);
            },
        };
    defer allocator.free(source);

    var tokenizer = Tokenizer.init(source);

    var parser_diags: Parser.Diags = .empty;
    defer parser_diags.deinit(allocator);

    var parser_scratch: Parser.Scratch = .empty;
    defer parser_scratch.deinit(allocator);

    var ast = Parser.parse(
        allocator,
        &tokenizer,
        &parser_diags,
        &parser_scratch,
    ) catch |err| switch (err) {
        error.ParseFailure => {
            DiagsPrinter.printParserDiags(source, &parser_diags, stderr);
            std.posix.exit(1);
        },
        else => |cli_error| return cli_error,
    };
    defer ast.deinit(allocator);

    if (args.print_ast) {
        TreePrinter.printAst(source, stderr, &ast);
    }

    var intern_pool = try InternPool.init(allocator);
    defer intern_pool.deinit(allocator);

    var sema_diags: Sema.Diags = .empty;
    defer sema_diags.deinit(allocator);

    var sema_scratch: Sema.Scratch = .empty;
    defer sema_scratch.deinit(allocator);

    var air = Sema.analyze(
        allocator,
        source,
        &intern_pool,
        &ast,
        &sema_diags,
        &sema_scratch,
    ) catch |err| switch (err) {
        error.AnalyzeFailure => {
            DiagsPrinter.printSemaDiags(
                source,
                &sema_diags,
                &intern_pool,
                stderr,
            );
            std.posix.exit(1);
        },
        else => |cli_error| return cli_error,
    };
    defer air.deinit(allocator);

    if (args.print_air) {
        TreePrinter.printAir(source, stderr, &intern_pool, &air);
    }

    var memory = Memory.init(allocator);
    defer memory.deinit();

    var compiler_diags: Compiler.Diags = .empty;
    defer compiler_diags.deinit(allocator);

    var compiler_scratch: Compiler.Scratch = .empty;
    defer compiler_scratch.deinit(allocator);

    const build_mode: BuildMode =
        if (args.print_byte_code or args.trace_execution)
            .debug
        else
            .release;

    var module = Compiler.compile(
        allocator,
        &memory,
        &intern_pool,
        &air,
        &compiler_diags,
        &compiler_scratch,
        build_mode,
    ) catch |err| switch (err) {
        error.CompileFailure => {
            DiagsPrinter.printCompilerDiags(source, &compiler_diags, stderr);
            std.posix.exit(1);
        },
        else => |cli_error| return cli_error,
    };
    defer module.deinit(allocator);

    if (args.print_byte_code) {
        ModulePrinter.print(&module, stderr);
    }

    const vm_tracer = VmTracer.init(stderr);
    const debug_tracer: ?Vm.DebugTracer =
        if (args.trace_execution)
            vm_tracer.debugTracer()
        else
            null;

    var vm_diags: Vm.Diags = .empty;
    defer vm_diags.deinit(allocator);

    Vm.interpret(&memory, &module, stdout, &vm_diags, debug_tracer) catch |err|
        switch (err) {
            error.Panic => {
                DiagsPrinter.printVmDiags(source, &vm_diags, stderr);
                std.posix.exit(1);
            },
            else => |cli_error| return cli_error,
        };
}

fn readFileAlloc(
    allocator: Allocator,
    file_path: []const u8,
) Error![:0]const u8 {
    const file = try fs.cwd().openFile(
        file_path,
        .{ .mode = .read_only },
    );
    defer file.close();

    var file_buffer: [512]u8 = undefined;
    var reader = file.reader(&file_buffer);
    var file_reader = &reader.interface;

    var buffer: ArrayList(u8) = .empty;
    defer buffer.deinit(allocator);

    file_reader.appendRemaining(allocator, &buffer, .unlimited) catch |err|
        switch (err) {
            error.StreamTooLong => unreachable,
            else => |cli_error| return cli_error,
        };

    return buffer.toOwnedSliceSentinel(allocator, 0);
}

fn usage(output: *const Output, params: anytype) !void {
    output.print("Usage: ");
    try clap.usage(output.writer, clap.Help, params);
}

fn options(output: *const Output, params: anytype) !void {
    output.print("Options:\n");
    try clap.help(output.writer, clap.Help, params, .{});
}
