const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const arium = @import("arium");
const Output = arium.Output;
const VmTracer = arium.VmTracer;

const cli_mod = @import("cli_new.zig");

pub fn main2() !void {
    try cli_mod.runCli();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const source = @embedFile("test.aum");

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const output = Output.init(stdout);

    var tokenizer = arium.Tokenizer.init(source);

    var parser_diags: arium.Parser.Diags = .empty;
    defer parser_diags.deinit(allocator);

    var parser_scratch: arium.Parser.Scratch = .empty;
    defer parser_scratch.deinit(allocator);

    var ast = arium.Parser.parse(
        allocator,
        &tokenizer,
        &parser_diags,
        &parser_scratch,
    ) catch |err| {
        for (parser_diags.entries.items) |item| {
            std.debug.print(
                "{any} in '{s}'\n",
                .{ item.tag, source[item.loc.index..][0..item.loc.len] },
            );
        }
        return err;
    };

    arium.debug_ast_reporter.printAstIndex(
        arium.Ast.Index,
        arium.Ast.Key,
        source,
        null,
        &ast,
        arium.Ast.Index.from(0),
        null,
        &output,
    );

    var intern_pool = try arium.InternPool.init(allocator);

    var sema_diags: arium.SemaNew.Diags = .empty;
    defer sema_diags.deinit(allocator);

    var sema_scratch: arium.SemaNew.Scratch = .empty;
    defer sema_scratch.deinit(allocator);

    var air = arium.SemaNew.analyze(
        allocator,
        source,
        &intern_pool,
        &ast,
        &sema_diags,
        &sema_scratch,
    ) catch |err| {
        for (sema_diags.entries.items) |item| {
            std.debug.print(
                "{any} in '{s}'\n",
                .{ item.tag, source[item.loc.index..][0..item.loc.len] },
            );
            switch (item.tag) {
                .unexpected_expr_type => |x| {
                    std.debug.print("expected: {any}\n", .{x.expected.slice()});
                },
                else => {},
            }

            std.debug.print("\n\n", .{});
        }
        return err;
    };
    defer air.deinit(allocator);

    const root = arium.Air.Index.from(0);

    arium.debug_ast_reporter.printAstIndex(
        arium.Air.Index,
        arium.Air.Key,
        source,
        &intern_pool,
        &air,
        root,
        null,
        &output,
    );

    var memory = arium.Memory.init(allocator);
    defer memory.deinit();

    var compiler_diags: arium.CompilerNew.Diags = .empty;
    defer compiler_diags.deinit(allocator);

    var compiler_scratch: arium.CompilerNew.Scratch = .empty;
    defer compiler_scratch.deinit(allocator);

    var module = try arium.CompilerNew.compile(
        allocator,
        &memory,
        root,
        &intern_pool,
        &air,
        &compiler_diags,
        &compiler_scratch,
        .release,
    );
    defer module.deinit(allocator);

    arium.module_reporter.printModule(&module, &output);

    // var vm_tracer = VmTracer.init(allocator, &output);

    var vm_diags: arium.VmNew.Diags = .empty;
    defer vm_diags.deinit(allocator);

    arium.VmNew.interpret(
        &memory,
        &module,
        &output,
        &vm_diags,
        null,
        // vm_tracer.debugTracer(),
    ) catch |err| {
        if (vm_diags.entry) |entry| {
            std.debug.print(
                "{any} in '{s}'\n",
                .{ entry.tag, source[entry.loc.index..][0..entry.loc.len] },
            );
        }
        return err;
    };
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
