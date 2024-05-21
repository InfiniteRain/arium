const std = @import("std");
const io_handler = @import("io_handler.zig");
const tokenizer_mod = @import("parser/tokenizer.zig");
const parser_mod = @import("parser/parser.zig");
const managed_memory_mod = @import("state/managed_memory.zig");
const compiler_mod = @import("compiler/compiler.zig");
const chunk_mod = @import("compiler/chunk.zig");
const vm_mod = @import("vm/vm.zig");
const sema_mod = @import("sema/sema.zig");

const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const IoHandler = io_handler.IoHandler;
const Tokenizer = tokenizer_mod.Tokenizer;
const Parser = parser_mod.Parser;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const Compiler = compiler_mod.Compiler;
const OpCode = chunk_mod.OpCode;
const Vm = vm_mod.Vm;
const Sema = sema_mod.Sema;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var stdout = std.io.getStdOut().writer();
    var stderr = std.io.getStdErr().writer();
    var stdin = std.io.getStdIn().reader();

    var io = try IoHandler.init(allocator, &stdin, &stdout, &stderr);
    defer io.deinit();

    const source = "-true";
    var tokenizer = Tokenizer.init(source);

    var parser = Parser.init(&tokenizer);
    const expr = parser.parse(allocator) catch |err| switch (err) {
        error.ParseFailure => {
            // todo: MEMORY LEAK HERE
            io.outf("{s}\n", .{parser.err.?.message});
            return;
        },
        else => return err,
    };
    defer expr.destroy(allocator);

    io.out("== TREE ==\n");
    expr.print(&io);
    io.out("\n");

    var sema = Sema.init(allocator);
    defer sema.deinit();

    var memory = ManagedMemory.init(allocator);
    defer memory.deinit();

    var sema_expression = sema.analyze(expr) catch |err| switch (err) {
        error.SemaFailure => {
            for (sema.errs.items) |item| {
                io.outf("Error at {}:{}: {s}\n", .{
                    item.position.line,
                    item.position.column,
                    item.message,
                });
            }
            return;
        },
        else => return err,
    };
    defer sema_expression.destroy(allocator);

    try Compiler.compile(&memory, sema_expression);

    io.out("== CHUNK ==\n");
    memory.vm_state.?.chunk.print(&io);

    io.out("===========\n");
    try Vm.interpret(&memory, &io);
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
