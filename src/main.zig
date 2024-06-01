const std = @import("std");
const io_handler = @import("io_handler.zig");
const tokenizer_mod = @import("parser/tokenizer.zig");
const parser_mod = @import("parser/parser.zig");
const managed_memory_mod = @import("state/managed_memory.zig");
const compiler_mod = @import("compiler/compiler.zig");
const chunk_mod = @import("compiler/chunk.zig");
const vm_mod = @import("vm/vm.zig");
const sema_mod = @import("sema/sema.zig");
const hash_table_mod = @import("state/hash_table.zig");

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

    const source =
        \\0 < 10 == true
    ;
    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(allocator);
    defer parser.deinit();

    const parsed_expr = parser.parse(&tokenizer) catch |err| switch (err) {
        error.ParseFailure => {
            for (parser.errs.items) |parser_err| {
                io.outf("Error at {}:{}: {s}\n", .{
                    parser_err.token.position.line,
                    parser_err.token.position.column,
                    parser_err.message,
                });
            }
            return;
        },
        else => return err,
    };
    defer parsed_expr.destroy(allocator);

    io.out("\n== TREE ==\n");
    parsed_expr.print(&io);
    io.out("\n");

    var sema = Sema.init(allocator);
    defer sema.deinit();

    var sema_expr = sema.analyze(parsed_expr) catch |err| switch (err) {
        error.SemaFailure => {
            for (sema.errs.items) |sema_err| {
                io.outf("Error at {}:{}: {s}\n", .{
                    sema_err.position.line,
                    sema_err.position.column,
                    sema_err.message,
                });
            }
            return;
        },
        else => return err,
    };
    defer sema_expr.destroy(allocator);

    var memory = ManagedMemory.init(allocator);
    defer memory.deinit();

    try Compiler.compile(&memory, sema_expr);

    io.out("== CHUNK ==\n");
    memory.vm_state.?.chunk.print(&io);

    io.out("===========\n");
    try Vm.interpret(&memory, &io);
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
