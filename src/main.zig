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

    // todo: extract into tests
    const source =
        // \\ 10 > 30 == true or 40 > 30 == true
        // \\10 > 30 and (20 < 30 or 30 < 50)
        // \\ 1 == 1 and (1 == 1 and (1 == 1 and 1 == 1)) or 2 == 2 and 2 == 2
        // \\ 1 == 1 and 1 == 1 and 1 == 1 and 1 == 1 or 2 == 2 and 2 == 2
        // \\ (5 < 4) == false and (true or (false and (true and true)))
        // \\ 10 > 5 == true or 20 > 6 == true or 40 > 30 == true == true
        // \\ 10 > 5 == (1 == 1 and 1 == 1) or 20 > 6 == true or 40 > 30 == true == true
        // \\ 10 > 5 and 20 > 5 or 30 > 5 and 40 > 50
        // \\ 10 > 5 == true or 20 > 10

        // \\ 10 > 5 and (20 > 5 or (30 > 5 and 40 > 5))
        // \\ 10 > 5 and (20 > 5 and (30 > 5 and 40 > 5))
        // \\ 10 > 50 != true and 30 > 40 == false and 20 == 20 or 40 == 30
        // \\ (10 > 5 and 30 > 5) == 10 > 5
        // \\ 10 > 5 or 20 > 5 or 30 > 6
        // \\ 10 > 5 and 20 > 5 or 20 > 3 and 30 > 3
        \\ (10 > 5 and (4 > 3 or (40 > 3 and 3 > 2))) == (1 == 1 and 1 == 1)
    ;
    var tokenizer = try Tokenizer.init(allocator, source);
    defer tokenizer.deinit();

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

    // io.out("\n== SEMA ==\n");
    // sema_expr.print(&io);
    // io.out("\n");

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
