const std = @import("std");
const cli_mod = @import("cli.zig");
const arium = @import("arium");
const Writer = @import("shared").Writer;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const source =
        \\ print 123
        \\ 123
        \\ print 123
        \\ print -(123.123)
        \\ let foo = "hello"
        \\ do
        \\    let kek = 123
        \\    let foo = "hello2"
        \\    do
        \\       let a = 123
        \\       let b = 123
        \\    end
        \\    let c = 123
        \\ end
        \\ let a = for true do
        \\    print("hello")
        \\    
        \\ end
    ;

    const stdout = std.io.getStdOut().writer().any();
    const stdout_writer = Writer.init(&stdout);

    var tokenizer = arium.Tokenizer.init(source);

    var parser_diags: std.ArrayListUnmanaged(arium.Parser.Diag) = .empty;
    defer parser_diags.deinit(allocator);

    var parser = arium.Parser.init(allocator);
    defer parser.deinit();

    var ast = try parser.parse(&tokenizer, &parser_diags);
    defer ast.deinit(allocator);

    arium.debug_ast_reporter.printAstIndex(
        arium.Ast.Index,
        arium.Ast.Key,
        null,
        &ast,
        arium.Ast.Index.from(0),
        null,
        &stdout_writer,
    );

    var intern_pool = try arium.InternPool.init(allocator);

    var sema_diags: std.ArrayListUnmanaged(arium.SemaNew.Diag) = .empty;
    defer sema_diags.deinit(allocator);

    var scratch: std.ArrayListUnmanaged(arium.Air.Index) = .empty;
    defer scratch.deinit(allocator);

    var air = arium.SemaNew.analyze(
        allocator,
        &intern_pool,
        &ast,
        &sema_diags,
        &scratch,
    ) catch |err| {
        for (sema_diags.items) |item| {
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
        &intern_pool,
        &air,
        root,
        null,
        &stdout_writer,
    );
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
