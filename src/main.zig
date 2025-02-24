const std = @import("std");
const cli_mod = @import("cli.zig");
const Tokenizer = @import("arium").Tokenizer;
const Parser = @import("arium").NewParser;
const Ast = @import("arium").Ast;

const K = packed struct {
    foo: u32,
    bar: u32,
};

pub fn main() !void {
    // try cli_mod.runCli();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var tokenizer = Tokenizer.init(
        \\ print 5 <= 10
    );

    var diags: std.ArrayListUnmanaged(Parser.Diag) = .{};
    var parser = Parser.init(allocator);
    defer parser.deinit();

    var ast = parser.parse(&tokenizer, &diags) catch {
        std.debug.print("{any}\n", .{diags.items});
        return;
    };

    const idx = Ast.Index.from(0).toKey(&ast).block[0].toKey(&ast).print;

    std.debug.print("{any}", .{idx.toKey(&ast)});
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
