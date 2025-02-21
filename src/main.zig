const std = @import("std");
const cli_mod = @import("cli.zig");
const Tokenizer = @import("arium").Tokenizer;
const Parser = @import("arium").NewParser;

pub fn main() !void {
    // try cli_mod.runCli();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var tokenizer = Tokenizer.init(
        \\ print 123 + 333 * - 3212
        \\ print "hello"
        \\ print true
        \\ print false
        \\ print asd = 1230
        \\ print not true
        \\ true
    );

    var diags: std.ArrayListUnmanaged(Parser.Diag) = .{};
    var parser = Parser.init(allocator, &diags);
    var ast = try parser.parse(&tokenizer);

    std.debug.print("'{any}'\n", .{
        ast.root.toKey(&ast).block[4].toKey(&ast).print
            .toKey(&ast),
    });

    std.debug.print("{any}\n", .{diags.items});
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
