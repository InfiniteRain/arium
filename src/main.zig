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
        \\ print not true
        \\ true;
    );

    var parser = Parser.init(allocator, null);
    var ast = try parser.parse(&tokenizer);

    std.debug.print("{any}\n", .{ast.root.toKey(&ast)});
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
