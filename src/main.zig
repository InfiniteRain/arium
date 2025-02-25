const std = @import("std");
const cli_mod = @import("cli.zig");
const Tokenizer = @import("arium").Tokenizer;
const Parser = @import("arium").NewParser;
const Ast = @import("arium").Ast;

pub fn main() !void {
    try cli_mod.runCli();
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    //
    // var tokenizer = Tokenizer.init(
    //     \\ fn hello_world(a: Int, b: String): Unit
    //     \\     print "hello, world"
    //     \\ end
    // );
    //
    // var diags: std.ArrayListUnmanaged(Parser.Diag) = .{};
    // var parser = Parser.init(allocator);
    // defer parser.deinit();
    //
    // var ast = parser.parse(&tokenizer, &diags) catch {
    //     std.debug.print("{any}\n", .{diags.items});
    //     return;
    // };
    //
    // const idx = Ast.Index.from(0).toKey(&ast).block[0].toKey(&ast);
    //
    // std.debug.print("{s}", .{idx.@"fn".params[1].identifier.toLoc(&ast).toSlice(tokenizer.source)});
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
