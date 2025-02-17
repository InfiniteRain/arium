const std = @import("std");
const cli_mod = @import("cli.zig");
const Tokenizer = @import("arium").Tokenizer;
const Ast = @import("ast").Ast;

pub fn main() !void {
    try cli_mod.runCli();
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
