const std = @import("std");
const cli_mod = @import("cli.zig");

pub fn main() !void {
    try cli_mod.runCli();
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
