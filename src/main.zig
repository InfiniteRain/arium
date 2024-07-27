const std = @import("std");
const cli_mod = @import("cli.zig");

pub fn main() !void {
    try cli_mod.runCli();

    // todo: extract into tests
    // const source =
    //     // \\ 10 > 30 == true or 40 > 30 == true
    //     // \\10 > 30 and (20 < 30 or 30 < 50)
    //     // \\ 1 == 1 and (1 == 1 and (1 == 1 and 1 == 1)) or 2 == 2 and 2 == 2
    //     // \\ 1 == 1 and 1 == 1 and 1 == 1 and 1 == 1 or 2 == 2 and 2 == 2
    //     // \\ (5 < 4) == false and (true or (false and (true and true)))
    //     // \\ 10 > 5 == true or 20 > 6 == true or 40 > 30 == true == true
    //     // \\ 10 > 5 == (1 == 1 and 1 == 1) or 20 > 6 == true or 40 > 30 == true == true
    //     // \\ 10 > 5 and 20 > 5 or 30 > 5 and 40 > 50
    //     // \\ 10 > 5 == true or 20 > 10
    //
    //     // \\ 10 > 5 and (20 > 5 or (30 > 5 and 40 > 5))
    //     // \\ 10 > 5 and (20 > 5 and (30 > 5 and 40 > 5))
    //     // \\ 10 > 50 != true and 30 > 40 == false and 20 == 20 or 40 == 30
    //     // \\ (10 > 5 and 30 > 5) == 10 > 5
    //     // \\ 10 > 5 or 20 > 5 or 30 > 6
    //     // \\ 10 > 5 and 20 > 5 or 20 > 3 and 30 > 3
    //     // \\ (10 > 5 and (4 > 3 or (40 > 3 and 3 > 2))) == (1 == 1 and 1 == 1)
    //     \\ print "Hello, world!" print "Bye, world!"
    // ;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
