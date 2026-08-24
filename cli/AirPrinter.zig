const std = @import("std");
// const meta = std.meta;
// const fmt = std.fmt;

const arium = @import("arium");
const Output = arium.Output;
const InternPool = arium.InternPool;
const Air = arium.Air;

const AirPrinter = @This();

source: []const u8,
output: *const Output,
intern_pool: *const InternPool,
air: *const Air,

pub fn print(
    source: []const u8,
    output: *const Output,
    intern_pool: *const InternPool,
    air: *const Air,
) void {}
    const air_printer: AirPrinter = .{
        .source = source,
        .output = output,
        .intern_pool = intern_pool,
        .air = air,
    };

    //
}
