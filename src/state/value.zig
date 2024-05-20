const std = @import("std");
const io_handler_mod = @import("../io_handler.zig");

const IoHandler = io_handler_mod.IoHandler;

pub const Value = union(enum) {
    const Self = @This();

    int: i64,
    float: f64,
    bool: bool,

    pub fn print(self: Self, io: *IoHandler) void {
        switch (self) {
            .int => io.outf("{}", .{self.int}),
            .float => io.outf("{d}", .{self.float}),
            .bool => io.outf("{}", .{self.bool}),
        }
    }
};
