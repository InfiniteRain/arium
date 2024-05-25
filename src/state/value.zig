const std = @import("std");
const io_handler_mod = @import("../io_handler.zig");
const object_mod = @import("object.zig");

const IoHandler = io_handler_mod.IoHandler;
const Object = object_mod.Object;

pub const Value = union(enum) {
    const Self = @This();

    unit,
    int: i64,
    float: f64,
    bool: bool,
    object: *Object,

    pub fn print(self: Self, io: *IoHandler) void {
        switch (self) {
            .unit => io.out("unit"),
            .int => io.outf("{}", .{self.int}),
            .float => io.outf("{d}", .{self.float}),
            .bool => io.outf("{}", .{self.bool}),
            .object => |object| switch (object.kind) {
                .string => io.outf("{s}", .{object.as(Object.String).chars}),
            },
        }
    }
};
