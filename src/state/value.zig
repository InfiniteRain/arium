const std = @import("std");
const io_handler_mod = @import("../io_handler.zig");
const obj_mod = @import("obj.zig");

const IoHandler = io_handler_mod.IoHandler;
const Obj = obj_mod.Obj;

pub const Value = union(enum) {
    const Self = @This();

    unit,
    int: i64,
    float: f64,
    bool: bool,
    obj: *Obj,

    pub fn print(self: Self, io: *IoHandler) void {
        switch (self) {
            .unit => io.out("unit"),
            .int => io.outf("{}", .{self.int}),
            .float => io.outf("{d}", .{self.float}),
            .bool => io.outf("{}", .{self.bool}),
            .obj => |obj| switch (obj.kind) {
                .string => io.outf("{s}", .{obj.as(Obj.String).chars}),
            },
        }
    }
};
