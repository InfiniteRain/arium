const shared = @import("shared");
const obj_mod = @import("obj.zig");

const Writer = shared.Writer;
const Obj = obj_mod.Obj;

pub const Value = union(enum) {
    const Self = @This();

    unit,
    int: i64,
    float: f64,
    bool: bool,
    obj: *Obj,

    pub fn print(self: Self, writer: *const Writer) void {
        switch (self) {
            .unit => writer.print("unit"),
            .int => writer.printf("{}", .{self.int}),
            .float => writer.printf("{d}", .{self.float}),
            .bool => writer.printf("{}", .{self.bool}),
            .obj => |obj| switch (obj.kind) {
                .string => writer.printf("{s}", .{obj.as(Obj.String).chars}),
            },
        }
    }
};
