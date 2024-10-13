const shared = @import("shared");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("../state/obj.zig");

const Value = value_mod.Value;
const Writer = shared.Writer;
const Obj = obj_mod.Obj;

pub fn printValue(value: Value, writer: *const Writer) void {
    switch (value) {
        .unit => writer.print("unit"),
        .int => |int| writer.printf("{}", .{int}),
        .float => |float| writer.printf("{d}", .{float}),
        .bool => |bool_| writer.printf("{}", .{bool_}),
        .obj => |obj| switch (obj.kind) {
            .string => writer.printf("{s}", .{obj.as(Obj.String).chars}),
            .@"fn" => writer.printf("<fn {s}>", .{
                if (obj.as(Obj.Fn).name) |name|
                    name.chars
                else
                    "?",
            }),
        },
    }
}
