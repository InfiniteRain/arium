const shared = @import("shared");
const obj_mod = @import("obj.zig");

const Writer = shared.Writer;
const Obj = obj_mod.Obj;

pub const Value = union(enum) {
    unit,
    int: i64,
    float: f64,
    bool: bool,
    obj: *Obj,
};
