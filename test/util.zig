const std = @import("std");
const mem = std.mem;

const arium = @import("arium");
const FixedArray = arium.FixedArray;

pub fn checkForFixedArray(T: type) ?struct { T: type, capacity: usize } {
    const type_info = @typeInfo(T);

    if (type_info != .@"struct") {
        return null;
    }

    const struct_info = type_info.@"struct";

    if (!mem.eql(u8, struct_info.fields[0].name, "buffer")) {
        return null;
    }

    const buffer_field = struct_info.fields[0];
    const buffer_field_info = @typeInfo(buffer_field.type);

    if (buffer_field_info != .array) {
        return null;
    }

    const array_info = buffer_field_info.array;

    if (T != FixedArray(array_info.child, array_info.len)) {
        return null;
    }

    return .{ .T = array_info.child, .capacity = array_info.len };
}
