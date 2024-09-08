const std = @import("std");

pub fn spread(a: anytype, b: anytype) @TypeOf(a) {
    var result = a;

    inline for (@typeInfo(@TypeOf(b)).Struct.fields) |field| {
        @field(result, field.name) = @field(b, field.name);
    }

    return result;
}

pub fn stringToUnion(T: type, name: []const u8) ?T {
    inline for (@typeInfo(T).Union.fields) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            return @unionInit(T, field.name, undefined);
        }
    }

    return null;
}

pub fn setUnionValue(union_: anytype, value: anytype) void {
    const typeInfo = @typeInfo(@TypeOf(union_));

    inline for (@typeInfo(typeInfo.Pointer.child).Union.fields) |field| {
        if (std.mem.eql(u8, @tagName(union_.*), field.name) and
            @TypeOf(value) == field.type)
        {
            union_.* = @unionInit(
                typeInfo.Pointer.child,
                field.name,
                value,
            );
        }
    }
}

pub fn getUnionValue(union_: anytype, T: type) T {
    const typeInfo = @typeInfo(@TypeOf(union_));

    inline for (@typeInfo(typeInfo.Pointer.child).Union.fields) |field| {
        if (std.mem.eql(u8, @tagName(union_.*), field.name) and
            T == field.type)
        {
            return @field(union_, field.name);
        }
    }

    @panic("value of the active field doesn't match desired type");
}

pub fn typeName(T: type) []const u8 {
    const full_name = @typeName(T);
    const last_dot_index = std.mem.lastIndexOfScalar(u8, full_name, '.') orelse
        return full_name;
    return full_name[last_dot_index + 1 ..];
}
