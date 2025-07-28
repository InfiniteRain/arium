const std = @import("std");
const builtin = @import("builtin");

const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const assert = std.debug.assert;
const meta = std.meta;

pub fn spread(a: anytype, b: anytype) @TypeOf(a) {
    var result = a;

    inline for (@typeInfo(@TypeOf(b)).@"struct".fields) |field| {
        @field(result, field.name) = @field(b, field.name);
    }

    return result;
}

pub fn typeName(T: type) []const u8 {
    const full_name = @typeName(T);
    const last_dot_index = std.mem.lastIndexOfScalar(u8, full_name, '.') orelse
        return full_name;
    return full_name[last_dot_index + 1 ..];
}

pub fn isArrayList(T: type) bool {
    const type_info = @typeInfo(T);

    if (type_info != .@"struct" or
        !@hasDecl(T, "Slice") or
        @typeInfo(T.Slice) != .pointer)
    {
        return false;
    }

    const Child = @typeInfo(T.Slice).pointer.child;

    return T == ArrayList(Child) or T == ArrayListUnmanaged(Child);
}

pub fn valueInTuple(value: anytype, tuple: anytype) bool {
    const TupleType = @TypeOf(tuple);
    const tuple_type_info = @typeInfo(TupleType);

    if (tuple_type_info != .@"struct" or !tuple_type_info.@"struct".is_tuple) {
        @compileError("should be a tuple");
    }

    inline for (tuple_type_info.@"struct".fields) |field| {
        if (value == @field(tuple, field.name)) {
            return true;
        }
    }

    return false;
}

pub fn ReturnType(@"fn": anytype) type {
    return @typeInfo(@TypeOf(@"fn")).Fn.return_type.?;
}

pub fn normalizeArgs(args: anytype) blk: {
    const ArgType = @TypeOf(args);
    const type_info = @typeInfo(ArgType);
    break :blk if (type_info == .@"struct" and type_info.@"struct".is_tuple)
        ArgType
    else
        struct { ArgType };
} {
    const type_info = @typeInfo(@TypeOf(args));

    return if (type_info == .@"struct" and type_info.@"struct".is_tuple)
        args
    else
        .{args};
}

pub fn nullableArrayFrom(T: type, values: anytype) T {
    const tuple = normalizeArgs(values);
    var array: T = .{null} ** @typeInfo(T).array.len;

    inline for (meta.fields(@TypeOf(tuple)), 0..) |field, index| {
        array[index] = @field(tuple, field.name);
    }

    return array;
}
