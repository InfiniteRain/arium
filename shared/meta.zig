const std = @import("std");

const ArrayList = std.ArrayList;

pub fn spread(a: anytype, b: anytype) @TypeOf(a) {
    var result = a;

    inline for (@typeInfo(@TypeOf(b)).Struct.fields) |field| {
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

    if (type_info != .Struct or
        !@hasDecl(T, "Slice") or
        @typeInfo(T.Slice) != .Pointer)
    {
        return false;
    }

    return T == ArrayList(@typeInfo(T.Slice).Pointer.child);
}

pub fn typeInTuple(T: type, tuple: anytype) bool {
    const TupleType = @TypeOf(tuple);
    const tuple_type_info = @typeInfo(TupleType);

    if (tuple_type_info != .Struct or !tuple_type_info.Struct.is_tuple) {
        @compileError("should be a tuple");
    }

    inline for (tuple_type_info.Struct.fields) |field| {
        if (field.type != type) {
            @compileError("tuple should consist of types");
        }

        if (T == @field(tuple, field.name)) {
            return true;
        }
    }

    return false;
}

pub fn ReturnType(@"fn": anytype) type {
    return @typeInfo(@TypeOf(@"fn")).Fn.return_type.?;
}
