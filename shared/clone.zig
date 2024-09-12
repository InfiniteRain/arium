const std = @import("std");
const meta = @import("meta.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const BuiltinType = std.builtin.Type;
const comptimePrint = std.fmt.comptimePrint;

pub fn createClone(
    allocator: Allocator,
    value: anytype,
    allowlist: anytype,
) Allocator.Error!@TypeOf(value) {
    const Type = @TypeOf(value);
    const type_info = @typeInfo(Type);
    const type_name = @typeName(Type);

    if (type_info == .Pointer and type_info.Pointer.size == .Slice) {
        return try cloneSlice(allocator, value, allowlist);
    }

    if (comptime meta.isArrayList(Type)) {
        return try cloneArrayList(allocator, value, allowlist);
    }

    return switch (type_info) {
        .Int,
        .Float,
        .Bool,
        .Enum,
        .Void,
        => value,

        .Union,
        => if (comptime meta.typeInTuple(Type, allowlist))
            try cloneUnion(allocator, value, allowlist)
        else
            @compileError(comptimePrint(
                "union of type {s} is not in the allowlist",
                .{type_name},
            )),

        .Struct,
        => if (comptime meta.typeInTuple(Type, allowlist))
            try cloneStruct(allocator, value, allowlist)
        else
            @compileError(comptimePrint(
                "struct of type {s} is not in the allowlist",
                .{type_name},
            )),

        else => @compileError(comptimePrint(
            "clone wasn't implemented for {s} / {s}",
            .{ type_name, @tagName(type_info) },
        )),
    };
}

pub fn cloneSlice(
    allocator: Allocator,
    value: anytype,
    allowlist: anytype,
) Allocator.Error!@TypeOf(value) {
    const type_info = @typeInfo(@TypeOf(value));
    const new_buffer = try allocator.alloc(type_info.Pointer.child, value.len);

    for (value, new_buffer) |original, *cloned| {
        cloned.* = try createClone(allocator, original, allowlist);
    }

    return new_buffer;
}

pub fn cloneArrayList(
    allocator: Allocator,
    value: anytype,
    allowlist: anytype,
) Allocator.Error!@TypeOf(value) {
    return ArrayList(@typeInfo(@TypeOf(value.items)).Pointer.child)
        .fromOwnedSlice(
        allocator,
        try createClone(
            allocator,
            value.items,
            allowlist,
        ),
    );
}

pub fn cloneUnion(
    allocator: Allocator,
    value: anytype,
    allowlist: anytype,
) Allocator.Error!@TypeOf(value) {
    const Type = @TypeOf(value);
    const type_info = @typeInfo(Type);

    inline for (type_info.Union.fields) |field| {
        if (std.mem.eql(u8, field.name, @tagName(value))) {
            return @unionInit(Type, field.name, try createClone(
                allocator,
                @field(value, field.name),
                allowlist,
            ));
        }
    }

    unreachable;
}

pub fn cloneStruct(
    allocator: Allocator,
    value: anytype,
    allowlist: anytype,
) Allocator.Error!@TypeOf(value) {
    const Type = @TypeOf(value);
    const type_info = @typeInfo(Type);

    var clone: Type = undefined;

    inline for (type_info.Struct.fields) |field| {
        @field(clone, field.name) = try createClone(
            allocator,
            @field(value, field.name),
            allowlist,
        );
    }

    return clone;
}
