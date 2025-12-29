const std = @import("std");

const MultiArrayList = std.MultiArrayList;
const ArrayListUmanaged = std.ArrayListUnmanaged;
const mem = std.mem;
const Allocator = mem.Allocator;
const meta = std.meta;
const testing = std.testing;
const math = std.math;
const ArrayHashMapUnmanaged = std.ArrayHashMapUnmanaged;
const Wyhash = std.hash.Wyhash;
const assert = std.debug.assert;

pub const InternPool = struct {
    map: ArrayHashMapUnmanaged(
        void,
        void,
        std.array_hash_map.AutoContext(void),
        false,
    ),
    items: MultiArrayList(Item),
    extra: ArrayListUmanaged(u32),
    strings: ArrayListUmanaged(u8),

    pub const Item = struct {
        tag: Tag,
        data: u32,

        pub const Tag = enum(u32) {
            none,

            type_simple,
            type_fn,

            value_simple,
            value_int_small,
            value_int_big,
            value_float_small,
            value_float_big,
            value_string_short,
            value_string_long,

            invalid,
        };
    };

    pub const Key = union(enum) {
        none,

        type_simple: TypeSimple,
        type_fn: TypeFn,

        value_simple: ValueSimple,
        value_int: i64,
        value_float: f64,
        value_string: []const u8,

        invalid,

        pub const TypeSimple = enum(u32) {
            int = @intFromEnum(Index.type_int),
            float = @intFromEnum(Index.type_float),
            bool = @intFromEnum(Index.type_bool),
            string = @intFromEnum(Index.type_string),
            unit = @intFromEnum(Index.type_unit),
            never = @intFromEnum(Index.type_never),
            type = @intFromEnum(Index.type_type),
        };

        pub const TypeFn = struct {
            id: u32,
            locals_count: u32,
            arg_types: []const Index,
            return_type: Index,
        };

        pub const ValueSimple = enum(u32) {
            unit = @intFromEnum(Index.value_unit),
            bool_true = @intFromEnum(Index.value_bool_true),
            bool_false = @intFromEnum(Index.value_bool_false),
        };

        pub const Adapter = struct {
            intern_pool: *const InternPool,

            pub fn hash(_: Adapter, key: Key) u32 {
                const KeyTag = @typeInfo(Key).@"union".tag_type.?;
                const seed = @intFromEnum(@as(KeyTag, key));
                const wyhash = switch (key) {
                    .none => Wyhash.hash(seed, mem.asBytes(&key)),
                    .type_simple => |type_simple| Wyhash.hash(
                        seed,
                        mem.asBytes(&type_simple),
                    ),
                    .type_fn => |type_fn| blk: {
                        const new_seed = Wyhash.hash(
                            seed,
                            mem.asBytes(&type_fn.id) ++
                                mem.asBytes(&type_fn.locals_count) ++
                                mem.asBytes(&type_fn.return_type),
                        );
                        const final = Wyhash.hash(
                            new_seed,
                            @ptrCast(type_fn.arg_types),
                        );

                        break :blk final;
                    },
                    .value_simple => |value_simple| Wyhash.hash(
                        seed,
                        mem.asBytes(&value_simple),
                    ),
                    .value_int => |int| Wyhash.hash(seed, mem.asBytes(&int)),
                    .value_float => |float| Wyhash.hash(
                        seed,
                        mem.asBytes(&float),
                    ),
                    .value_string => |string| Wyhash.hash(seed, string),
                    .invalid => Wyhash.hash(seed, mem.asBytes(&key)),
                };

                return @truncate(wyhash);
            }

            pub fn eql(
                adapter: Adapter,
                a: Key,
                _: void,
                b_index: usize,
            ) bool {
                const b = Index.from(b_index).toKey(adapter.intern_pool);
                const KeyTag = @typeInfo(Key).@"union".tag_type.?;

                const a_tag: KeyTag = a;
                const b_tag: KeyTag = b;

                if (a_tag != b_tag) {
                    return false;
                }

                return switch (a) {
                    .none => true,
                    .type_simple => |type_simple| meta.eql(
                        type_simple,
                        b.type_simple,
                    ),
                    .type_fn => |type_fn| type_fn.id == b.type_fn.id and
                        type_fn.locals_count == b.type_fn.locals_count and
                        std.mem.eql(Index, type_fn.arg_types, b.type_fn.arg_types) and
                        type_fn.return_type == b.type_fn.return_type,
                    .value_simple => |value_simple| meta.eql(
                        value_simple,
                        b.value_simple,
                    ),
                    .value_int => |int| int == b.value_int,
                    .value_float => |float| float == b.value_float,
                    .value_string => |string| std.mem.eql(
                        u8,
                        string,
                        b.value_string,
                    ),
                    .invalid => true,
                };
            }
        };
    };

    pub const Index = enum(u32) {
        none,

        type_int,
        type_float,
        type_bool,
        type_string,
        type_unit,
        type_never,
        type_type,

        value_unit,
        value_bool_true,
        value_bool_false,

        invalid,

        _,

        pub fn from(int: anytype) Index {
            return @enumFromInt(int);
        }

        pub fn toInt(self: Index) u32 {
            return @intFromEnum(self);
        }

        pub fn toKey(self: Index, intern_pool: *const InternPool) Key {
            const item = intern_pool.items.get(self.toInt());
            const data = item.data;

            return switch (item.tag) {
                .none => .none,
                .type_simple => .{ .type_simple = @enumFromInt(data) },
                .type_fn => .{ .type_fn = blk: {
                    const id = intern_pool.extra.items[data];
                    const locals_count = intern_pool.extra.items[data + 1];
                    const arg_count = intern_pool.extra.items[data + 2];
                    const arg_types = intern_pool.extra.items[data + 3 ..][0..arg_count];
                    const return_type = intern_pool.extra.items[data + 3 + arg_count];

                    break :blk .{
                        .id = id,
                        .locals_count = locals_count,
                        .arg_types = @ptrCast(arg_types),
                        .return_type = .from(return_type),
                    };
                } },
                .value_simple => .{ .value_simple = @enumFromInt(data) },
                .value_int_small => .{ .value_int = mem.bytesToValue(
                    i64,
                    &[_]u32{ data, 0 },
                ) },
                .value_int_big => .{ .value_int = mem.bytesToValue(
                    i64,
                    intern_pool.extra.items[data..][0..2],
                ) },
                .value_float_small => .{ .value_float = mem.bytesToValue(
                    f64,
                    &[_]u32{ 0, data },
                ) },
                .value_float_big => .{ .value_float = mem.bytesToValue(
                    f64,
                    intern_pool.extra.items[data..][0..2],
                ) },
                .value_string_short => .{
                    .value_string = blk: {
                        const data_ptr = &intern_pool.items
                            .items(.data)[self.toInt()];
                        const str_ptr: [*]const u8 = @ptrCast(data_ptr);
                        const str_slice = str_ptr[0..4];
                        break :blk mem.sliceTo(str_slice, 0);
                    },
                },
                .value_string_long => blk: {
                    const loc = intern_pool.extra.items[data .. data + 2];
                    break :blk .{ .value_string = intern_pool
                        .strings
                        .items[loc[0]..][0..loc[1]] };
                },
                .invalid => .invalid,
            };
        }

        pub fn toType(self: Index, intern_pool: *const InternPool) Index {
            return switch (self) {
                .none,
                .invalid,
                => self,

                .type_int,
                .type_float,
                .type_bool,
                .type_string,
                .type_unit,
                .type_never,
                .type_type,
                => .type_type,

                .value_unit,
                => .type_unit,

                .value_bool_true,
                .value_bool_false,
                => .type_bool,

                _ => blk: {
                    const tag = intern_pool.items.items(.tag)[self.toInt()];

                    break :blk switch (tag) {
                        .none,
                        .type_simple,
                        .value_simple,
                        .invalid,
                        => unreachable, // already handled above

                        .value_int_small,
                        .value_int_big,
                        => .type_int,

                        .value_float_small,
                        .value_float_big,
                        => .type_float,

                        .value_string_short,
                        .value_string_long,
                        => .type_string,

                        .type_fn,
                        => .type_type,
                    };
                },
            };
        }

        pub fn toKind(self: Index, intern_pool: *const InternPool) enum {
            type,
            value,
            invalid,
        } {
            return switch (self) {
                .none,
                .invalid,
                => .invalid,

                .type_int,
                .type_float,
                .type_bool,
                .type_string,
                .type_unit,
                .type_never,
                .type_type,
                => .type,

                .value_unit,
                .value_bool_true,
                .value_bool_false,
                => .value,

                _ => blk: {
                    const tag = intern_pool.items.items(.tag)[self.toInt()];

                    break :blk switch (tag) {
                        .none,
                        .type_simple,
                        .value_simple,
                        .invalid,
                        => unreachable, // already handled above

                        .value_int_small,
                        .value_int_big,
                        .value_float_small,
                        .value_float_big,
                        .value_string_short,
                        .value_string_long,
                        => .value,

                        .type_fn,
                        => .type,
                    };
                },
            };
        }
    };

    pub fn init(allocator: Allocator) Allocator.Error!InternPool {
        var intern_pool: InternPool = .{
            .map = .empty,
            .items = .empty,
            .extra = .empty,
            .strings = .empty,
        };
        const fields = meta.fields(Index);

        try intern_pool.map.ensureUnusedCapacity(allocator, fields.len);
        try intern_pool.items.ensureUnusedCapacity(allocator, fields.len);
        try intern_pool.extra.ensureUnusedCapacity(allocator, fields.len);

        inline for (fields) |field| {
            _ = try intern_pool.get(
                allocator,
                switch (@field(Index, field.name)) {
                    .none => .none,
                    .type_int => .{ .type_simple = .int },
                    .type_float => .{ .type_simple = .float },
                    .type_bool => .{ .type_simple = .bool },
                    .type_string => .{ .type_simple = .string },
                    .type_unit => .{ .type_simple = .unit },
                    .type_never => .{ .type_simple = .never },
                    .type_type => .{ .type_simple = .type },
                    .value_unit => .{ .value_simple = .unit },
                    .value_bool_true => .{ .value_simple = .bool_true },
                    .value_bool_false => .{ .value_simple = .bool_false },
                    .invalid => .invalid,
                    _ => unreachable,
                },
            );
        }

        assert(fields.len == intern_pool.items.len);

        return intern_pool;
    }

    pub fn deinit(self: *InternPool, allocator: Allocator) void {
        self.map.deinit(allocator);
        self.items.deinit(allocator);
        self.extra.deinit(allocator);
        self.strings.deinit(allocator);
    }

    pub fn get(
        self: *InternPool,
        allocator: Allocator,
        key: Key,
    ) Allocator.Error!Index {
        const adapter: Key.Adapter = .{ .intern_pool = self };
        const result = try self.map.getOrPutAdapted(allocator, key, adapter);

        if (result.found_existing) {
            return Index.from(result.index);
        }

        try self.items.ensureUnusedCapacity(allocator, 1);

        switch (key) {
            .none => {
                self.items.appendAssumeCapacity(.{
                    .tag = .none,
                    .data = undefined,
                });
            },
            .type_simple => |type_simple| {
                self.items.appendAssumeCapacity(.{
                    .tag = .type_simple,
                    .data = @intFromEnum(type_simple),
                });
            },
            .type_fn => |type_fn| {
                try self.extra.ensureUnusedCapacity(
                    allocator,
                    type_fn.arg_types.len + 4,
                );

                const top = self.extra.items.len;

                self.extra.appendSliceAssumeCapacity(&[_]u32{
                    type_fn.id,
                    type_fn.locals_count,
                    @intCast(type_fn.arg_types.len),
                });
                self.extra.appendSliceAssumeCapacity(
                    @ptrCast(type_fn.arg_types),
                );
                self.extra.appendAssumeCapacity(type_fn.return_type.toInt());

                self.items.appendAssumeCapacity(.{
                    .tag = .type_fn,
                    .data = @intCast(top),
                });
            },
            .value_simple => |value_simple| {
                self.items.appendAssumeCapacity(.{
                    .tag = .value_simple,
                    .data = @intFromEnum(value_simple),
                });
            },
            .value_int => |int| {
                const bits: [2]u32 = @bitCast(int);

                if (bits[1] == 0) {
                    self.items.appendAssumeCapacity(.{
                        .tag = .value_int_small,
                        .data = bits[0],
                    });
                } else {
                    try self.extra.appendSlice(allocator, &bits);
                    self.items.appendAssumeCapacity(.{
                        .tag = .value_int_big,
                        .data = @intCast(self.extra.items.len - 2),
                    });
                }
            },
            .value_float => |float| {
                const bits: [2]u32 = @bitCast(float);

                if (bits[0] == 0) {
                    self.items.appendAssumeCapacity(.{
                        .tag = .value_float_small,
                        .data = bits[1],
                    });
                } else {
                    try self.extra.appendSlice(allocator, &bits);
                    self.items.appendAssumeCapacity(.{
                        .tag = .value_float_big,
                        .data = @intCast(self.extra.items.len - 2),
                    });
                }
            },
            .value_string => |string| {
                if (string.len <= 4) {
                    var bytes: [4]u8 = mem.bytesToValue([4]u8, string);

                    if (string.len < 4) {
                        bytes[string.len] = 0;
                    }

                    self.items.appendAssumeCapacity(.{
                        .tag = .value_string_short,
                        .data = @bitCast(bytes),
                    });
                } else {
                    try self.strings.appendSlice(allocator, string);
                    try self.extra.appendSlice(allocator, &[_]u32{
                        @intCast(self.strings.items.len - string.len),
                        @intCast(string.len),
                    });
                    self.items.appendAssumeCapacity(.{
                        .tag = .value_string_long,
                        .data = @intCast(self.extra.items.len - 2),
                    });
                }
            },
            .invalid => {
                self.items.appendAssumeCapacity(.{
                    .tag = .invalid,
                    .data = undefined,
                });
            },
        }

        return Index.from(self.items.len - 1);
    }

    pub fn patchFnLocalsCount(
        self: *InternPool,
        index: Index,
        locals_count: u32,
    ) void {
        const tag = self.items.items(.tag)[index.toInt()];

        assert(tag == .type_fn);

        const data = self.items.items(.data)[index.toInt()];

        self.extra.items[data + 1] = locals_count;
    }
};

test "should intern values" {
    var intern_pool: InternPool = try .init(testing.allocator);
    defer intern_pool.deinit(std.testing.allocator);

    const index1 = try intern_pool.get(
        testing.allocator,
        .{ .type_simple = .type },
    );

    const index2 = try intern_pool.get(
        testing.allocator,
        .{ .type_simple = .type },
    );

    try testing.expectEqual(index1, index2);
}

test "should intern ints" {
    var intern_pool: InternPool = try .init(testing.allocator);
    defer intern_pool.deinit(std.testing.allocator);

    for ([_]i64{ 1032, 9999999999999, -1032, -9999999999999 }) |test_int| {
        const index1 = try intern_pool.get(
            testing.allocator,
            .{ .value_int = test_int },
        );

        try testing.expectEqual(test_int, index1.toKey(&intern_pool).value_int);

        const index2 = try intern_pool.get(
            testing.allocator,
            .{ .value_int = test_int },
        );

        try testing.expectEqual(index1, index2);

        const other_index1 = try intern_pool.get(
            testing.allocator,
            .{ .value_int = test_int + 1 },
        );

        try testing.expect(index1 != other_index1);

        try testing.expectEqual(
            test_int + 1,
            other_index1.toKey(&intern_pool).value_int,
        );

        const other_index2 = try intern_pool.get(
            testing.allocator,
            .{ .value_int = test_int + 1 },
        );

        try testing.expectEqual(other_index1, other_index2);
    }
}

test "should intern floats" {
    var intern_pool: InternPool = try .init(testing.allocator);
    defer intern_pool.deinit(std.testing.allocator);

    for ([_]f64{
        1010.33,
        9999999.000003,
        -1010.33,
        -9999999.000003,
    }) |test_float| {
        const index1 = try intern_pool.get(
            testing.allocator,
            .{ .value_float = test_float },
        );

        try testing.expectEqual(
            test_float,
            index1.toKey(&intern_pool).value_float,
        );

        const index2 = try intern_pool.get(
            testing.allocator,
            .{ .value_float = test_float },
        );

        try testing.expectEqual(index1, index2);

        const other_index1 = try intern_pool.get(
            testing.allocator,
            .{ .value_float = test_float + 0.5 },
        );

        try testing.expect(index1 != other_index1);

        try testing.expectEqual(
            test_float + 0.5,
            other_index1.toKey(&intern_pool).value_float,
        );

        const other_index2 = try intern_pool.get(
            testing.allocator,
            .{ .value_float = test_float + 0.5 },
        );

        try testing.expectEqual(other_index1, other_index2);
    }
}

test "should intern bools" {
    var intern_pool: InternPool = try .init(testing.allocator);
    defer intern_pool.deinit(std.testing.allocator);

    const index1 = try intern_pool.get(testing.allocator, .{
        .value_simple = .bool_true,
    });

    const index2 = try intern_pool.get(testing.allocator, .{
        .value_simple = .bool_true,
    });

    try testing.expectEqual(
        InternPool.Key.ValueSimple.bool_true,
        index1.toKey(&intern_pool).value_simple,
    );
    try testing.expectEqual(index1, index2);

    const index3 = try intern_pool.get(testing.allocator, .{
        .value_simple = .bool_false,
    });
    const index4 = try intern_pool.get(testing.allocator, .{
        .value_simple = .bool_false,
    });

    try testing.expectEqual(
        InternPool.Key.ValueSimple.bool_false,
        index3.toKey(&intern_pool).value_simple,
    );
    try testing.expectEqual(index3, index4);
}

test "should intern strings" {
    var intern_pool: InternPool = try .init(testing.allocator);
    defer intern_pool.deinit(std.testing.allocator);

    for ([_]struct { []const u8, []const u8 }{
        .{ "", "test" },
        .{ "h", "htest" },
        .{ "he", "hetest" },
        .{ "hel", "heltest" },
        .{ "hell", "helltest" },
        .{ "hello", "hellotest" },
    }) |tuple| {
        const test_string1, const test_string2 = tuple;
        const index1 = try intern_pool.get(
            testing.allocator,
            .{ .value_string = test_string1 },
        );

        try testing.expect(
            mem.eql(u8, test_string1, index1.toKey(&intern_pool).value_string),
        );

        const index2 = try intern_pool.get(
            testing.allocator,
            .{ .value_string = test_string1 },
        );

        try testing.expectEqual(index1, index2);

        const other_index1 = try intern_pool.get(
            testing.allocator,
            .{ .value_string = test_string2 },
        );

        try testing.expect(index1 != other_index1);

        try testing.expect(mem.eql(
            u8,
            test_string2,
            other_index1.toKey(&intern_pool).value_string,
        ));

        const other_index2 = try intern_pool.get(
            testing.allocator,
            .{ .value_string = test_string2 },
        );

        try testing.expectEqual(other_index1, other_index2);
    }
}

test "should intern invalid" {
    var intern_pool: InternPool = try .init(testing.allocator);
    defer intern_pool.deinit(std.testing.allocator);

    const index1 = try intern_pool.get(testing.allocator, .invalid);

    try testing.expectEqual(InternPool.Key.invalid, index1.toKey(&intern_pool));

    const index2 = try intern_pool.get(testing.allocator, .invalid);

    try testing.expectEqual(InternPool.Key.invalid, index2.toKey(&intern_pool));
    try testing.expectEqual(index1, index2);
}

test "should intern type_fn" {
    var intern_pool: InternPool = try .init(testing.allocator);
    defer intern_pool.deinit(std.testing.allocator);

    const fn_key: InternPool.Key = .{ .type_fn = .{
        .id = 39,
        .locals_count = 10,
        .arg_types = &[_]InternPool.Index{ .type_int, .type_int },
        .return_type = .type_int,
    } };

    const index1 = try intern_pool.get(testing.allocator, fn_key);
    const key1 = index1.toKey(&intern_pool);

    try testing.expectEqual(39, key1.type_fn.id);
    try testing.expectEqual(2, key1.type_fn.arg_types.len);
    try testing.expectEqual(.type_int, key1.type_fn.arg_types[0]);
    try testing.expectEqual(.type_int, key1.type_fn.arg_types[1]);
    try testing.expectEqual(.type_int, key1.type_fn.return_type);
    try testing.expectEqual(10, key1.type_fn.locals_count);

    const index2 = try intern_pool.get(testing.allocator, fn_key);
    const key2 = index1.toKey(&intern_pool);

    try testing.expectEqual(39, key2.type_fn.id);
    try testing.expectEqual(2, key2.type_fn.arg_types.len);
    try testing.expectEqual(.type_int, key2.type_fn.arg_types[0]);
    try testing.expectEqual(.type_int, key2.type_fn.arg_types[1]);
    try testing.expectEqual(.type_int, key2.type_fn.return_type);
    try testing.expectEqual(10, key2.type_fn.locals_count);

    try testing.expectEqual(index1, index2);
}
