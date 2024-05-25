const std = @import("std");
const object_mod = @import("object.zig");
const value_mod = @import("value.zig");
const managed_memory_mod = @import("managed_memory.zig");
const test_util_mod = @import("test_util.zig");

const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const Object = object_mod.Object;
const Value = value_mod.Value;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const TestUtil = test_util_mod.TestUtil;

const HashTableError = error{
    OutOfMemory,
};

pub const Entry = struct {
    key: ?*Object.String,
    value: Value,
};

pub const HashTable = struct {
    const Self = @This();
    const max_load = 0.75;

    allocator: Allocator,
    count: usize = 0,
    entries: []Entry,

    pub fn init(allocator: Allocator) HashTableError!Self {
        return .{
            .allocator = allocator,
            .entries = try allocator.alloc(Entry, 0),
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.entries);
    }

    pub fn set(self: *Self, key: *Object.String, value: Value) HashTableError!bool {
        const flen: f64 = @floatFromInt(self.entries.len);
        const threshold: usize = @intFromFloat(flen * max_load);

        if (self.count + 1 > threshold) {
            try self.growCapacity();
        }

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;

        if (is_new_key and entry.value == .unit) {
            self.count += 1;
        }

        entry.key = key;
        entry.value = value;

        return is_new_key;
    }

    pub fn get(self: *Self, key: *Object.String) ?Value {
        if (self.entries.len == 0) {
            return null;
        }

        const entry = findEntry(self.entries, key);

        if (entry.key == null) {
            return null;
        }

        return entry.value;
    }

    pub fn delete(self: *Self, key: *Object.String) bool {
        if (self.entries.len == 0) {
            return false;
        }

        var entry = findEntry(self.entries, key);

        if (entry.key == null) {
            return false;
        }

        entry.key = null;
        entry.value = .{ .bool = true };

        return true;
    }

    pub fn findString(self: *Self, buf: []const u8, hash: u32) ?*Object.String {
        if (self.count == 0) {
            return null;
        }

        var index = hash & (self.entries.len - 1);

        while (true) : (index = (index + 1) % self.entries.len) {
            const entry = &self.entries[index];

            if (entry.key) |key| {
                if (key.chars.len == buf.len and
                    key.hash == hash and
                    std.mem.eql(u8, key.chars, buf))
                {
                    return key;
                }
            } else if (entry.value == .unit) {
                return null;
            }
        }
    }

    fn growCapacity(self: *Self) HashTableError!void {
        const capacity = if (self.entries.len <= 0) 8 else self.entries.len * 2;
        const new_entries = try self.allocator.alloc(Entry, capacity);

        for (new_entries) |*new_entry| {
            new_entry.key = null;
            new_entry.value = .unit;
        }

        self.count = 0;

        for (self.entries) |*entry| {
            if (entry.key) |key| {
                var dest = findEntry(new_entries, key);

                dest.key = entry.key;
                dest.value = entry.value;

                self.count += 1;
            }
        }

        self.allocator.free(self.entries);
        self.entries = new_entries;
    }

    fn findEntry(entries: []Entry, key: *Object.String) *Entry {
        var index = key.hash & (entries.len - 1);
        var tombstone_opt: ?*Entry = null;

        while (true) : (index = (index + 1) & (entries.len - 1)) {
            const entry = &entries[index];

            if (entry.key == key) {
                return entry;
            }

            if (entry.key != null) {
                continue;
            }

            if (entry.value == .unit) {
                // empty entry
                return if (tombstone_opt) |tombstone| tombstone else entry;
            }

            // found a tombstone
            if (tombstone_opt == null) {
                tombstone_opt = entry;
            }
        }
    }
};

test "hashing function overflows properly" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    // WHEN
    const result = t.createString("some very long string that will overflow hash");

    // THEN
    _ = try result;
}

test "table starts at 0 capacity" {
    // GIVEN - WHEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    // THEN
    try expect(t.table.entries.len == 0);
}

test "adding entries resizes the capacity" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    // WHEN
    for (0..6) |i| {
        var buf: [16]u8 = undefined;
        const str = try std.fmt.bufPrint(&buf, "str {}", .{i});
        const str_obj = try t.createString(str);

        _ = try t.table.set(str_obj, .unit);
    }

    // THEN
    try expect(t.table.entries.len == 8);
}

test "adding entries beyond 75% of capacity two times resizes the capacity" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    // WHEN
    for (0..7) |i| {
        var buf: [16]u8 = undefined;
        const str = try std.fmt.bufPrint(&buf, "str {}", .{i});
        const str_obj = try t.createString(str);

        _ = try t.table.set(str_obj, .unit);
    }

    // THEN
    try expect(t.table.entries.len == 16);
}

test "adding entries beyond 75% of capacity three times resizes the capacity" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    // WHEN
    for (0..13) |i| {
        var buf: [16]u8 = undefined;
        const str = try std.fmt.bufPrint(&buf, "str {}", .{i});
        const str_obj = try t.createString(str);

        _ = try t.table.set(str_obj, .unit);
    }

    // THEN
    try expect(t.table.entries.len == 32);
}

test "set should return true when new key inserted" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str = try t.createString("key");

    // WHEN
    const result = try t.table.set(str, .unit);

    // THEN
    try expect(result);
}

test "set should return false when key value overriden" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str = try t.createString("key");
    _ = try t.table.set(str, .unit);

    // WHEN
    const result = try t.table.set(str, .{ .float = 10.0 });

    // THEN
    try expect(!result);
}

test "set should properly handle collisions" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    var t2 = try TestUtil.init(std.testing.allocator);
    defer t2.deinit();

    // hashes of both AAAA and AAAI % 8 are 1
    const s_key1 = try t.createString("AAAA");
    const s_key2 = try t.createString("AAAI");

    const s2_key1 = try t2.createString("AAAA");
    const s2_key2 = try t2.createString("AAAI");

    // WHEN
    _ = try t.table.set(s_key1, .{ .bool = true });
    _ = try t.table.set(s_key2, .{ .bool = false });

    _ = try t2.table.set(s2_key2, .{ .bool = false });
    _ = try t2.table.set(s2_key1, .{ .bool = true });

    // THEN
    try expect(t.table.entries[1].value.bool);
    try expect(&t.table.entries[1].key.?.object == &s_key1.object);

    try expect(!t.table.entries[2].value.bool);
    try expect(&t.table.entries[2].key.?.object == &s_key2.object);

    try expect(!t2.table.entries[1].value.bool);
    try expect(&t2.table.entries[1].key.?.object == &s2_key2.object);

    try expect(t2.table.entries[2].value.bool);
    try expect(&t2.table.entries[2].key.?.object == &s2_key1.object);
}

test "get should return true on existent items" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str = try t.createString("key");
    _ = try t.table.set(str, .unit);

    // WHEN
    const value = t.table.get(str);

    // THEN
    try expect(value.? == .unit);
}

test "get should return false on non-existent item" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str = try t.createString("non-existent key");

    // WHEN
    const value = t.table.get(str);

    // THEN
    try expect(value == null);
}

test "get should properly handle collisions" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    var t2 = try TestUtil.init(std.testing.allocator);
    defer t2.deinit();

    // hashes of both AAAA and AAAI % 8 are 1
    const s_key1 = try t.createString("AAAA");
    const s_key2 = try t.createString("AAAI");

    const s2_key1 = try t2.createString("AAAA");
    const s2_key2 = try t2.createString("AAAI");

    _ = try t.table.set(s_key1, .{ .bool = true });
    _ = try t.table.set(s_key2, .{ .bool = false });

    _ = try t2.table.set(s2_key2, .{ .bool = false });
    _ = try t2.table.set(s2_key1, .{ .bool = true });

    // WHEN
    const s_val1 = t.table.get(s_key1);
    const s_val2 = t.table.get(s_key2);

    const s2_val1 = t2.table.get(s2_key1);
    const s2_val2 = t2.table.get(s2_key2);

    // THEN
    try expect(s_val1.?.bool);
    try expect(!s_val2.?.bool);

    try expect(s2_val1.?.bool);
    try expect(!s2_val2.?.bool);
}

test "get should resolve with correct values" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str1 = try t.createString("key1");
    const str2 = try t.createString("key2");
    const str3 = try t.createString("key3");
    const str4 = try t.createString("key4");

    _ = try t.table.set(str1, .unit);
    _ = try t.table.set(str2, .{ .int = 10 });
    _ = try t.table.set(str3, .{ .bool = false });
    _ = try t.table.set(str4, .{ .object = &(try t.createString("some value")).object });
    _ = try t.table.set(str2, .{ .int = 20 });

    // WHEN
    const val1 = t.table.get(str1);
    const val2 = t.table.get(str2);
    const val3 = t.table.get(str3);
    const val4 = t.table.get(str4);

    // THEN
    try expect(val1.? == .unit);
    try expect(val2.?.int == 20);
    try expect(!val3.?.bool);
    try expect(val4.?.object == &(try t.createString("some value")).object);
}

test "delete returns true on successful deletion" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str = try t.createString("key1");

    _ = try t.table.set(str, .unit);

    // WHEN
    const result = t.table.delete(str);

    // THEN
    try expect(result);
}

test "delete returns false on unsuccessful deletion" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str = try t.createString("key1");

    // WHEN
    const result = t.table.delete(str);

    // THEN
    try expect(!result);
}

test "delete should properly delete values" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str1 = try t.createString("key1");
    const str2 = try t.createString("key2");

    _ = try t.table.set(str1, .unit);
    _ = try t.table.set(str2, .{ .bool = true });

    const delete_result = t.table.delete(str1);
    _ = delete_result;

    // WHEN
    const val1 = t.table.get(str1);
    const val2 = t.table.get(str2);

    // THEN
    try expect(val1 == null);
    try expect(val2.?.bool);
}

test "delete should place a tombstone" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    // key AAAA will put the entry into index 1
    const str1 = try t.createString("AAAA");

    _ = try t.table.set(str1, .unit);

    // WHEN
    _ = t.table.delete(str1);

    // THEN
    try expect(t.table.entries[1].key == null);
    try expect(t.table.entries[1].value.bool == true);
}

test "tombstone should prevent collided items from becoming inaccessible" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    // hashes of both AAAA and AAAI % 8 are 1
    const key1 = try t.createString("AAAA");
    const key2 = try t.createString("AAAI");

    _ = try t.table.set(key1, .unit);
    _ = try t.table.set(key2, .{ .bool = true });

    // WHEN
    _ = t.table.delete(key1);

    const value = t.table.get(key2);

    // THEN
    try expect(value.?.bool);
}

test "tombstones should not be copied over to the resized buffer" {
    // GIVEN
    var t = try TestUtil.init(std.testing.allocator);
    defer t.deinit();

    const str1 = try t.createString("key1");

    _ = try t.table.set(str1, .unit);
    _ = t.table.delete(str1);

    // WHEN
    for (0..7) |i| {
        var buf: [16]u8 = undefined;
        const str = try std.fmt.bufPrint(&buf, "str {}", .{i});
        const str_obj = try t.createString(str);

        _ = try t.table.set(str_obj, .unit);
    }

    // THEN
    for (t.table.entries) |entry| {
        try expect(entry.key != null or entry.value != .bool or !entry.value.bool);
    }
}
