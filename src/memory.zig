const std = @import("std");
const debug = std.debug;
const assert = debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMapUnmanaged = std.StringHashMapUnmanaged;

pub const Value = packed union {
    int: i64,
    float: f64,
    bool: bool,
    @"fn": u64,
    object: *Object,

    comptime {
        assert(@sizeOf(Value) == 8);
    }

    pub const DebugTag = enum {
        int,
        float,
        bool,
        @"fn",
        object,
    };
};

pub const TaggedValue = union(enum) {
    int: i64,
    float: f64,
    bool: bool,
    @"fn": u64,
    object: *Object,

    pub fn from(value: Value, tag: Value.DebugTag) TaggedValue {
        return switch (tag) {
            .int => .{ .int = value.int },
            .float => .{ .float = value.float },
            .bool => .{ .bool = value.bool },
            .@"fn" => .{ .@"fn" = value.@"fn" },
            .object => .{ .object = value.object },
        };
    }

    pub fn separate(self: TaggedValue) struct { Value, Value.DebugTag } {
        return switch (self) {
            .int => |int| .{ .{ .int = int }, .int },
            .float => |float| .{ .{ .float = float }, .float },
            .bool => |@"bool"| .{ .{ .bool = @"bool" }, .bool },
            .@"fn" => |@"fn"| .{ .{ .@"fn" = @"fn" }, .@"fn" },
            .object => |object| .{ .{ .object = object }, .object },
        };
    }
};

pub const Object = struct {
    tag: Tag,
    next: ?*Object,

    const Tag = enum {
        string,
    };

    pub const String = struct {
        object: Object,
        chars: []u8,

        pub fn create(
            memory: *Memory,
            buf: []const u8,
        ) Allocator.Error!*String {
            if (memory.string_pool.get(buf)) |string| {
                return string;
            }

            const allocator = memory.allocator();

            const prev_gc_lock_status = memory.gc_lock_status;
            defer memory.gc_lock_status = prev_gc_lock_status;

            memory.gc_lock_status = .locked;

            var object = try Object.create(String, memory);
            var string = object.as(String);

            string.chars = try allocator.alloc(u8, buf.len);
            @memcpy(string.chars, buf);

            _ = try memory.string_pool.put(allocator, buf, string);

            return string;
        }

        pub fn createTakeOwnership(
            memory: *Memory,
            buf: []u8,
        ) Allocator.Error!*String {
            const allocator = memory.allocator();

            if (memory.string_pool.get(buf)) |string| {
                allocator.free(buf);
                return string;
            }

            const prev_gc_lock_status = memory.gc_lock_status;
            defer memory.gc_lock_status = prev_gc_lock_status;

            memory.gc_lock_status = .locked;

            var object = try Object.create(String, memory);
            var string = object.as(String);

            string.chars = buf;

            _ = try memory.string_pool.put(allocator, buf, string);

            return string;
        }
    };

    pub fn as(self: *Object, T: type) *T {
        return @fieldParentPtr("object", self);
    }

    pub fn destroy(self: *Object, memory: *Memory) void {
        const allocator = memory.allocator();

        switch (self.tag) {
            .string => {
                const string = self.as(String);
                allocator.free(string.chars);
                allocator.destroy(string);
            },
        }
    }

    fn create(T: type, memory: *Memory) Allocator.Error!*Object {
        const object = try memory.allocator().create(T);

        object.object = .{
            .tag = typeToTag(T),
            .next = memory.head,
        };

        memory.head = &object.object;

        return &object.object;
    }

    fn typeToTag(T: type) Tag {
        return switch (T) {
            String => .string,
            else => @compileError("type is not a valid object"),
        };
    }
};

pub const Memory = struct {
    backing_allocator: Allocator,
    bytes_allocated: usize,
    head: ?*Object,
    string_pool: StringHashMapUnmanaged(*Object.String),
    gc_lock_status: enum { locked, unlocked },

    pub fn init(backing_allocator: Allocator) Memory {
        return .{
            .backing_allocator = backing_allocator,
            .bytes_allocated = 0,
            .head = null,
            .string_pool = .empty,
            .gc_lock_status = .unlocked,
        };
    }

    pub fn deinit(self: *Memory) void {
        var current = self.head;

        while (current) |object| {
            const next = object.next;
            object.destroy(self);
            current = next;
        }
    }

    pub fn allocator(self: *Memory) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
                .remap = remap,
            },
        };
    }

    fn alloc(
        ctx: *anyopaque,
        len: usize,
        ptr_align: std.mem.Alignment,
        ret_addr: usize,
    ) ?[*]u8 {
        const self: *Memory = @ptrCast(@alignCast(ctx));
        const out_opt = self.backing_allocator.rawAlloc(
            len,
            ptr_align,
            ret_addr,
        );

        if (out_opt != null) {
            self.bytes_allocated += len;
        }

        return out_opt;
    }

    fn resize(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) bool {
        const self: *Memory = @ptrCast(@alignCast(ctx));
        const is_same_address = self.backing_allocator.rawResize(
            buf,
            buf_align,
            new_len,
            ret_addr,
        );

        if (is_same_address) {
            if (new_len > buf.len) {
                self.bytes_allocated += new_len - buf.len;
            } else {
                self.bytes_allocated -= buf.len - new_len;
            }
        }

        return is_same_address;
    }

    fn remap(
        ctx: *anyopaque,
        memory: []u8,
        alignment: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) ?[*]u8 {
        const self: *Memory = @ptrCast(@alignCast(ctx));
        const result = self.backing_allocator.rawRemap(
            memory,
            alignment,
            new_len,
            ret_addr,
        );

        if (result != null) {
            if (new_len > memory.len) {
                self.bytes_allocated += new_len - memory.len;
            } else {
                self.bytes_allocated -= memory.len - new_len;
            }
        }

        return result;
    }

    fn free(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: std.mem.Alignment,
        ret_addr: usize,
    ) void {
        const self: *Memory = @ptrCast(@alignCast(ctx));

        self.backing_allocator.rawFree(buf, buf_align, ret_addr);
        self.bytes_allocated -= buf.len;
    }
};
