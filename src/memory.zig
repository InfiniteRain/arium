const std = @import("std");
const debug = std.debug;
const assert = debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMapUnmanaged = std.StringHashMapUnmanaged;

const ExecutionMode = @import("debug.zig").ExecutionMode;

pub fn Value(comptime mode: ExecutionMode) type {
    return switch (mode) {
        .debug => union(enum) {
            int: i64,
            float: f64,
            bool: bool,
            @"fn": u64,
            object: *Object(.debug),
        },
        .release => packed union {
            int: i64,
            float: f64,
            bool: bool,
            @"fn": u64,
            object: *Object(.release),

            comptime {
                assert(@sizeOf(@This()) == 8);
            }
        },
    };
}

pub fn Object(comptime mode: ExecutionMode) type {
    return struct {
        tag: Tag,
        next: ?*Self,

        const Self = @This();

        const Tag = enum {
            string,
        };

        pub const String = struct {
            object: Self,
            chars: []u8,

            pub fn create(
                memory: *Memory(mode),
                buf: []const u8,
            ) Allocator.Error!*String {
                if (memory.string_pool.get(buf)) |string| {
                    return string;
                }

                const allocator = memory.allocator();

                const prev_gc_lock_status = memory.gc_lock_status;
                defer memory.gc_lock_status = prev_gc_lock_status;

                memory.gc_lock_status = .locked;

                var object = try Self.create(String, memory);
                var string = object.as(String);

                string.chars = try allocator.alloc(u8, buf.len);
                @memcpy(string.chars, buf);

                _ = try memory.string_pool.put(allocator, buf, string);

                return string;
            }

            pub fn createTakeOwnership(
                memory: *Memory(mode),
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

                var object = try Self.create(String, memory);
                var string = object.as(String);

                string.chars = buf;

                _ = try memory.string_pool.put(allocator, buf, string);

                return string;
            }
        };

        pub fn as(self: *Self, T: type) *T {
            return @fieldParentPtr("object", self);
        }

        pub fn destroy(self: *Self, memory: *Memory(mode)) void {
            const allocator = memory.allocator();

            switch (self.tag) {
                .string => {
                    const string = self.as(String);
                    allocator.free(string.chars);
                    allocator.destroy(string);
                },
            }
        }

        fn create(T: type, memory: *Memory(mode)) Allocator.Error!*Self {
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
}

pub fn Memory(comptime mode: ExecutionMode) type {
    return struct {
        backing_allocator: Allocator,
        bytes_allocated: usize,
        head: ?*Object(mode),
        string_pool: StringHashMapUnmanaged(*Object(mode).String),
        gc_lock_status: enum { locked, unlocked },

        const Self = @This();

        pub fn init(backing_allocator: Allocator) Self {
            return .{
                .backing_allocator = backing_allocator,
                .bytes_allocated = 0,
                .head = null,
                .string_pool = .empty,
                .gc_lock_status = .unlocked,
            };
        }

        pub fn deinit(self: *Self) void {
            var current = self.head;

            while (current) |object| {
                const next = object.next;
                object.destroy(self);
                current = next;
            }

            self.string_pool.deinit(self.backing_allocator);
        }

        pub fn allocator(self: *Self) Allocator {
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
            const self: *Self = @ptrCast(@alignCast(ctx));
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
            const self: *Self = @ptrCast(@alignCast(ctx));
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
            const self: *Self = @ptrCast(@alignCast(ctx));
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
            const self: *Self = @ptrCast(@alignCast(ctx));

            self.backing_allocator.rawFree(buf, buf_align, ret_addr);
            self.bytes_allocated -= buf.len;
        }
    };
}
