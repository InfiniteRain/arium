const std = @import("std");
const value_mod = @import("value.zig");

const Allocator = std.mem.Allocator;
const Value = value_mod.Value;

pub const Stack = struct {
    const Self = @This();
    const max_frames = 64;
    const max_stack = max_frames * 256;

    allocator: Allocator,
    items: []Value,
    top: [*]Value,

    pub fn init(allocator: Allocator) !Self {
        const items = try allocator.alloc(Value, max_stack);

        return .{
            .allocator = allocator,
            .items = items,
            .top = @ptrCast(&items[0]),
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.items);
    }

    pub fn push(self: *Self, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.top -= 1;
        return self.top[0];
    }

    pub fn peek(self: *const Self, distance: usize) Value {
        return (self.top - 1 - distance)[0];
    }
};
