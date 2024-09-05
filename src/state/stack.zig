const std = @import("std");
const value_mod = @import("value.zig");
const limits = @import("../limits.zig");

const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Value = value_mod.Value;

pub const Stack = struct {
    const Self = @This();

    const Error = error{OutOfMemory};

    allocator: Allocator,
    items: []Value,
    top: [*]Value,

    pub fn init(allocator: Allocator) Error!Self {
        const items = try allocator.alloc(Value, limits.max_stack);

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
        assert(self.top != @as([*]Value, @ptrCast(&self.items[0])));

        self.top -= 1;
        return self.top[0];
    }

    pub fn peek(self: *const Self, distance: usize) Value {
        return (self.top - 1 - distance)[0];
    }
};
