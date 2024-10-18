const std = @import("std");
const value_mod = @import("value.zig");
const limits = @import("../limits.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const Value = value_mod.Value;

pub const Stack = struct {
    const Self = @This();

    const Error = error{
        StackOverflow,
    } || Allocator.Error;

    allocator: Allocator,
    values: ArrayList(Value),
    top: u32,

    pub fn init(allocator: Allocator, capacity: u32) Allocator.Error!Self {
        var values = ArrayList(Value).init(allocator);
        try values.resize(capacity);

        return .{
            .allocator = allocator,
            .values = values,
            .top = capacity,
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.clearAndFree();
    }

    pub fn push(self: *Self, value: Value) Error!void {
        if (self.top == self.values.items.len) {
            if (self.top + 1 > limits.max_stack) {
                return error.StackOverflow;
            }

            try self.values.append(value);
        } else {
            self.values.items[self.top] = value;
        }

        self.top += 1;
    }

    pub fn pop(self: *Self) Value {
        assert(self.top != 0);

        self.top -= 1;
        return self.values.items[self.top];
    }

    pub fn peek(self: *const Self, distance: u32) Value {
        return self.values.items[self.top - 1 - distance];
    }

    pub fn increaseTop(self: *Self, distance: u32) Error!void {
        const new_capacity = self.top + distance;

        if (new_capacity > limits.max_stack) {
            return error.StackOverflow;
        }

        if (new_capacity > self.values.items.len) {
            try self.values.resize(new_capacity);
        }

        self.top += distance;
    }

    pub fn decreaseTop(self: *Self, distance: u32) void {
        self.top -= distance;
    }

    pub fn getAt(self: *Self, index: u32) Value {
        return self.values.items[index];
    }

    pub fn setAt(self: *Self, index: u32, value: Value) void {
        self.values.items[index] = value;
    }
};
