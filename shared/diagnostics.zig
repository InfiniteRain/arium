const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn Diagnostics(T: type) type {
    return struct {
        const Self = @This();

        const Error = error{OutOfMemory};

        allocator: Allocator,
        entries: ArrayList(T),

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .entries = ArrayList(T).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            for (self.entries.items) |*entry| {
                T.deinit(entry, self.allocator);
            }

            self.entries.clearAndFree();
        }

        pub fn getLen(self: *const Self) usize {
            return self.entries.items.len;
        }

        pub fn getEntries(self: *const Self) []T {
            return self.entries.items;
        }

        pub fn add(self: *Self, entry: T) Error!void {
            try self.entries.append(entry);
        }

        pub fn clone(self: *const Self) Error!Self {
            return .{
                .allocator = self.allocator,
                .entries = try self.entries.clone(),
            };
        }
    };
}
