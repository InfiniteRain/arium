const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const TestWriter = struct {
    const Self = @This();

    pub const WriteError = error{OutOfMemory};

    pub const Writer = std.io.Writer(*Self, WriteError, write);

    output: ArrayList(u8),

    pub fn init(allocator: Allocator) Self {
        return .{
            .output = ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.clearAndFree();
    }

    pub fn writer(self: *Self) Writer {
        return .{ .context = self };
    }

    pub fn write(self: *Self, bytes: []const u8) WriteError!usize {
        try self.output.appendSlice(bytes);
        return bytes.len;
    }
};
