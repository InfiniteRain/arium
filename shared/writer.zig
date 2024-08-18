const std = @import("std");

const AnyWriter = std.io.AnyWriter;

pub const Writer = struct {
    const Self = @This();

    backing_writer: *const AnyWriter,

    pub fn init(writer: *const AnyWriter) Self {
        return .{ .backing_writer = writer };
    }

    pub fn print(self: *const Self, text: []const u8) void {
        self.printf("{s}", .{text});
    }

    pub fn printf(
        self: *const Self,
        comptime format: []const u8,
        args: anytype,
    ) void {
        self.backing_writer.print(format, args) catch {
            @panic("unable to print to writer");
        };
    }
};
