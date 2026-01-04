const std = @import("std");
const Writer = std.io.Writer;

pub const Output = struct {
    writer: *Writer,

    pub fn init(writer: *Writer) Output {
        return .{
            .writer = writer,
        };
    }

    pub fn print(self: *const Output, text: []const u8) void {
        self.printf("{s}", .{text});
    }

    pub fn printf(
        self: *const Output,
        comptime format: []const u8,
        args: anytype,
    ) void {
        self.writer.print(format, args) catch {
            @panic("Unable to print to output.");
        };
        self.writer.flush() catch {
            @panic("Unable to flush output.");
        };
    }
};
