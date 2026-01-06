const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

pub const TestWriter = struct {
    allocator: Allocator,
    output: *ArrayList(u8),
    interface: Writer,

    pub fn init(allocator: Allocator, output: *ArrayList(u8)) TestWriter {
        return .{
            .allocator = allocator,
            .output = output,
            .interface = .{
                .vtable = &.{
                    .drain = drain,
                },
                .buffer = &.{},
            },
        };
    }

    pub fn drain(
        io_w: *std.Io.Writer,
        data: []const []const u8,
        splat: usize,
    ) Writer.Error!usize {
        _ = splat;

        const self: *TestWriter =
            @alignCast(@fieldParentPtr("interface", io_w));

        self.output.appendSlice(self.allocator, data[0]) catch
            return error.WriteFailed;

        return data[0].len;
    }
};
