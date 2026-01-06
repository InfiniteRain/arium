pub fn Span(T: type) type {
    return struct {
        index: u32,
        len: u32,

        const Self = @This();

        pub const zero: Self = .{
            .index = 0,
            .len = 0,
        };

        pub fn init(start_index: anytype, end_index: anytype) Self {
            return .{
                .index = @intCast(start_index),
                .len = @intCast(end_index - start_index),
            };
        }

        pub fn toSlice(self: Self, source: []const T) []const T {
            return source[self.index..][0..self.len];
        }

        pub fn extend(a: Self, b: Self) Self {
            return .{
                .index = a.index,
                .len = (b.index - a.index) + b.len,
            };
        }

        pub fn toLineCol(
            span: Span(u8),
            source: []const u8,
        ) struct { u32, u32 } {
            var line: u32 = 1;
            var column: u32 = 1;

            for (source, 0..) |char, index| {
                if (index == span.index) {
                    return .{ line, column };
                }

                if (char == '\n') {
                    line += 1;
                    column = 1;
                } else {
                    column += 1;
                }
            }

            return .{ line, column };
        }
    };
}
