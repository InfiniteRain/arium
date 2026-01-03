pub fn FixedArray(T: type, comptime capacity: usize) type {
    return struct {
        buffer: [capacity]T,
        len: usize,

        const Self = @This();

        pub fn from(value: anytype) Self {
            const Type = @TypeOf(value);
            const type_info = @typeInfo(Type);
            const is_tuple = type_info == .@"struct" and
                type_info.@"struct".is_tuple;

            const new_capacity = if (is_tuple)
                type_info.@"struct".fields.len
            else
                1;

            if (new_capacity > capacity) {
                @compileError("not enough capacity to initialize fixed array");
            }

            var array: Self = .{
                .buffer = undefined,
                .len = 0,
            };
            array.len = new_capacity;

            if (is_tuple) {
                inline for (value, 0..) |item, idx| {
                    array.buffer[idx] = item;
                }
            } else {
                array.buffer[0] = value;
            }

            return array;
        }

        pub fn slice(self: *const Self) []const T {
            return self.buffer[0..self.len];
        }
    };
}
