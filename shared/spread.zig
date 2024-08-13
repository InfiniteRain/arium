pub fn spread(a: anytype, b: anytype) @TypeOf(a) {
    var result = a;

    inline for (@typeInfo(@TypeOf(b)).Struct.fields) |field| {
        @field(result, field.name) = @field(b, field.name);
    }

    return result;
}
