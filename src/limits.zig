const std = @import("std");

pub const max_frames = 2048;
pub const max_stack = max_frames * 256;
pub const max_constants = std.math.maxInt(u8) + 1;
pub const max_locals = std.math.maxInt(u8) + 1;
