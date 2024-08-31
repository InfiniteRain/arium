const std = @import("std");

pub const max_hash_table_load = 0.75;
pub const max_frames = 64;
pub const max_stack = max_frames * 256;
pub const max_constants = std.math.maxInt(u8) + 1;
pub const max_locals = std.math.maxInt(u8) + 1;
