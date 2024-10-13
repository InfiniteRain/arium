const std = @import("std");
const chunk_mod = @import("../compiler/chunk.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("obj.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const limits = @import("../limits.zig");

const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const BoundedArray = std.BoundedArray;
const allocPrint = std.fmt.allocPrint;
const expect = std.testing.expect;
const Chunk = chunk_mod.Chunk;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;
const Obj = obj_mod.Obj;
const Position = tokenizer_mod.Position;

pub const CallFrame = struct {
    @"fn": *Obj.Fn,
    ip: [*]u8,
    stack: [*]Value,
    locals_count: u8,
};

pub const VmState = struct {
    const Self = @This();

    call_frames: BoundedArray(CallFrame, limits.max_frames),
    stack: Stack,
    objs: ?*Obj,
    // todo: this will recalculate hash on removal. we can rewrite this to use
    // a custom context that accepts a tuple of string and pre-calculated hash
    // to avoid this issue
    strings: StringHashMap(*Obj.String),

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.stack.deinit();
        self.strings.deinit();

        var current = self.objs;

        while (current) |obj| {
            const next = obj.next;
            obj.destroy(allocator);
            current = next;
        }
    }
};

pub const ManagedMemory = struct {
    const Self = @This();

    backing_allocator: Allocator,
    bytes_allocated: usize = 0,
    is_gc_active: bool = false,
    vm_state: ?VmState = null,

    pub fn init(backing_allocator: Allocator) Self {
        return .{
            .backing_allocator = backing_allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        const local_allocator = self.allocator();

        if (self.vm_state) |*vm_state| {
            vm_state.deinit(local_allocator);
        }
    }

    pub fn allocator(self: *Self) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    fn alloc(
        ctx: *anyopaque,
        len: usize,
        ptr_align: u8,
        ret_addr: usize,
    ) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        const out_opt = self.backing_allocator.rawAlloc(
            len,
            ptr_align,
            ret_addr,
        );

        if (out_opt != null) {
            self.bytes_allocated += len;
        }

        return out_opt;
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));
        const is_same_address = self.backing_allocator.rawResize(
            buf,
            buf_align,
            new_len,
            ret_addr,
        );

        if (is_same_address) {
            if (new_len > buf.len) {
                self.bytes_allocated += new_len - buf.len;
            } else {
                self.bytes_allocated -= buf.len - new_len;
            }
        }

        return is_same_address;
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));

        self.backing_allocator.rawFree(buf, buf_align, ret_addr);
        self.bytes_allocated -= buf.len;
    }
};

test "should track memory correctly on alloc and free" {
    // GIVEN
    var managed_memory = ManagedMemory.init(std.testing.allocator);
    const allocator = managed_memory.allocator();

    // WHEN - THEN
    try expect(managed_memory.bytes_allocated == 0);

    const buf = try allocator.alloc(u8, 8);

    try expect(managed_memory.bytes_allocated == 8);

    allocator.free(buf);

    try expect(managed_memory.bytes_allocated == 0);
}

test "should track memory correctly on realloc" {
    // GIVEN
    var managed_memory = ManagedMemory.init(std.testing.allocator);
    const allocator = managed_memory.allocator();

    // WHEN - THEN
    var buf = try allocator.alloc(u8, 8);

    try expect(managed_memory.bytes_allocated == 8);

    buf = try allocator.realloc(buf, 32);

    try expect(managed_memory.bytes_allocated == 32);

    buf = try allocator.realloc(buf, 8);

    try expect(managed_memory.bytes_allocated == 8);

    allocator.free(buf);

    try expect(managed_memory.bytes_allocated == 0);
}
