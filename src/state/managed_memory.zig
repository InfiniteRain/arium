const std = @import("std");
const shared = @import("shared");
const chunk_mod = @import("../compiler/chunk.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("obj.zig");
const hash_table_mod = @import("hash_table.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;
const expect = std.testing.expect;
const SharedDiagnostics = shared.Diagnostics;
const Chunk = chunk_mod.Chunk;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;
const Obj = obj_mod.Obj;
const HashTable = hash_table_mod.HashTable;
const Position = tokenizer_mod.Position;

pub const VmState = struct {
    const Self = @This();

    chunk: Chunk,
    ip: [*]u8,
    stack: Stack,
    objs: ?*Obj,
    strings: HashTable,
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
            vm_state.chunk.deinit();
            vm_state.stack.deinit();
            vm_state.strings.deinit();

            var current = vm_state.objs;

            while (current) |obj| {
                const next = obj.next;
                obj.destroy(local_allocator);
                current = next;
            }
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
