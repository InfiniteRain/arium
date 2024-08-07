const std = @import("std");
const managed_memory_mod = @import("managed_memory.zig");
const chunk_mod = @import("../compiler/chunk.zig");
const stack_mod = @import("stack.zig");
const obj_mod = @import("obj.zig");
const hash_table_mod = @import("hash_table.zig");

const Allocator = std.mem.Allocator;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Chunk = chunk_mod.Chunk;
const Stack = stack_mod.Stack;
const Obj = obj_mod.Obj;
const HashTable = hash_table_mod.HashTable;

pub const TestUtil = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        TooManyConstants,
        JumpTooBig,
    };

    backing_allocator: Allocator,
    memory: *ManagedMemory,
    vm_state: *VmState,
    table: *HashTable,

    pub fn init(backing_allocator: Allocator) Error!Self {
        var memory = try backing_allocator.create(ManagedMemory);
        memory.* = ManagedMemory.init(backing_allocator);

        const allocator = memory.allocator();

        var chunk = try Chunk.init(allocator);
        try chunk.writeU8(.return_, null);

        memory.vm_state = .{
            .chunk = chunk,
            .ip = @ptrCast(&chunk.code.items[0]),
            .stack = try Stack.init(allocator),
            .objs = null,
            .strings = try HashTable.init(allocator),
            .panic_info_opt = null,
        };

        const table = try backing_allocator.create(HashTable);
        table.* = try HashTable.init(allocator);

        return .{
            .backing_allocator = backing_allocator,
            .memory = memory,
            .vm_state = &memory.vm_state.?,
            .table = table,
        };
    }

    pub fn deinit(self: *Self) void {
        self.table.deinit();
        self.backing_allocator.destroy(self.table);
        self.memory.deinit();
        self.backing_allocator.destroy(self.memory);
    }

    pub fn createString(self: *Self, buf: []const u8) Error!*Obj.String {
        return try Obj.String.createFromCopied(
            self.memory.allocator(),
            self.vm_state,
            buf,
        );
    }
};
