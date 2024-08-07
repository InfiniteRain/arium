const std = @import("std");
const managed_memory_mod = @import("managed_memory.zig");

const Allocator = std.mem.Allocator;
const VmState = managed_memory_mod.VmState;

pub const Obj = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
    };

    const Kind = enum {
        string,
    };

    pub const String = struct {
        obj: Self,
        chars: []u8,
        hash: u32,

        fn create(
            allocator: Allocator,
            vm_state: *VmState,
            owned_buf: []u8,
            content_hash: u32,
        ) Error!*String {
            const string_obj = (try Self.create(String, allocator, vm_state)).as(String);
            string_obj.chars = owned_buf;
            string_obj.hash = content_hash;

            vm_state.stack.push(.{ .obj = &string_obj.obj });
            _ = try vm_state.strings.set(string_obj, .unit);
            _ = vm_state.stack.pop();

            return string_obj;
        }

        pub fn createFromOwned(
            allocator: Allocator,
            vm_state: *VmState,
            owned_buf: []u8,
        ) Error!*String {
            const content_hash = hash(owned_buf);
            const interned_opt = vm_state.strings.findString(owned_buf, content_hash);

            if (interned_opt) |interned| {
                allocator.free(owned_buf);
                return interned;
            }

            return try String.create(allocator, vm_state, owned_buf, content_hash);
        }

        pub fn createFromCopied(
            allocator: Allocator,
            vm_state: *VmState,
            buf: []const u8,
        ) Error!*String {
            const content_hash = hash(buf);
            const interned_opt = vm_state.strings.findString(buf, content_hash);

            if (interned_opt) |interned| {
                return interned;
            }

            const owned_buf = try allocator.alloc(u8, buf.len);
            @memcpy(owned_buf, buf);

            return try String.create(allocator, vm_state, owned_buf, content_hash);
        }

        fn hash(buf: []const u8) u32 {
            var current_hash: u32 = 2_166_136_261;

            for (buf) |char| {
                current_hash ^= char;
                current_hash *%= 16_777_619;
            }

            return current_hash;
        }
    };

    kind: Kind,
    next: ?*Obj,

    pub fn is(self: *Self, KindType: type) bool {
        return self.kind == kindTypeToKind(KindType);
    }

    pub fn as(self: *Self, KindType: type) *KindType {
        return @fieldParentPtr("obj", self);
    }

    pub fn destroy(self: *Self, allocator: Allocator) void {
        switch (self.kind) {
            .string => {
                const string = self.as(String);
                allocator.free(string.chars);
                allocator.destroy(string);
            },
        }
    }

    fn create(
        KindType: type,
        allocator: Allocator,
        vm_state: *VmState,
    ) Error!*Self {
        const unknown_obj = try allocator.create(KindType);

        unknown_obj.obj = .{
            .kind = kindTypeToKind(KindType),
            .next = vm_state.objs,
        };

        vm_state.objs = &unknown_obj.obj;

        return &unknown_obj.obj;
    }

    fn kindTypeToKind(KindType: type) Kind {
        return switch (KindType) {
            String => .string,
            else => @panic("type is not of valid object"),
        };
    }
};
