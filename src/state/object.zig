const std = @import("std");
const managed_memory_mod = @import("managed_memory.zig");

const Allocator = std.mem.Allocator;
const VmState = managed_memory_mod.VmState;

const ObjectError = error{
    OutOfMemory,
};

pub const Object = struct {
    const Self = @This();

    const Kind = enum {
        string,
    };

    pub const String = struct {
        object: Self,
        chars: []u8,
        hash: u32,

        fn create(
            allocator: Allocator,
            vm_state: *VmState,
            owned_buf: []u8,
            content_hash: u32,
        ) ObjectError!*String {
            const string_obj = (try Self.create(String, allocator, vm_state)).as(String);
            string_obj.chars = owned_buf;
            string_obj.hash = content_hash;

            return string_obj;
        }

        pub fn createFromOwned(
            allocator: Allocator,
            vm_state: *VmState,
            owned_buf: []u8,
        ) ObjectError!*String {
            const content_hash = hash(owned_buf);

            return try String.create(allocator, vm_state, owned_buf, content_hash);
        }

        pub fn createFromCopied(
            allocator: Allocator,
            vm_state: *VmState,
            buf: []const u8,
        ) ObjectError!*String {
            const content_hash = hash(buf);
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
    next: ?*Object,

    pub fn is(self: *Self, KindType: type) bool {
        return self.kind == kindTypeToKind(KindType);
    }

    pub fn as(self: *Self, KindType: type) *KindType {
        return @fieldParentPtr("object", self);
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

    pub fn destroyChain(self: ?*Self, allocator: Allocator) void {
        var current = self;

        while (current) |object| {
            const next = object.next;
            object.destroy(allocator);
            current = next;
        }
    }

    fn create(
        KindType: type,
        allocator: Allocator,
        vm_state: *VmState,
    ) ObjectError!*Self {
        const unknown_obj = try allocator.create(KindType);

        unknown_obj.object = .{
            .kind = kindTypeToKind(KindType),
            .next = vm_state.objects,
        };

        vm_state.objects = &unknown_obj.object;

        return &unknown_obj.object;
    }

    fn kindTypeToKind(KindType: type) Kind {
        return switch (KindType) {
            String => .string,
            else => @panic("type is not of valid object"),
        };
    }
};
