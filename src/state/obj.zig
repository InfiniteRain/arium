const std = @import("std");
const managed_memory_mod = @import("managed_memory.zig");
const chunk_mod = @import("../compiler/chunk.zig");

const Allocator = std.mem.Allocator;
const VmState = managed_memory_mod.VmState;
const Chunk = chunk_mod.Chunk;

pub const Obj = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
    };

    const Kind = enum {
        string,
        @"fn",
    };

    pub const String = struct {
        obj: Self,
        chars: []u8,

        fn create(
            allocator: Allocator,
            vm_state: *VmState,
            owned_buf: []u8,
        ) Error!*String {
            const string_obj = (try Self.create(String, allocator, vm_state)).as(String);
            string_obj.chars = owned_buf;

            vm_state.stack.push(.{ .obj = &string_obj.obj });
            _ = try vm_state.strings.put(owned_buf, string_obj);
            _ = vm_state.stack.pop();

            return string_obj;
        }

        pub fn createFromOwned(
            allocator: Allocator,
            vm_state: *VmState,
            owned_buf: []u8,
        ) Error!*String {
            const interned_opt = vm_state.strings.get(owned_buf);

            if (interned_opt) |interned| {
                allocator.free(owned_buf);
                return interned;
            }

            return try String.create(allocator, vm_state, owned_buf);
        }

        pub fn createFromCopied(
            allocator: Allocator,
            vm_state: *VmState,
            buf: []const u8,
        ) Error!*String {
            const interned_opt = vm_state.strings.get(buf);

            if (interned_opt) |interned| {
                return interned;
            }

            const owned_buf = try allocator.alloc(u8, buf.len);
            @memcpy(owned_buf, buf);

            return try String.create(allocator, vm_state, owned_buf);
        }
    };

    pub const Fn = struct {
        obj: Self,
        name: ?*String,
        locals_count: u8,
        chunk: Chunk,

        pub fn create(
            allocator: Allocator,
            vm_state: *VmState,
            name: ?*String,
            locals_count: u8,
        ) Error!*Fn {
            const fn_obj = (try Self.create(Fn, allocator, vm_state)).as(Fn);

            fn_obj.name = name;
            fn_obj.locals_count = locals_count;
            fn_obj.chunk = try Chunk.init(allocator);

            return fn_obj;
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
            .@"fn" => {
                const @"fn" = self.as(Fn);
                @"fn".chunk.deinit();
                allocator.destroy(@"fn");
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
            Fn => .@"fn",
            else => @panic("type is not of valid object"),
        };
    }
};
