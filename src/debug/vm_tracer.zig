const std = @import("std");
const Allocator = std.mem.Allocator;

const shared = @import("shared");
const Output = shared.Output;

const vm_mod = @import("../vm.zig");
const Vm = vm_mod.Vm;
const memory_mod = @import("../memory.zig");
const Object = memory_mod.Object;
const module_reporter = @import("../reporter/module_reporter.zig");

pub const VmTracer = struct {
    allocator: Allocator,
    output: *const Output,

    pub fn init(
        allocator: Allocator,
        output: *const Output,
    ) VmTracer {
        return .{
            .allocator = allocator,
            .output = output,
        };
    }

    pub fn debugTracer(self: *VmTracer) Vm.DebugTracer {
        return .{
            .ptr = self,
            .vtable = &.{
                .step = step,
            },
        };
    }

    fn step(ctx: *anyopaque, vm: *const Vm) void {
        const self: *VmTracer = @ptrCast(@alignCast(ctx));

        for (vm.st.items, 0..) |item, i| {
            self.output.print("[");

            switch (vm.st_tags.items[i]) {
                .int => self.output.printf("{}", .{item.int}),
                .float => self.output.printf("{d}", .{item.float}),
                .bool => self.output.printf("{}", .{item.bool}),
                .@"fn" => self.output.printf("<fn {}>", .{item.@"fn"}),
                .object => switch (item.object.tag) {
                    .string => self.output.printf(
                        "\"{s}\"",
                        .{item.object.as(Object.String).chars},
                    ),
                },
            }

            self.output.print("] ");
        }

        self.output.print("\n");

        _ = module_reporter.printInstruction(
            vm.module,
            self.output,
            8,
            vm.ip,
        );
    }
};
