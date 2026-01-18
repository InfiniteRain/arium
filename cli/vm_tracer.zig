const arium = @import("arium");
const Vm = arium.Vm;
const Object = arium.Object;
const Output = arium.Output;

const ModulePrinter = @import("module_printer.zig").ModulePrinter;

pub const VmTracer = struct {
    output: *const Output,

    pub fn init(output: *const Output) VmTracer {
        return .{
            .output = output,
        };
    }

    pub fn debugTracer(self: *const VmTracer) Vm.DebugTracer {
        return .{
            .ptr = self,
            .vtable = &.{
                .step = step,
            },
        };
    }

    fn step(ctx: *const anyopaque, vm: *const Vm) void {
        const self: *const VmTracer = @ptrCast(@alignCast(ctx));

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

        _ = ModulePrinter.printInstruction(
            vm.module,
            self.output,
            8,
            vm.ip,
        );
    }
};
