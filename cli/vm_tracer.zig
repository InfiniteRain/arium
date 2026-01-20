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

    pub fn debugTracer(self: *const VmTracer) Vm(.debug).DebugTracer {
        return .{
            .ptr = self,
            .vtable = &.{
                .step = step,
            },
        };
    }

    fn step(ctx: *const anyopaque, vm: *const Vm(.debug)) void {
        const self: *const VmTracer = @ptrCast(@alignCast(ctx));

        for (vm.st.items) |item| {
            self.output.print("[");

            switch (item) {
                .int => |int| self.output.printf("{}", .{int}),
                .float => |float| self.output.printf("{d}", .{float}),
                .bool => |@"bool"| self.output.printf("{}", .{@"bool"}),
                .@"fn" => |@"fn"| self.output.printf("<fn {}>", .{@"fn"}),
                .object => |object| switch (object.tag) {
                    .string => self.output.printf(
                        "\"{s}\"",
                        .{object.as(Object(.debug).String).chars},
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
