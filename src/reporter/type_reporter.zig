const shared = @import("shared");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("../state/obj.zig");
const sema_ast_mod = @import("../sema/sema_ast.zig");

const Value = value_mod.Value;
const Writer = shared.Writer;
const Obj = obj_mod.Obj;
const SemaType = sema_ast_mod.SemaType;

pub fn printType(@"type": SemaType, writer: *const Writer) void {
    switch (@"type") {
        .unit => writer.print("Unit"),
        .int => writer.print("Int"),
        .float => writer.print("Float"),
        .bool => writer.print("Bool"),
        .string => writer.print("String"),
        .@"fn" => |@"fn"| {
            writer.print("Fn(");

            for (@"fn".arg_types.items, 0..) |arg, index| {
                printType(arg, writer);

                if (index != @"fn".arg_types.items.len - 1) {
                    writer.print(", ");
                }
            }

            writer.print("): ");
            printType(@"fn".return_type.*, writer);
        },
        .invalid => writer.print("Invalid"),
        .never => writer.print("Never"),
    }
}
