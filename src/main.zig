const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const arium = @import("arium");
const Writer = @import("shared").Writer;

const cli_mod = @import("cli.zig");

pub const VmTracer = struct {
    allocator: Allocator,
    writer: *const Writer,

    pub fn init(
        allocator: Allocator,
        writer: *const Writer,
    ) VmTracer {
        return .{
            .allocator = allocator,
            .writer = writer,
        };
    }

    pub fn debugTracer(self: *VmTracer) arium.VmNew.DebugTracer {
        return .{
            .ptr = self,
            .vtable = &.{
                .step = step,
            },
        };
    }

    fn step(ctx: *anyopaque, vm: *const arium.VmNew) void {
        const self: *VmTracer = @ptrCast(@alignCast(ctx));

        for (vm.st.items, 0..) |item, i| {
            self.writer.print("[");

            switch (vm.st_tags.items[i]) {
                .int => self.writer.printf("{}", .{item.int}),
                .float => self.writer.printf("{d}", .{item.float}),
                .bool => self.writer.printf("{}", .{item.bool}),
                .@"fn" => self.writer.printf("<fn {}>", .{item.@"fn"}),
                .object => switch (item.object.tag) {
                    .string => self.writer.printf(
                        "\"{s}\"",
                        .{item.object.as(arium.Object.String).chars},
                    ),
                },
            }

            self.writer.print("] ");
        }

        self.writer.print("\n");

        _ = arium.module_reporter.printInstruction(
            vm.module,
            self.writer,
            8,
            vm.ip,
        );
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const source = @embedFile("test.aum");

    const stdout = std.io.getStdOut().writer().any();
    const stdout_writer = Writer.init(&stdout);

    var tokenizer = arium.Tokenizer.init(source);

    var parser_diags: std.ArrayListUnmanaged(arium.Parser.Diag) = .empty;
    defer parser_diags.deinit(allocator);

    var parser = arium.Parser.init(allocator);
    defer parser.deinit();

    var ast = parser.parse(
        &tokenizer,
        &parser_diags,
    ) catch |err| {
        for (parser_diags.items) |item| {
            std.debug.print(
                "{any} in '{s}'\n",
                .{ item.tag, source[item.loc.index..][0..item.loc.len] },
            );
        }
        return err;
    };
    defer ast.deinit(allocator);

    // arium.debug_ast_reporter.printAstIndex(
    //     arium.Ast.Index,
    //     arium.Ast.Key,
    //     null,
    //     &ast,
    //     arium.Ast.Index.from(0),
    //     null,
    //     &stdout_writer,
    // );

    var intern_pool = try arium.InternPool.init(allocator);

    var sema_diags: std.ArrayListUnmanaged(arium.SemaNew.Diag) = .empty;
    defer sema_diags.deinit(allocator);

    var scratch: std.ArrayListUnmanaged(u32) = .empty;
    defer scratch.deinit(allocator);

    var air = arium.SemaNew.analyze(
        allocator,
        &intern_pool,
        &ast,
        &sema_diags,
        &scratch,
    ) catch |err| {
        for (sema_diags.items) |item| {
            std.debug.print(
                "{any} in '{s}'\n",
                .{ item.tag, source[item.loc.index..][0..item.loc.len] },
            );
            switch (item.tag) {
                .unexpected_expr_type => |x| {
                    std.debug.print("expected: {any}\n", .{x.expected.slice()});
                },
                else => {},
            }

            std.debug.print("\n\n", .{});
        }
        return err;
    };
    defer air.deinit(allocator);

    const root = arium.Air.Index.from(0);

    // arium.debug_ast_reporter.printAstIndex(
    //     arium.Air.Index,
    //     arium.Air.Key,
    //     &intern_pool,
    //     &air,
    //     root,
    //     null,
    //     &stdout_writer,
    // );

    var memory = arium.Memory.init(allocator);
    defer memory.deinit();

    var compiler_diags: std.ArrayListUnmanaged(arium.CompilerNew.Diag) = .empty;
    defer compiler_diags.deinit(allocator);

    var compiler_scratch: arium.CompilerNew.Scratch = .empty;
    defer compiler_scratch.deinit(allocator);

    var module = try arium.CompilerNew.compile(
        allocator,
        &memory,
        root,
        &intern_pool,
        &air,
        &compiler_diags,
        &compiler_scratch,
        .debug,
    );
    defer module.deinit(allocator);

    arium.module_reporter.printModule(&module, &stdout_writer);

    // var vm_tracer = VmTracer.init(allocator, &stdout_writer);

    try arium.VmNew.interpret(
        &memory,
        &module,
        &stdout_writer,
        // vm_tracer.debugTracer(),
        null,
    );
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
