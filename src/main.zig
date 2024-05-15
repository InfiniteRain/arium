const std = @import("std");
const io_handler = @import("io_handler.zig");
const tokenizer_pkg = @import("tokenizer.zig");
const parser_pkg = @import("parser.zig");

const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const IoHandler = io_handler.IoHandler;
const Tokenizer = tokenizer_pkg.Tokenizer;
const Parser = parser_pkg.Parser;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var stdout = std.io.getStdOut().writer();
    var stderr = std.io.getStdErr().writer();
    var stdin = std.io.getStdIn().reader();

    var io = try IoHandler.init(allocator, &stdin, &stdout, &stderr);
    defer io.deinit();

    const source = "(2 + 2) * -2";
    var tokenizer = Tokenizer.init(source);

    var parser = Parser.init(&tokenizer);
    const expr = parser.parse(allocator) catch |err| switch (err) {
        error.ParseFailure => {
            io.outf("{s}\n", .{parser.err.?.message});
            return;
        },
        else => return err,
    };
    defer expr.destroy(allocator);

    expr.print(&io);
    io.out("\n");
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
