const std = @import("std");
const io_handler = @import("io_handler.zig");
const tokenizer = @import("tokenizer.zig");

const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const IoHandler = io_handler.IoHandler;
const Tokenizer = tokenizer.Tokenizer;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var stdout = std.io.getStdOut().writer();
    var stderr = std.io.getStdErr().writer();
    var stdin = std.io.getStdIn().reader();

    var io = try IoHandler.init(allocator, &stdin, &stdout, &stderr);
    defer io.deinit();

    const source = "//2";
    var t = Tokenizer.init(source);

    while (true) {
        const token = t.scanToken();

        token.print(&io);
        io.out("\n");

        if (token.kind == .eof) {
            break;
        }
    }
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
