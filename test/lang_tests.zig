const std = @import("std");
const arium = @import("arium");
const test_mod = @import("test.zig");
const test_runner_mod = @import("test_runner.zig");

const Allocator = std.mem.Allocator;
const fs = std.fs;
const Tokenizer = arium.Tokenizer;
const TestRunner = test_runner_mod.TestRunner;
const Test = test_mod.Test;

const test_folder = "test/case";
const lang_extension = "aum";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var stdout = std.io.getStdOut().writer().any();
    var stderr = std.io.getStdErr().writer().any();

    {
        var test_runner = TestRunner.init(allocator);
        defer test_runner.deinit();

        const dir = try fs.cwd().openDir("./" ++ test_folder, .{ .iterate = true });
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        var has_config_errors = false;

        while (try walker.next()) |entry| {
            const is_long_enough = entry.basename.len > lang_extension.len;
            const is_correct_extension = if (!is_long_enough)
                false
            else
                std.mem.eql(
                    u8,
                    entry.basename[entry.basename.len - (lang_extension.len + 1) ..],
                    "." ++ lang_extension,
                );

            if (!is_correct_extension) {
                continue;
            }

            const path = try allocator.alloc(u8, entry.path.len);
            @memcpy(path, entry.path);
            const source = try readFileAlloc(allocator, entry.path);

            var diags = Test.Diagnostics.init(allocator);
            defer diags.deinit();

            test_runner.addTest(path, source, &diags) catch |err| {
                has_config_errors = true;
                switch (err) {
                    error.ConfigParseFailure => {
                        try stderr.print("Test configuration diagnostics for '{s}':\n", .{entry.path});

                        for (diags.getEntries()) |diag| {
                            try stderr.print("Line {}: {s}\n", .{
                                diag.position.line,
                                diag.message,
                            });
                        }

                        try stderr.print("\n", .{});
                    },
                    else => return err,
                }
            };
        }

        if (has_config_errors) {
            return error.ConfigParseFailure;
        }

        try test_runner.runTests(allocator, &stdout, &stderr);
    }

    if (gpa.deinit() == .leak) {
        return error.MemoryLeak;
    }
}

/// Return value is owned by the caller.
fn readFileAlloc(allocator: Allocator, file_path: []const u8) ![]u8 {
    const file = try (try std.fs.cwd().openDir(test_folder, .{})).openFile(file_path, .{ .mode = .read_only });
    defer file.close();

    try file.seekFromEnd(0);
    const end = try file.getPos();
    try file.seekTo(0);

    return try file.reader().readAllAlloc(allocator, end);
}
