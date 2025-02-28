const std = @import("std");
const shared = @import("shared");
const arium = @import("arium");
const runner_mod = @import("runner.zig");
const config_mod = @import("config.zig");
const test_reporter = @import("test_reporter.zig");

const Allocator = std.mem.Allocator;
const fs = std.fs;
const Writer = shared.Writer;
const Tokenizer = arium.Tokenizer;
const Runner = runner_mod.Runner;
const Config = config_mod.Config;

const test_folder = "test/case";
const lang_extension = "aum";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var stdout = std.io.getStdOut().writer().any();
    var stderr = std.io.getStdErr().writer().any();

    var stdout_writer = Writer.init(&stdout);
    var stderr_writer = Writer.init(&stderr);

    {
        var test_runner = Runner.init(allocator);
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
            defer allocator.free(source);

            var diags = Config.Diags.init(allocator);
            defer diags.deinit();

            test_runner.addTest(path, try allocator.dupeZ(u8, source), &diags) catch |err| {
                has_config_errors = true;
                switch (err) {
                    error.ConfigParseFailure => {
                        test_reporter.reportConfigDiags(
                            entry.path,
                            &diags,
                            source,
                            &stderr_writer,
                        );
                    },
                    else => return err,
                }
            };
        }

        if (has_config_errors) {
            return error.ConfigParseFailure;
        }

        try test_runner.runTests(allocator, &stdout_writer, &stderr_writer);
    }

    if (gpa.deinit() == .leak) {
        return error.MemoryLeak;
    }
}

/// Return value is owned by the caller.
fn readFileAlloc(allocator: Allocator, file_path: []const u8) ![:0]u8 {
    const file = try (try std.fs.cwd().openDir(test_folder, .{})).openFile(file_path, .{ .mode = .read_only });
    defer file.close();

    return try file.readToEndAllocOptions(
        allocator,
        std.math.maxInt(u32),
        null,
        @alignOf(u8),
        0,
    );
}
