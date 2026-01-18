const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const time = std.time;
const Allocator = mem.Allocator;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArrayList = std.ArrayList;

const arium = @import("arium");
const Span = arium.Span;
const Output = arium.Output;

const constants = @import("constants.zig");
const Runner = @import("runner.zig").Runner;
const TestDiagsPrinter = @import("test_diags_printer.zig").TestDiagsPrinter;
const TestParser = @import("test_parser.zig").TestParser;

const style_end = "\x1b[0m";
const style_err = "\x1b[31m";
const style_success = "\x1b[32m";

pub fn main() !void {
    var gpa: GeneralPurposeAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = Output.init(&stdout_writer.interface);

    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = Output.init(&stderr_writer.interface);

    {
        var dir = try fs.cwd().openDir(
            "./" ++ constants.tests_dir,
            .{ .iterate = true },
        );
        defer dir.close();

        var walker = try dir.walk(allocator);
        defer walker.deinit();

        var runner_diags: Runner.Diags = .empty;
        defer runner_diags.deinit(allocator);

        var runner_scratch: Runner.Scratch = .empty;
        defer runner_scratch.deinit(allocator);

        var test_scratch: TestParser.Scratch = .empty;
        defer test_scratch.deinit(allocator);

        const runner: Runner = .init(
            allocator,
            &runner_diags,
            &runner_scratch,
            &test_scratch,
        );
        const start_ms = time.milliTimestamp();
        var passed: u32 = 0;
        var failed: u32 = 0;

        var strings: ArrayList(u8) = .empty;
        defer strings.deinit(allocator);

        var test_files: ArrayList(Span(u8)) = .empty;
        defer test_files.deinit(allocator);

        var max_str_len: usize = 0;

        while (try walker.next()) |entry| {
            if (entry.kind != .file) {
                continue;
            }

            const file_path = entry.path;

            if (!mem.endsWith(u8, file_path, "." ++ constants.lang_extension)) {
                continue;
            }

            if (file_path.len > max_str_len) {
                max_str_len = file_path.len;
            }

            const strings_top = strings.items.len;
            try strings.appendSlice(allocator, file_path);
            try test_files.append(
                allocator,
                .init(strings_top, strings.items.len),
            );
        }

        for (test_files.items) |file_path| {
            const file_path_str = file_path.toSlice(strings.items);

            stdout.printf("Running '{s}'...   ", .{file_path_str});

            for (0..max_str_len - file_path.len) |_| {
                stdout.print(" ");
            }

            runner.runTest(file_path_str) catch |err|
                switch (err) {
                    error.RunTestFailure => {
                        stdout.printf(
                            "{s}FAILED{s}\n",
                            .{ style_err, style_end },
                        );
                        failed += 1;

                        continue;
                    },
                    else => |lang_test_err| return lang_test_err,
                };

            stdout.printf("{s}PASSED{s}\n", .{ style_success, style_end });
            passed += 1;
        }

        stdout.print("\n");

        const info = .{
            .{ "Tests ran", passed + failed },
            .{ "Tests passed", passed },
            .{ "Tests failed", failed },
            .{ "Time elapsed (ms)", time.milliTimestamp() - start_ms },
        };

        comptime var max_info_len: u32 = 0;

        inline for (info) |entry| {
            if (entry[0].len > max_info_len) {
                max_info_len = entry[0].len;
            }
        }

        inline for (info) |entry| {
            stdout.printf("{s}:   ", .{entry[0]});

            for (0..max_info_len - entry[0].len) |_| {
                stdout.print(" ");
            }

            stdout.printf("{}\n", .{entry[1]});
        }

        if (runner_diags.entries.items.len > 0) {
            stdout.print("\n");
            TestDiagsPrinter.printRunnerDiags(&runner_diags, &stderr);
            return error.TestFailure;
        }
    }

    if (gpa.deinit() == .leak) {
        return error.MemoryLeak;
    }
}

test {
    std.testing.expectEqual(3, 5);
    _ = std.testing.refAllDeclsRecursive(@This());
}
