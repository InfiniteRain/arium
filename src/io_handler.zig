const std = @import("std");

const fs = std.fs;
const io = std.io;
const mem = std.mem;
const Writer = fs.File.Writer;
const Reader = fs.File.Reader;
const FixedBufferStream = io.FixedBufferStream;
const fixedBufferStream = io.fixedBufferStream;
const Allocator = mem.Allocator;

pub const IoHandler = struct {
    const Self = @This();
    const buffer_length = 1024;

    allocator: Allocator,
    write_buffer: []u8,
    write_buffer_stream: FixedBufferStream([]u8),
    stdin: *Reader,
    stdout: *Writer,
    stderr: *Writer,

    // todo: support buffered writers

    pub fn init(
        allocator: Allocator,
        stdin: *Reader,
        stdout: *Writer,
        stderr: *Writer,
    ) !Self {
        const write_buffer = try allocator.alloc(u8, buffer_length);
        const write_buffer_stream = fixedBufferStream(write_buffer);

        return .{
            .allocator = allocator,
            .write_buffer = write_buffer,
            .write_buffer_stream = write_buffer_stream,
            .stdin = stdin,
            .stdout = stdout,
            .stderr = stderr,
        };
    }

    pub fn deinit(self: *const Self) void {
        self.allocator.free(self.write_buffer);
    }

    pub fn out(self: *const Self, comptime text: []const u8) void {
        self.outf(text, .{});
    }

    pub fn outf(self: *const Self, comptime format: []const u8, args: anytype) void {
        self.stdout.print(format, args) catch {
            @panic("unable to print to stdout");
        };
    }

    pub fn err(self: *const Self, comptime text: []const u8) void {
        self.errf(text, .{});
    }

    pub fn errf(self: *const Self, comptime format: []const u8, args: anytype) void {
        self.stderr.print(format, args) catch {
            @panic("unable to print to stderr");
        };
    }

    pub fn readLine(self: *Self) ![]const u8 {
        self.write_buffer_stream.reset();
        try self.stdin.streamUntilDelimiter(self.write_buffer_stream.writer(), '\n', buffer_length);
        return self.write_buffer_stream.getWritten();
    }
};
