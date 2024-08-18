const std = @import("std");
const shared = @import("shared");
const arium = @import("arium");
const test_mod = @import("test.zig");
const test_writer_mod = @import("test_writer.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const SharedDiagnostics = shared.Diagnostics;
const Writer = shared.Writer;
const Tokenizer = arium.Tokenizer;
const IoHandler = arium.IoHandler;
const Parser = arium.Parser;
const Sema = arium.Sema;
const ManagedMemory = arium.ManagedMemory;
const Compiler = arium.Compiler;
const Vm = arium.Vm;
const Test = test_mod.Test;
const TestWriter = test_writer_mod.TestWriter;

pub const TestRunner = struct {
    const Self = @This();

    const Error = error{
        OutOfMemory,
        TestFailure,
    };

    const DiagnosticEntry = struct {
        pub const FailureInfo = union(enum) {
            parser: Parser.Diagnostics,
            sema: Sema.Diagnostics,
            compiler: Compiler.Error,
            vm: Vm.Diagnostics,
            out_mismatch: struct {
                expected: []const u8,
                actual: []const u8,
            },
            memory_leak,
        };

        path: []const u8,
        failure_info: ArrayList(FailureInfo),

        pub fn deinit(self: *DiagnosticEntry, allocator: Allocator) void {
            for (self.failure_info.items) |*info| {
                switch (info.*) {
                    .parser => |*diags| {
                        diags.deinit();
                    },
                    .sema => |*diags| {
                        diags.deinit();
                    },
                    .vm => |*diags| {
                        diags.deinit();
                    },
                    .out_mismatch => |mismatch| {
                        allocator.free(mismatch.expected);
                        allocator.free(mismatch.actual);
                    },

                    .compiler,
                    .memory_leak,
                    => {},
                }
            }
        }

        fn print(self: *DiagnosticEntry, writer: *Writer) void {
            for (self.failure_info.items) |*info| {
                switch (info.*) {
                    .parser => |*diags| {
                        for (diags.getEntries()) |diag| {
                            writer.printf("Parser error [{}:{}]: {s}\n", .{
                                diag.token.position.line,
                                diag.token.position.column,
                                diag.getMessage(),
                            });
                        }
                    },
                    .sema => |*diags| {
                        for (diags.getEntries()) |diag| {
                            writer.printf("Sema error [{}:{}]: {s}\n", .{
                                diag.position.line,
                                diag.position.column,
                                diag.message,
                            });
                        }
                    },
                    .compiler => |compiler_err| {
                        writer.printf("Compiler error: {!}\n", .{compiler_err});
                    },
                    .vm => |*diags| {
                        const diag = diags.getEntries()[0];

                        writer.printf("VM panic [{}:{}]: {s}\n", .{
                            diag.position.line,
                            diag.position.column,
                            diag.message,
                        });
                    },
                    .out_mismatch => |mismatch| {
                        writer.printf("Unexpected stdout.\nExpected:\n{s}\nActual:\n{s}", .{
                            mismatch.expected,
                            mismatch.actual,
                        });
                    },
                    .memory_leak => {
                        writer.print("Memory leak\n");
                    },
                }
            }
        }
    };

    pub const Diagnostics = SharedDiagnostics(DiagnosticEntry);

    pub const Actuals = struct {
        out: ArrayList(u8),
    };

    allocator: Allocator,
    tests: ArrayList(Test),

    pub fn init(allocator: Allocator) TestRunner {
        return .{
            .allocator = allocator,
            .tests = ArrayList(Test).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.tests.items) |*test_| {
            test_.deinit();
        }

        self.tests.clearAndFree();
    }

    /// Takes ownership of path.
    /// Takes ownership of source.
    pub fn addTest(
        self: *Self,
        path: []const u8,
        source: []const u8,
        test_diags: *Test.Diagnostics,
    ) !void {
        const test_ = try Test.initFromOwnedPathAndSource(
            self.allocator,
            path,
            source,
            test_diags,
        );
        try self.tests.append(test_);
    }

    pub fn runTests(
        self: *Self,
        allocator: Allocator,
        stdout_writer: *Writer,
        stderr_writer: *Writer,
    ) !void {
        var diags = Diagnostics.init(allocator);
        defer diags.deinit();

        var passed: u32 = 0;
        var failed: u32 = 0;

        stdout_writer.print("Running language tests...\n\n");

        for (self.tests.items) |*test_| {
            stdout_writer.printf("Running '{s}'... ", .{test_.path});

            self.runTest(test_, &diags) catch |err| switch (err) {
                error.TestFailure => {
                    stdout_writer.print("FAILED\n");
                    failed += 1;

                    continue;
                },
                else => return err,
            };

            stdout_writer.print("PASSED\n");
            passed += 1;
        }

        stdout_writer.printf(
            "\nTests ran: {}\nTests passed: {}\nTests failed: {}\n",
            .{
                passed + failed,
                passed,
                failed,
            },
        );

        if (failed > 0) {
            for (diags.getEntries()) |*diag| {
                stderr_writer.printf("\nDiagnostics for '{s}':\n", .{diag.path});
                diag.print(stderr_writer);
            }

            stderr_writer.print("\n");

            return error.TestFailure;
        }
    }

    fn runTest(self: *Self, test_: *Test, diags: *Diagnostics) Error!void {
        switch (test_.kind) {
            .run => try self.runRunKind(test_, diags),
        }
    }

    fn runRunKind(self: *Self, test_: *Test, diags: *Diagnostics) Error!void {
        assert(test_.kind == .run);

        var stdout_test_writer = TestWriter.init(self.allocator);
        defer stdout_test_writer.deinit();

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const allocator = gpa.allocator();

        var diag_entry = DiagnosticEntry{
            .path = test_.path,
            .failure_info = ArrayList(DiagnosticEntry.FailureInfo).init(self.allocator),
        };

        blk: {
            const stdout = stdout_test_writer.writer().any();

            const stdout_writer = Writer.init(&stdout);

            var tokenizer = Tokenizer.init(test_.source);
            var parser = Parser.init(allocator);

            // allocate using TestRunner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var parser_diags = Parser.Diagnostics.init(self.allocator);

            const parsed_stmt = parser.parse(&tokenizer, &parser_diags) catch |err| switch (err) {
                error.ParseFailure => {
                    try diag_entry.failure_info.append(.{
                        .parser = parser_diags,
                    });
                    break :blk;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            defer parsed_stmt.destroy(allocator);

            var sema = Sema.init(allocator);

            // allocate using TestRunner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var sema_diags = Sema.Diagnostics.init(test_.allocator);

            const sema_stmt = sema.analyze(parsed_stmt, &sema_diags) catch |err| switch (err) {
                error.SemaFailure => {
                    // try diags.add(.{
                    //     .path = test_.path,
                    //     .failure_info = .{ .sema = sema_diags },
                    // });
                    try diag_entry.failure_info.append(.{ .sema = sema_diags });
                    break :blk;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            defer sema_stmt.destroy(allocator);

            var memory = ManagedMemory.init(allocator);
            defer memory.deinit();

            Compiler.compile(&memory, sema_stmt) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    try diag_entry.failure_info.append(.{ .compiler = err });
                    break :blk;
                },
            };

            // allocate using TestRunner's allocator to prevent segfaults on dealloc.
            // diags are owned by the test runner, not the tests.
            var vm_diags = Vm.Diagnostics.init(self.allocator);

            Vm.interpret(&memory, &stdout_writer, &vm_diags, .{}) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.Panic => {
                    try diag_entry.failure_info.append(.{ .vm = vm_diags });
                },
            };
        }

        switch (gpa.deinit()) {
            .leak => {
                try diag_entry.failure_info.append(.memory_leak);
            },
            .ok => {},
        }

        if (diag_entry.failure_info.items.len == 0) {
            const actual = Test.Expectations{
                .out = stdout_test_writer.output,
            };

            try self.checkExpectations(test_, &actual, &diag_entry);
        }

        if (diag_entry.failure_info.items.len > 0) {
            try diags.add(diag_entry);
            return error.TestFailure;
        }
    }

    fn checkExpectations(
        self: *Self,
        test_: *const Test,
        actuals: *const Test.Expectations,
        diag_entry: *DiagnosticEntry,
    ) !void {
        if (!std.mem.eql(u8, actuals.out.items, test_.expectations.out.items)) {
            const expected = try self.allocator.alloc(u8, test_.expectations.out.items.len);
            @memcpy(expected, test_.expectations.out.items);

            const actual = try self.allocator.alloc(u8, actuals.out.items.len);
            @memcpy(actual, actuals.out.items);

            try diag_entry.failure_info.append(.{
                .out_mismatch = .{
                    .expected = expected,
                    .actual = actual,
                },
            });
        }
    }
};
