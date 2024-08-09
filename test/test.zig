const std = @import("std");
const arium = @import("arium");
const shared = @import("shared");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const allocPrint = std.fmt.allocPrint;
const Tokenizer = arium.Tokenizer;
const Position = arium.Position;
const Parser = arium.Parser;
const Vm = arium.Vm;
const SharedDiagnostics = shared.Diagnostics;

pub const Test = struct {
    const Self = @This();

    pub const DiagnosticEntry = struct {
        message: []const u8,
        position: Position,

        pub fn deinit(self: *DiagnosticEntry, allocator: Allocator) void {
            allocator.free(self.message);
        }
    };

    pub const Diagnostics = SharedDiagnostics(DiagnosticEntry);

    const Kind = enum {
        run,
    };

    const DirectiveKind = enum {
        out,
    };

    pub const Expectations = struct {
        out: ArrayList(u8),
    };

    allocator: Allocator,
    path: []const u8,
    source: []const u8,
    kind: Kind,
    vm_config: Vm.Config,
    expectations: Expectations,

    /// Takes ownership of path.
    /// Takes ownership of source.
    pub fn initFromOwnedPathAndSource(
        allocator: Allocator,
        path: []const u8,
        source: []const u8,
        diags: *Diagnostics,
    ) error{
        OutOfMemory,
        ConfigParseFailure,
    }!Self {
        errdefer allocator.free(path);
        errdefer allocator.free(source);

        var tokenizer = Tokenizer.init(source);
        const header_comment = tokenizer.scanToken();
        const header_lexeme = header_comment.lexeme;

        if (header_comment.kind != .comment or header_lexeme.len < 3 or header_lexeme[2] != '/') {
            try addDiag(diags, header_comment.position, "Test header is missing.", .{});
            return error.ConfigParseFailure;
        }

        const test_kind = std.mem.trim(u8, header_comment.lexeme[3..], " ");
        var is_valid_kind = false;

        inline for (@typeInfo(Kind).Enum.fields) |field| {
            if (std.mem.eql(u8, test_kind, field.name)) {
                is_valid_kind = true;
            }
        }

        if (!is_valid_kind) {
            try addDiag(diags, header_comment.position, "Invalid test kind '{s}'.", .{test_kind});
            return error.ConfigParseFailure;
        }

        var expected_out = ArrayList(u8).init(allocator);
        errdefer expected_out.clearAndFree();

        var current_token = tokenizer.scanToken();

        while (current_token.kind != .eof) : (current_token = tokenizer.scanToken()) {
            const lexeme = current_token.lexeme;

            if (current_token.kind != .comment or lexeme.len < 3 or lexeme[2] != '/') {
                continue;
            }

            const directive_line = std.mem.trim(u8, lexeme[3..], " ");
            var split = std.mem.splitScalar(u8, directive_line, ' ');
            const directive_kind_str = split.first();
            const directive_arg = std.mem.trim(u8, split.rest(), " ");
            var directive_kind_opt: ?DirectiveKind = null;

            inline for (@typeInfo(DirectiveKind).Enum.fields) |field| {
                if (std.mem.eql(u8, directive_kind_str, field.name)) {
                    directive_kind_opt = std.meta.stringToEnum(DirectiveKind, directive_kind_str);
                }
            }

            if (directive_kind_opt) |directive_kind| switch (directive_kind) {
                .out => {
                    try expected_out.appendSlice(directive_arg);
                    try expected_out.append('\n');
                },
            } else {
                try addDiag(
                    diags,
                    current_token.position,
                    "Invalid directive kind '{s}'.",
                    .{directive_kind_str},
                );
            }
        }

        if (diags.getEntries().len > 0) {
            return error.ConfigParseFailure;
        }

        return .{
            .allocator = allocator,
            .path = path,
            .source = source,
            .kind = std.meta.stringToEnum(Kind, test_kind) orelse unreachable,
            .vm_config = .{},
            .expectations = .{ .out = expected_out },
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.path);
        self.allocator.free(self.source);

        self.expectations.out.clearAndFree();
    }

    fn addDiag(
        diags: *Diagnostics,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        const message = try allocPrint(diags.allocator, fmt, args);

        try diags.add(.{
            .message = message,
            .position = position,
        });
    }
};
