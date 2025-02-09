const std = @import("std");

pub const Loc = struct {
    start: u32,
    end: u32,

    const Self = @This();

    pub fn toLineCol(self: Self, source: []const u8) struct { u32, u32 } {
        var line: u32 = 1;
        var column: u32 = 1;

        for (source, 0..) |char, index| {
            if (index == self.start) {
                return .{ line, column };
            }

            if (char == '\n') {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }

        unreachable;
    }
};

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    const Self = @This();

    pub const Tag = enum {
        left_paren,
        right_paren,
        minus,
        plus,
        slash,
        star,
        plus_plus,

        bang_equal,
        equal,
        equal_equal,
        greater,
        greater_equal,
        less,
        less_equal,

        identifier,
        int,
        float,
        string,

        true,
        false,
        not,
        @"and",
        @"or",
        do,
        end,

        mut,
        let,
        @"fn",
        @"return",

        assert,
        print,
        @"if",
        then,
        @"else",
        elseif,
        @"for",
        @"break",
        @"continue",

        new_line,
        colon,
        semicolon,
        comma,
        comment,
        eof,
        invalid,
    };
};

pub const Tokenizer = struct {
    source: [:0]const u8,
    index: usize,

    const Self = @This();

    const State = enum {
        start,
        minus,
        plus,
        slash,
        bang,
        equal,
        less,
        greater,
        string,
        int,
        int_period,
        float,
        identifier,
        comment,
    };

    const keyword_map = std.StaticStringMap(Token.Tag).initComptime(.{
        .{ "and", .@"and" },
        .{ "or", .@"or" },
        .{ "false", .false },
        .{ "true", .true },
        .{ "not", .not },
        .{ "print", .print },
        .{ "assert", .assert },
        .{ "do", .do },
        .{ "end", .end },
        .{ "mut", .mut },
        .{ "let", .let },
        .{ "if", .@"if" },
        .{ "then", .then },
        .{ "else", .@"else" },
        .{ "elseif", .elseif },
        .{ "for", .@"for" },
        .{ "break", .@"break" },
        .{ "continue", .@"continue" },
        .{ "fn", .@"fn" },
        .{ "return", .@"return" },
    });

    pub fn init(source: [:0]const u8) Self {
        return .{
            .source = source,
            .index = 0,
        };
    }

    pub fn next(self: *Self) Token {
        var token: Token = .{
            .tag = undefined,
            .loc = .{
                .start = @intCast(self.index),
                .end = undefined,
            },
        };

        state: switch (State.start) {
            .start => switch (self.current()) {
                0 => {
                    if (self.index == self.source.len) {
                        token.tag = .eof;
                    } else {
                        _ = self.advance();
                        token.tag = .invalid;
                    }
                },
                ' ', '\t', '\r' => {
                    _ = self.advance();
                    token.loc.start = @intCast(self.index);
                    continue :state .start;
                },
                '\n' => {
                    _ = self.advance();
                    token.tag = .new_line;
                },
                '(' => {
                    _ = self.advance();
                    token.tag = .left_paren;
                },
                ')' => {
                    _ = self.advance();
                    token.tag = .right_paren;
                },
                '-' => {
                    continue :state .minus;
                },
                '+' => {
                    continue :state .plus;
                },
                '/' => {
                    continue :state .slash;
                },
                '*' => {
                    _ = self.advance();
                    token.tag = .star;
                },
                '!' => {
                    continue :state .bang;
                },
                '=' => {
                    continue :state .equal;
                },
                '<' => {
                    continue :state .less;
                },
                '>' => {
                    continue :state .greater;
                },
                ';' => {
                    _ = self.advance();
                    token.tag = .semicolon;
                },
                ':' => {
                    _ = self.advance();
                    token.tag = .colon;
                },
                ',' => {
                    _ = self.advance();
                    token.tag = .comma;
                },
                '"' => {
                    _ = self.advance();
                    continue :state .string;
                },
                '0'...'9' => {
                    continue :state .int;
                },
                'a'...'z', 'A'...'Z', '_' => {
                    continue :state .identifier;
                },
                else => {
                    _ = self.advance();
                    token.tag = .invalid;
                },
            },

            .minus => switch (self.advance()) {
                '0'...'9' => {
                    continue :state .int;
                },
                else => {
                    token.tag = .minus;
                },
            },

            .plus => switch (self.advance()) {
                '+' => {
                    _ = self.advance();
                    token.tag = .plus_plus;
                },
                else => {
                    token.tag = .plus;
                },
            },

            .slash => switch (self.advance()) {
                '/' => {
                    continue :state .comment;
                },
                else => {
                    token.tag = .slash;
                },
            },

            .bang => switch (self.advance()) {
                '=' => {
                    _ = self.advance();
                    token.tag = .bang_equal;
                },
                else => {
                    token.tag = .invalid;
                },
            },

            .equal => switch (self.advance()) {
                '=' => {
                    _ = self.advance();
                    token.tag = .equal_equal;
                },
                else => {
                    token.tag = .equal;
                },
            },

            .less => switch (self.advance()) {
                '=' => {
                    _ = self.advance();
                    token.tag = .less_equal;
                },
                else => {
                    token.tag = .less;
                },
            },

            .greater => switch (self.advance()) {
                '=' => {
                    _ = self.advance();
                    token.tag = .greater_equal;
                },
                else => {
                    token.tag = .greater;
                },
            },

            .string => switch (self.advance()) {
                0 => {
                    if (self.index < self.source.len) {
                        _ = self.advance();
                    }
                    token.tag = .invalid;
                },
                '\n' => {
                    token.tag = .invalid;
                },
                '"' => {
                    _ = self.advance();
                    token.tag = .string;
                },
                else => {
                    continue :state .string;
                },
            },

            .int => switch (self.advance()) {
                '0'...'9' => {
                    continue :state .int;
                },
                '.' => {
                    continue :state .int_period;
                },
                else => {
                    token.tag = .int;
                },
            },

            .int_period => switch (self.advance()) {
                '0'...'9' => {
                    continue :state .float;
                },
                else => {
                    token.tag = .invalid;
                },
            },

            .float => switch (self.advance()) {
                '0'...'9' => {
                    continue :state .float;
                },
                else => {
                    token.tag = .float;
                },
            },

            .identifier => switch (self.advance()) {
                '0'...'9', 'a'...'z', 'A'...'Z', '_' => {
                    continue :state .identifier;
                },
                else => {
                    if (keyword_map.get(
                        self.source[token.loc.start..self.index],
                    )) |tag| {
                        token.tag = tag;
                    } else {
                        token.tag = .identifier;
                    }
                },
            },

            .comment => switch (self.advance()) {
                0 => {
                    if (self.index == self.source.len) {
                        token.tag = .comment;
                    } else {
                        _ = self.advance();
                        token.tag = .invalid;
                    }
                },
                '\n' => {
                    token.tag = .comment;
                },
                else => {
                    continue :state .comment;
                },
            },
        }

        token.loc.end = @intCast(self.index);

        return token;
    }

    fn current(self: *Self) u8 {
        return self.source[self.index];
    }

    fn advance(self: *Self) u8 {
        std.debug.assert(self.index < self.source.len);
        self.index += 1;
        return self.source[self.index];
    }
};
