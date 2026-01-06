const std = @import("std");
const assert = std.debug.assert;

const arium = @import("arium");
const Span = arium.Span;

pub const TestToken = struct {
    tag: Tag,
    loc: Span(u8),

    pub const Tag = enum {
        left_paren,
        right_paren,
        left_brace,
        right_brace,
        equal,

        identifier,
        int,
        float,
        string,

        true,
        false,

        dot,
        comma,
        eof,
        invalid,
    };
};

pub const TestTokenizer = struct {
    source: []const u8,
    index: usize,

    const State = enum {
        start,
        string,
        int,
        int_period,
        float,
        identifier,
    };

    pub fn init(source: []const u8) TestTokenizer {
        return .{
            .source = source,
            .index = 0,
        };
    }

    pub fn next(self: *TestTokenizer) TestToken {
        var token: TestToken = .{
            .tag = undefined,
            .loc = .{
                .index = @intCast(self.index),
                .len = undefined,
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
                    token.loc.index = @intCast(self.index);
                    continue :state .start;
                },
                '.' => {
                    _ = self.advance();
                    token.tag = .dot;
                },
                '(' => {
                    _ = self.advance();
                    token.tag = .left_paren;
                },
                ')' => {
                    _ = self.advance();
                    token.tag = .right_paren;
                },
                '{' => {
                    _ = self.advance();
                    token.tag = .left_brace;
                },
                '}' => {
                    _ = self.advance();
                    token.tag = .right_brace;
                },
                '=' => {
                    _ = self.advance();
                    token.tag = .equal;
                },
                ',' => {
                    _ = self.advance();
                    token.tag = .comma;
                },
                '"' => {
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
                    token.tag = .identifier;
                },
            },
        }

        token.loc.len = @as(u32, @intCast(self.index)) - token.loc.index;

        return token;
    }

    fn current(self: *TestTokenizer) u8 {
        if (self.index >= self.source.len) {
            return 0;
        }

        return self.source[self.index];
    }

    fn advance(self: *TestTokenizer) u8 {
        assert(self.index < self.source.len);
        self.index += 1;

        return self.current();
    }

    fn isAtEnd(self: *TestTokenizer) bool {
        return self.index >= self.source.len;
    }
};
