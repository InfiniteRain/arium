const std = @import("std");
const shared = @import("shared");
const token_trie_mod = @import("token_trie.zig");

const mem = std.mem;
const Allocator = mem.Allocator;
const expect = std.testing.expect;
const Writer = shared.Writer;
const generateTrie = token_trie_mod.generateTrie;

const token_trie = generateTrie(.{
    .{ "and", .and_ },
    .{ "or", .or_ },
    .{ "false", .false_ },
    .{ "true", .true_ },
    .{ "not", .not },
    .{ "print", .print },
    .{ "assert", .assert },
    .{ "do", .do },
    .{ "end", .end },
    .{ "mut", .mut },
    .{ "let", .let },
});

pub const Position = struct {
    line: u64,
    column: u64,
};

pub const Token = struct {
    const Self = @This();

    pub const Kind = enum {
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

        true_,
        false_,
        not,
        and_,
        or_,
        do,
        end,
        mut,
        let,

        assert,
        print,

        new_line,
        colon,
        semicolon,
        comment,
        eof,
        invalid,
    };

    kind: Kind,
    lexeme: []const u8,
    position: Position,
};

pub const Tokenizer = struct {
    const Self = @This();

    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: u64 = 1,
    column: u64 = 0,
    column_start: u64 = 0,

    pub fn init(source: []const u8) Self {
        return .{
            .source = source,
        };
    }

    pub fn scanNonCommentToken(self: *Self) Token {
        while (true) {
            const next_token = self.scanToken();

            if (next_token.kind != .comment) {
                return next_token;
            }
        }
    }

    pub fn scanToken(self: *Self) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) {
            return self.makeEofToken();
        }

        const old_line = self.line;
        const old_column = self.column;
        const char = self.advance();
        self.column_start = self.column;

        return switch (char) {
            '(' => self.makeToken(.left_paren),
            ')' => self.makeToken(.right_paren),
            '-' => if (self.matchDigit()) self.number() else self.makeToken(.minus),
            '+' => self.makeToken(if (self.match('+')) .plus_plus else .plus),
            '/' => if (self.match('/')) self.comment() else self.makeToken(.slash),
            '*' => self.makeToken(.star),
            '!' => self.makeToken(if (self.match('=')) .bang_equal else return self.makeInvalidToken("Invalid character.")),
            '=' => self.makeToken(if (self.match('=')) .equal_equal else .equal),
            '<' => self.makeToken(if (self.match('=')) .less_equal else .less),
            '>' => self.makeToken(if (self.match('=')) .greater_equal else .greater),
            '"' => self.string(),
            ';' => self.makeToken(.semicolon),
            ':' => self.makeToken(.colon),
            '\n' => makeNewLineToken(old_line, old_column),
            else => {
                if (Self.isAlpha(char)) {
                    return self.identifier();
                }

                if (Self.isDigit(char)) {
                    return self.number();
                }

                return self.makeInvalidToken("Invalid character.");
            },
        };
    }

    fn isDigit(char_opt: ?u8) bool {
        const char = char_opt orelse return false;

        return char >= '0' and char <= '9';
    }

    fn isAlpha(char_opt: ?u8) bool {
        const char = char_opt orelse return false;

        return (char >= 'a' and char <= 'z') or
            (char >= 'A' and char <= 'Z') or
            char == '_';
    }

    fn isAtEnd(self: *const Self) bool {
        return self.current >= self.source.len;
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }

        if (self.source[self.current] != expected) {
            return false;
        }

        _ = self.advance();
        return true;
    }

    fn matchDigit(self: *Self) bool {
        if (self.isAtEnd()) {
            return false;
        }

        const current = self.source[self.current];

        if (current < '0' or current > '9') {
            return false;
        }

        _ = self.advance();
        return true;
    }

    fn peek(self: *const Self) ?u8 {
        if (self.isAtEnd()) {
            return null;
        }

        return self.source[self.current];
    }

    fn peekNext(self: *Self) ?u8 {
        if (self.current + 1 >= self.source.len) {
            return null;
        }

        return self.source[self.current + 1];
    }

    fn advance(self: *Self) u8 {
        const char = self.source[self.current];
        self.current += 1;

        if (char == '\n') {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }

        return char;
    }

    fn skipWhitespace(self: *Self) void {
        while (self.peek()) |char| {
            switch (char) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                else => break,
            }
        }
    }

    fn comment(self: *Self) Token {
        while (self.peek()) |char| {
            if (char == '\n') {
                break;
            }

            _ = self.advance();
        }

        return self.makeToken(.comment);
    }

    fn identifier(self: *Self) Token {
        while (true) {
            const char = self.peek();

            if (!Self.isAlpha(char) and !Self.isDigit(char)) {
                break;
            }

            _ = self.advance();
        }

        return self.makeToken(self.identifierKind());
    }

    fn number(self: *Self) Token {
        var is_float = false;

        while (Self.isDigit(self.peek() orelse 0)) {
            _ = self.advance();
        }

        if (self.peek() == '.' and Self.isDigit(self.peekNext())) {
            is_float = true;
            _ = self.advance();

            while (Self.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(if (is_float) .float else .int);
    }

    fn string(self: *Self) Token {
        while (self.peek() != '"' and self.peek() != '\n' and !self.isAtEnd()) {
            _ = self.advance();
        }

        if (self.peek() == '\n' or self.isAtEnd()) {
            return self.makeInvalidToken("Unterminated string.");
        }

        _ = self.advance();
        return self.makeToken(.string);
    }

    fn identifierKind(self: *Self) Token.Kind {
        const lexeme = self.source[self.start..self.current];

        return token_trie.findWord(lexeme) catch return .identifier;
    }

    fn makeToken(self: *const Self, kind: Token.Kind) Token {
        return .{
            .kind = kind,
            .lexeme = self.source[self.start..self.current],
            .position = .{
                .line = self.line,
                .column = self.column_start,
            },
        };
    }

    fn makeNewLineToken(old_line: u64, old_column: u64) Token {
        return .{
            .kind = .new_line,
            .lexeme = "\n",
            .position = .{
                .line = old_line,
                .column = old_column + 1,
            },
        };
    }

    fn makeEofToken(self: *const Self) Token {
        return .{
            .kind = .eof,
            .lexeme = "",
            .position = .{
                .line = self.line,
                .column = self.column + 1,
            },
        };
    }

    fn makeInvalidToken(self: *const Self, message: []const u8) Token {
        return .{
            .kind = .invalid,
            .lexeme = message,
            .position = .{
                .line = self.line,
                .column = self.column_start,
            },
        };
    }
};

test "tokenizer results with eof on an empty string" {
    // GIVEN
    const source = "";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token = tokenizer.scanToken();

    // THEN
    try expect(token.kind == .eof);
    try expect(token.lexeme.len == 0);
    try expect(token.position.line == 1);
    try expect(token.position.column == 1);
}

test "tokenizer correctly handles a string with only a newline" {
    // GIVEN
    const source = "\n";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token = tokenizer.scanToken();

    // THEN
    try expect(token.kind == .new_line);
    try expect(token.lexeme.len == 1);
    try expect(token.position.line == 1);
    try expect(token.position.column == 1);
}

test "tokenizer parses numbers correctly" {
    // GIVEN
    const source = "1 12345";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token1 = tokenizer.scanToken();
    const token2 = tokenizer.scanToken();
    const token3 = tokenizer.scanToken();

    // THEN
    try expect(token1.kind == .int);
    try expect(mem.eql(u8, token1.lexeme, "1"));
    try expect(token1.position.line == 1);
    try expect(token1.position.column == 1);

    try expect(token2.kind == .int);
    try expect(mem.eql(u8, token2.lexeme, "12345"));
    try expect(token2.position.line == 1);
    try expect(token2.position.column == 3);

    try expect(token3.kind == .eof);
    try expect(token3.lexeme.len == 0);
    try expect(token3.position.line == 1);
    try expect(token3.position.column == 8);
}

test "tokenizer keeps track of lines correctly" {
    // GIVEN
    const source = "1\n123\n1256";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token1 = tokenizer.scanToken();
    _ = tokenizer.scanToken();
    const token2 = tokenizer.scanToken();
    _ = tokenizer.scanToken();
    const token3 = tokenizer.scanToken();
    _ = tokenizer.scanToken();
    const token4 = tokenizer.scanToken();

    // THEN
    try expect(token1.kind == .int);
    try expect(mem.eql(u8, token1.lexeme, "1"));
    try expect(token1.position.line == 1);
    try expect(token1.position.column == 1);

    try expect(token2.kind == .int);
    try expect(mem.eql(u8, token2.lexeme, "123"));
    try expect(token2.position.line == 2);
    try expect(token2.position.column == 1);

    try expect(token3.kind == .int);
    try expect(mem.eql(u8, token3.lexeme, "1256"));
    try expect(token3.position.line == 3);
    try expect(token3.position.column == 1);

    try expect(token4.kind == .eof);
    try expect(token4.lexeme.len == 0);
    try expect(token4.position.line == 3);
    try expect(token4.position.column == 5);
}

test "tokenizer parses single character tokens correctly" {
    // GIVEN
    const source = "()-+/*";
    var tokenizer = Tokenizer.init(source);

    // WHEN - THEN
    for (source, 1..) |char, line| {
        const token = tokenizer.scanToken();

        try expect(token.kind == switch (char) {
            '(' => Token.Kind.left_paren,
            ')' => Token.Kind.right_paren,
            '-' => Token.Kind.minus,
            '+' => Token.Kind.plus,
            '/' => Token.Kind.slash,
            '*' => Token.Kind.star,
            else => @panic("unexpected char"),
        });
        try expect(token.lexeme.len == 1 and token.lexeme[0] == char);
        try expect(token.position.line == 1);
        try expect(token.position.column == line);
    }
}

test "tokenizer parses comments before eof correctly" {
    // GIVEN
    const source = "123 // some comment here";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token1 = tokenizer.scanToken();
    const token2 = tokenizer.scanToken();
    const token3 = tokenizer.scanToken();

    // THEN
    try expect(token1.kind == .int);
    try expect(mem.eql(u8, token1.lexeme, "123"));
    try expect(token1.position.line == 1);
    try expect(token1.position.column == 1);

    try expect(token2.kind == .comment);
    try expect(mem.eql(u8, token2.lexeme, "// some comment here"));
    try expect(token2.position.line == 1);
    try expect(token2.position.column == 5);

    try expect(token3.kind == .eof);
    try expect(token3.lexeme.len == 0);
    try expect(token3.position.line == 1);
    try expect(token3.position.column == 25);
}

test "tokenizer parses comment before newline correctly" {
    // GIVEN
    const source = "1//c\n2";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token1 = tokenizer.scanToken();
    const token2 = tokenizer.scanToken();
    _ = tokenizer.scanToken();
    const token3 = tokenizer.scanToken();
    const token4 = tokenizer.scanToken();

    // THEN
    try expect(token1.kind == .int);
    try expect(mem.eql(u8, token1.lexeme, "1"));
    try expect(token1.position.line == 1);
    try expect(token1.position.column == 1);

    try expect(token2.kind == .comment);
    try expect(mem.eql(u8, token2.lexeme, "//c"));
    try expect(token2.position.line == 1);
    try expect(token2.position.column == 2);

    try expect(token3.kind == .int);
    try expect(mem.eql(u8, token3.lexeme, "2"));
    try expect(token3.position.line == 2);
    try expect(token3.position.column == 1);

    try expect(token4.kind == .eof);
    try expect(token4.lexeme.len == 0);
    try expect(token4.position.line == 2);
    try expect(token4.position.column == 2);
}

test "scanNonCommentToken scans while ignoring comment tokens" {
    // GIVEN
    const source = "// comment\n//comment 2\nprint";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token1 = tokenizer.scanNonCommentToken();
    const token2 = tokenizer.scanNonCommentToken();
    const token3 = tokenizer.scanNonCommentToken();
    const token4 = tokenizer.scanNonCommentToken();

    // THEN
    try expect(token1.kind == .new_line);
    try expect(token2.kind == .new_line);
    try expect(token3.kind == .print);
    try expect(token4.kind == .eof);
}
