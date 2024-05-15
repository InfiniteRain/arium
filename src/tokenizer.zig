const std = @import("std");
const io_handler = @import("io_handler.zig");

const mem = std.mem;
const expect = std.testing.expect;
const IoHandler = io_handler.IoHandler;

pub const Token = struct {
    const Self = @This();

    pub const Kind = enum {
        left_paren,
        right_paren,
        minus,
        plus,
        slash,
        star,
        number,
        comment,
        eof,
        invalid,
    };

    kind: Kind,
    lexeme: []const u8,
    line: u64,
    column: u64,

    pub fn print(self: *const Self, io: *IoHandler) void {
        io.outf("[kind = .{s}, lexeme = \"{s}\", line = {}, column = {}]", .{
            @tagName(self.kind),
            self.lexeme,
            self.line,
            self.column,
        });
    }
};

pub const Tokenizer = struct {
    const Self = @This();

    source: []const u8,
    start: usize,
    current: usize,
    line: u64,
    column: u64,
    column_start: u64,

    pub fn init(source: []const u8) Self {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .column = 0,
            .column_start = 0,
        };
    }

    pub fn scanToken(self: *Self) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) {
            return self.makeEofToken();
        }

        const char = self.advance();
        self.column_start = self.column;

        if (Self.isDigit(char)) {
            return self.number();
        }

        return switch (char) {
            '(' => self.makeToken(.left_paren),
            ')' => self.makeToken(.right_paren),
            '-' => self.makeToken(.minus),
            '+' => self.makeToken(.plus),
            '/' => if (self.match('/')) self.comment() else self.makeToken(.slash),
            '*' => self.makeToken(.star),
            else => self.makeInvalidToken(),
        };
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
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

        self.current += 1;
        return true;
    }

    fn peek(self: *const Self) ?u8 {
        if (self.isAtEnd()) {
            return null;
        }

        return self.source[self.current];
    }

    fn advance(self: *Self) u8 {
        const char = self.source[self.current];
        self.current += 1;
        self.column += 1;
        return char;
    }

    fn skipWhitespace(self: *Self) void {
        while (self.peek()) |char| {
            switch (char) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    _ = self.advance();
                    self.line += 1;
                    self.column = 0;
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

        // if at eof, adjust column so that the column of eof token is one
        // character after the comment end
        if (self.peek() == null) {
            self.column += 1;
        }

        return self.makeToken(.comment);
    }

    fn number(self: *Self) Token {
        while (Self.isDigit(self.peek() orelse 0)) {
            _ = self.advance();
        }

        return self.makeToken(.number);
    }

    fn makeToken(self: *const Self, kind: Token.Kind) Token {
        return .{
            .kind = kind,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
            .column = self.column_start,
        };
    }

    fn makeEofToken(self: *const Self) Token {
        return .{
            .kind = .eof,
            .lexeme = "",
            .line = self.line,
            .column = self.column + 1,
        };
    }

    fn makeInvalidToken(self: *const Self) Token {
        return .{
            .kind = .invalid,
            .lexeme = "",
            .line = self.line,
            .column = self.column_start,
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
    try expect(token.line == 1);
    try expect(token.column == 1);
}

test "tokenizer correctly handles a string with only a newline" {
    // GIVEN
    const source = "\n";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token = tokenizer.scanToken();

    // THEN
    try expect(token.kind == .eof);
    try expect(token.lexeme.len == 0);
    try expect(token.line == 2);
    try expect(token.column == 1);
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
    try expect(token1.kind == .number);
    try expect(mem.eql(u8, token1.lexeme, "1"));
    try expect(token1.line == 1);
    try expect(token1.column == 1);

    try expect(token2.kind == .number);
    try expect(mem.eql(u8, token2.lexeme, "12345"));
    try expect(token2.line == 1);
    try expect(token2.column == 3);

    try expect(token3.kind == .eof);
    try expect(token3.lexeme.len == 0);
    try expect(token3.line == 1);
    try expect(token3.column == 8);
}

test "tokenizer keeps track of lines correctly" {
    // GIVEN
    const source = "1\n123\n1256";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token1 = tokenizer.scanToken();
    const token2 = tokenizer.scanToken();
    const token3 = tokenizer.scanToken();
    const token4 = tokenizer.scanToken();

    // THEN
    try expect(token1.kind == .number);
    try expect(mem.eql(u8, token1.lexeme, "1"));
    try expect(token1.line == 1);
    try expect(token1.column == 1);

    try expect(token2.kind == .number);
    try expect(mem.eql(u8, token2.lexeme, "123"));
    try expect(token2.line == 2);
    try expect(token2.column == 1);

    try expect(token3.kind == .number);
    try expect(mem.eql(u8, token3.lexeme, "1256"));
    try expect(token3.line == 3);
    try expect(token3.column == 1);

    try expect(token4.kind == .eof);
    try expect(token4.lexeme.len == 0);
    try expect(token4.line == 3);
    try expect(token4.column == 5);
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
        try expect(token.line == 1);
        try expect(token.column == line);
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
    try expect(token1.kind == .number);
    try expect(mem.eql(u8, token1.lexeme, "123"));
    try expect(token1.line == 1);
    try expect(token1.column == 1);

    try expect(token2.kind == .comment);
    try expect(mem.eql(u8, token2.lexeme, "// some comment here"));
    try expect(token2.line == 1);
    try expect(token2.column == 5);

    try expect(token3.kind == .eof);
    try expect(token3.lexeme.len == 0);
    try expect(token3.line == 1);
    try expect(token3.column == 25);
}

test "tokenizer parses comment before newline correctly" {
    // GIVEN
    const source = "1//c\n2";
    var tokenizer = Tokenizer.init(source);

    // WHEN
    const token1 = tokenizer.scanToken();
    const token2 = tokenizer.scanToken();
    const token3 = tokenizer.scanToken();
    const token4 = tokenizer.scanToken();

    // THEN
    try expect(token1.kind == .number);
    try expect(mem.eql(u8, token1.lexeme, "1"));
    try expect(token1.line == 1);
    try expect(token1.column == 1);

    try expect(token2.kind == .comment);
    try expect(mem.eql(u8, token2.lexeme, "//c"));
    try expect(token2.line == 1);
    try expect(token2.column == 2);

    try expect(token3.kind == .number);
    try expect(mem.eql(u8, token3.lexeme, "2"));
    try expect(token3.line == 2);
    try expect(token3.column == 1);

    try expect(token4.kind == .eof);
    try expect(token4.lexeme.len == 0);
    try expect(token4.line == 2);
    try expect(token4.column == 2);
}
