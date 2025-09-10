const std = @import("std");

pub const Token = union(enum) {
    // general syntax
    LParen,
    RParen,
    LBrace,
    RBrace,
    LCurly,
    RCurly,
    Dot,
    Comma,
    Semicolon,
    Colon,
    Equal,
    DotStar,

    // keywords
    Let,
    Const,
    Fn,
    Pub,

    If,
    Else,
    Switch,
    Case,
    While,
    Do,
    For,
    Break,
    Continue,
    Return,
    True,
    False,

    // Arithmetic
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    // Bitwise
    Bang, // !
    Ampersand, // &
    Pipe, // |
    Caret, // ^
    DoubleLeftCaret, // <<
    DoubleRightCaret, // >>

    // Assignment
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,

    BangEqual, // !=
    AmpersandEqual, // &=
    PipeEqual, // |=
    CaretEqual, // ^=
    DoubleLeftCaretEqual, // <<=
    DoubleRightCaretEqual, // >>=

    // Conditional
    EqualEqual, // ==
    DoubleAmpersand, // &&
    DoublePipe, // ||
    LeftCaret, // <
    RightCaret, // >
    LeftCaretEqual, // <=
    RightCaretEqual, // >=

    StringLit: []const u8,
    Identifier: []const u8,
    IntLit: i64,
    FloatLit: f64,
    Bool: bool,
    EscChar: u8,

    // special characters
    Null,
    Newline,
    Tab,

    Unknown: u8,
    EOF,
};

pub const Tokenizer = struct {
    i: u64,
    input: []const u8,
    out: std.ArrayList(Token),
    keywords: std.StringHashMap(Token),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Tokenizer {
        var map = std.StringHashMap(Token).init(allocator);

        try map.put("let", Token.Let);
        try map.put("const", Token.Const);
        try map.put("pub", Token.Pub);
        try map.put("fn", Token.Fn);

        try map.put("if", Token.If);
        try map.put("else", Token.Else);
        try map.put("switch", Token.Switch);
        try map.put("case", Token.Case);
        try map.put("while", Token.While);
        try map.put("do", Token.Do);
        try map.put("for", Token.For);
        try map.put("break", Token.Break);
        try map.put("continue", Token.Continue);
        try map.put("return", Token.Return);
        try map.put("true", Token{ .Bool = true });
        try map.put("false", Token{ .Bool = false });

        return .{ .i = 0, .input = "", .out = std.ArrayList(Token).init(allocator), .keywords = map, .allocator = allocator };
    }

    fn peek(self: *Tokenizer) ?u8 {
        if (self.i + 1 >= self.input.len) {
            return null;
        }

        return self.input[self.i + 1];
    }

    fn isDouble(self: *Tokenizer, c: u8) bool {
        if (self.peek()) |next| {
            if (next == c) return true;
        }

        return false;
    }

    fn isNextEqual(self: *Tokenizer, default: Token, equal: Token) Token {
        if (self.peek() == '=') {
            self.i += 1;
            return equal;
        } else {
            return default;
        }
    }

    fn scanIdentifer(self: *Tokenizer, curr: u8) ![]const u8 {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        try buffer.append(curr);

        while (self.peek()) |next| {
            switch (next) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                    try buffer.append(next);
                    self.i += 1;
                },
                else => break,
            }
        }

        return try buffer.toOwnedSlice();
    }

    fn scanNumber(self: *Tokenizer, c: u8) !Token {
        var isFloat: bool = false;
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        try buffer.append(c);

        while (self.peek()) |next| {
            switch (next) {
                '0'...'9' => try buffer.append(next),
                '.' => {
                    try buffer.append(next);
                    isFloat = true;
                },
                else => break,
            }

            self.i += 1;
        }

        const slice = try buffer.toOwnedSlice();
        if (isFloat) {
            const float = try std.fmt.parseFloat(f64, slice);
            return Token{ .FloatLit = float };
        } else {
            const int = try std.fmt.parseInt(i64, slice, 10);
            return Token{ .IntLit = int };
        }
    }

    fn scanStr(self: *Tokenizer, delimiter: u8) !Token {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        while (self.peek()) |next| {
            if (next == delimiter) {
                self.i += 1;
                const str = try buffer.toOwnedSlice();
                return Token{ .StringLit = str };
            }

            if (next == '\\') { // escaped characters in stringLits
                self.i += 1;

                if (self.peek()) |c| {
                    switch (c) { // todo add all valid ones here
                        'n' => try buffer.append('\n'),
                        'r' => try buffer.append('\r'),
                        't' => try buffer.append('\t'),
                        else => try buffer.append(c), // todo report error here instead of blinding appending the next character
                    }

                    self.i += 1;
                } else continue;
            } else {
                try buffer.append(next);
                self.i += 1;
            }
        }

        return error.missingClosingQuote;
    }

    fn getToken(self: *Tokenizer, c: u8) ?Token {
        return switch (c) {
            '(' => Token.LParen,
            ')' => Token.RParen,
            '[' => Token.LBrace,
            ']' => Token.RBrace,
            '{' => Token.LCurly,
            '}' => Token.RCurly,
            '.' => {
                if (self.peek() == '*') {
                    self.i += 1;
                    return Token.DotStar;
                }

                return Token.Dot;
            },
            ',' => Token.Comma,
            ';' => Token.Semicolon,
            ':' => Token.Colon,
            '\\' => {
                if (self.peek()) |next| {
                    self.i += 1;
                    return Token{ .EscChar = next };
                } else {
                    return Token{ .StringLit = "\\" };
                }
            },
            '=' => self.isNextEqual(Token.Equal, Token.EqualEqual),
            '+' => self.isNextEqual(Token.Plus, Token.PlusEqual),
            '-' => self.isNextEqual(Token.Minus, Token.MinusEqual),
            '*' => self.isNextEqual(Token.Star, Token.StarEqual),
            '/' => {
                if (self.peek() == '/') {
                    self.i += 1;

                    while (self.peek()) |next| {
                        if (next == '\n') return null;
                        self.i += 1;
                    }
                }

                return self.isNextEqual(Token.Slash, Token.SlashEqual);
            },
            '%' => self.isNextEqual(Token.Percent, Token.PercentEqual),
            '!' => self.isNextEqual(Token.Bang, Token.BangEqual),
            '^' => self.isNextEqual(Token.Caret, Token.CaretEqual),
            '|' => {
                if (self.isDouble(c)) {
                    self.i += 1;
                    return Token.DoublePipe;
                } else {
                    return self.isNextEqual(Token.Pipe, Token.PipeEqual);
                }
            },
            '&' => {
                if (self.isDouble(c)) {
                    self.i += 1;
                    return Token.DoubleAmpersand;
                } else {
                    return self.isNextEqual(Token.Ampersand, Token.AmpersandEqual);
                }
            },
            '>' => {
                if (self.isDouble(c)) {
                    self.i += 1;

                    if (self.peek() == '=') {
                        self.i += 1;
                        return Token.DoubleRightCaretEqual;
                    }

                    return Token.DoubleRightCaret;
                } else {
                    return self.isNextEqual(Token.RightCaret, Token.RightCaretEqual);
                }
            },
            '<' => {
                if (self.isDouble(c)) {
                    self.i += 1;

                    if (self.peek() == '=') {
                        self.i += 1;
                        return Token.DoubleLeftCaretEqual;
                    }

                    return Token.DoubleLeftCaret;
                } else {
                    return self.isNextEqual(Token.LeftCaret, Token.LeftCaretEqual);
                }
            },
            'a'...'z', 'A'...'Z' => {
                const str = self.scanIdentifer(c) catch |err| {
                    std.debug.print("{any}", .{err});
                    std.process.exit(2);
                };

                if (self.keywords.contains(str)) {
                    const token = self.keywords.get(str);
                    self.allocator.free(str);
                    return token;
                } else return Token{ .Identifier = str };
            },
            '0'...'9' => {
                return self.scanNumber(c) catch |err| {
                    std.debug.print("{any}", .{err});
                    std.process.exit(2);
                };
            },
            '\"' => { // TODO: I guess i forgot to implement chars too???? im really good at this whole programming thing
                return self.scanStr(c) catch |err| {
                    switch (err) {
                        error.missingClosingQuote => std.debug.print("Missing closing brace", .{}), // TODO: eventually add better error handling (whenever i decide to add an error class yknow)
                        else => std.debug.print("{any}", .{err}),
                    }

                    std.process.exit(2);
                };
            },
            ' ', '\n', '\t', '\r' => null,
            else => Token{ .Unknown = c },
        };
    }

    pub fn tokenize(self: *Tokenizer, input: []const u8) ![]const Token {
        self.input = input;

        while (self.i < self.input.len) {
            if (self.getToken(self.input[self.i])) |token| {
                try self.out.append(token);
            }

            self.i += 1;
        }

        try self.out.append(Token.EOF);
        return self.out.items;
    }

    pub fn deinit(self: *Tokenizer) void {
        for (self.out.items) |token| {
            switch (token) {
                .StringLit, .Identifier => |val| self.allocator.free(val), // hi val!! :3
                else => {},
            }
        }

        self.keywords.deinit();
        self.out.deinit();
    }
};

fn lexerT(input: []const u8, expected: []const Token) !void {
    const allocator = std.testing.allocator;
    var tokenizer = try Tokenizer.init(allocator);
    const tokens = try tokenizer.tokenize(input);

    for (expected, tokens) |expectedT, T| {
        switch (T) {
            .Identifier => |val| try std.testing.expectEqualStrings(val, expectedT.Identifier),
            .StringLit => |val| try std.testing.expectEqualStrings(val, expectedT.StringLit),
            else => try std.testing.expectEqual(expectedT, T),
        }
    }

    tokenizer.deinit();
}

fn debugLexer(input: []const u8) !void {
    const allocator = std.testing.allocator;
    var tokenizer = try Tokenizer.init(allocator);
    const tokens = try tokenizer.tokenize(input);

    for (tokens) |token| {
        std.debug.print("{any} ", .{token});
    }

    std.debug.print("\n\npretty print: \n", .{});
    const debug = @import("debug.zig");
    debug.printTokens(tokens);

    tokenizer.deinit();
}

test "Keywords" {
    try lexerT("let const pub fn if else switch case while do for break continue return true false", &[_]Token{
        Token.Let,
        Token.Const,
        Token.Pub,
        Token.Fn,
        Token.If,
        Token.Else,
        Token.Switch,
        Token.Case,
        Token.While,
        Token.Do,
        Token.For,
        Token.Break,
        Token.Continue,
        Token.Return,
        Token{ .Bool = true },
        Token{ .Bool = false },
        Token.EOF,
    });
}

test "General Syntax" {
    try lexerT("()[]{}.,;:=.*", &[_]Token{
        Token.LParen,
        Token.RParen,
        Token.LBrace,
        Token.RBrace,
        Token.LCurly,
        Token.RCurly,
        Token.Dot,
        Token.Comma,
        Token.Semicolon,
        Token.Colon,
        Token.Equal,
        Token.DotStar,
        Token.EOF,
    });
}

test "Arithmetic" {
    try lexerT("+ - * / % ! & | ^ << >>", &[_]Token{
        Token.Plus,
        Token.Minus,
        Token.Star,
        Token.Slash,
        Token.Percent,
        Token.Bang,
        Token.Ampersand,
        Token.Pipe,
        Token.Caret,
        Token.DoubleLeftCaret,
        Token.DoubleRightCaret,
        Token.EOF,
    });
}

test "Assignment" {
    try lexerT("+= -= *= /= %= != &= |= ^= <<= >>=", &[_]Token{
        Token.PlusEqual,
        Token.MinusEqual,
        Token.StarEqual,
        Token.SlashEqual,
        Token.PercentEqual,
        Token.BangEqual,
        Token.AmpersandEqual,
        Token.PipeEqual,
        Token.CaretEqual,
        Token.DoubleLeftCaretEqual,
        Token.DoubleRightCaretEqual,
        Token.EOF,
    });
}

test "Conditional" {
    try lexerT("== && || < > <= >=", &[_]Token{
        Token.EqualEqual,
        Token.DoubleAmpersand,
        Token.DoublePipe,
        Token.LeftCaret,
        Token.RightCaret,
        Token.LeftCaretEqual,
        Token.RightCaretEqual,
        Token.EOF,
    });
}

test "Strings, Identifiers, and Escaped characters" {
    try lexerT("const foo = \"test string.\";", &[_]Token{
        Token.Const,
        Token{ .Identifier = "foo" },
        Token.Equal,
        Token{ .StringLit = "test string." },
        Token.Semicolon,
        Token.EOF,
    });

    try lexerT("\"abc \n \\\"abc\\\"\"", &[_]Token{
        Token{ .StringLit = "abc \n \"abc\"" },
        Token.EOF,
    });
}
