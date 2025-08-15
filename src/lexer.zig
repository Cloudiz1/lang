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
    Mul,
    Div, 
    Mod, 

    PlusEq,
    MinusEq,
    MulEq,
    DivEq,
    ModEq,

    // Bitwise
    BNot, // !
    BAnd, // &
    BOr, // |
    BXor, // ^
    BLShift, // <<
    BRShift, // >>

    BNotEq, // !=
    BAndEq, // &=
    BOrEq, // |=
    BXorEq, // ^=
    BLShiftEq, // <<=
    BRShiftEq, // >>=

    // Conditional
    IfEqual, // ==
    IfAnd, // &&
    IfOr, // ||
    IfLesser, // <
    IfGreater, // >
    IfLesserEqual, // <=
    IfGreaterEqual, // >=

    StringLit: []const u8,
    Identifier: []const u8,
    IntLit: []const u8,
    FloatLit: []const u8,
    Bool: bool,
    EscChar: u8,

    // special characters
    Null,
    Newline,
    Tab,

    Unknown: u8
};

pub const Tokenizer = struct {
    i: u8,
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

        return .{ 
            .i = 0, 
            .input = "", 
            .out = std.ArrayList(Token).init(allocator), 
            .keywords = map, 
            .allocator = allocator 
        };
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
                else => break
            }
        }

        return try buffer.toOwnedSlice();
    }

    fn scanNumber(self: *Tokenizer, c: u8) !Token {
        var float: bool = false;
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        try buffer.append(c);

        while (self.peek()) |next| {
            switch (next) {
                '0'...'9' => try buffer.append(next),
                '.' => {
                    try buffer.append(next);
                    float = true;
                },
                else => break
            }

            self.i += 1;
        }

        const val = try buffer.toOwnedSlice(); // hi val :3
        if (float) {
            return Token{ .FloatLit = val };
        } else {
            return Token{ .IntLit = val };
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
                        else => try buffer.append(c) // todo report error here instead of blinding appending the next character
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
        return switch(c) {
            '(' => Token.LParen,
            ')' => Token.RParen,
            '[' => Token.LBrace,
            ']' => Token.RBrace,
            '{' => Token.LCurly,
            '}' => Token.RCurly,
            '.' => Token.Dot,
            ',' => Token.Comma,
            '\\' => {
                if (self.peek()) |next| {
                    self.i += 1;
                    return Token{ .EscChar = next };
                } else {
                    return Token{ .StringLit = "\\" };
                }
            },
            '=' => self.isNextEqual(Token.Equal, Token.IfEqual),
            '+' => self.isNextEqual(Token.Plus, Token.PlusEq),
            '-' => self.isNextEqual(Token.Minus, Token.MinusEq),
            '*' => self.isNextEqual(Token.Mul, Token.MulEq),
            '/' => {
                if (self.peek() == '/') {
                    self.i += 1;

                    while (self.peek()) |next| {
                        if (next == '\n') return null;
                        self.i += 1;
                    }
                }

                return self.isNextEqual(Token.Div, Token.DivEq);
            },
            '%' => self.isNextEqual(Token.Mod, Token.ModEq),
            '!' => self.isNextEqual(Token.BNot, Token.BNotEq),
            '^' => self.isNextEqual(Token.BXor, Token.BXorEq),
            '|' => {
                if (self.isDouble(c)) {
                    self.i += 1;
                    return Token.IfOr;
                } else {
                    return self.isNextEqual(Token.BOr, Token.BOrEq);
                }
            },
            '&' => {
                if (self.isDouble(c)) {
                    self.i += 1;
                    return Token.IfAnd;
                } else {
                    return self.isNextEqual(Token.BAnd, Token.BAndEq);
                }
            },
            '>' => {
                if (self.isDouble(c)) {
                    self.i += 1;

                    if (self.peek() == '=') {
                        self.i += 1;
                        return Token.BRShiftEq;
                    }

                    return Token.BRShift;
                } else {
                    return self.isNextEqual(Token.IfGreater, Token.IfGreaterEqual);
                }
            },
            '<' => {
                if (self.isDouble(c)) {
                    self.i += 1;

                    if (self.peek() == '=') {
                        self.i += 1;
                        return Token.BLShiftEq;
                    }
                    
                    return Token.BLShift;
                } else {
                    return self.isNextEqual(Token.IfLesser, Token.IfLesserEqual);
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
                } else return Token { .Identifier = str };
            },
            '0'...'9' => {
                return self.scanNumber(c) catch |err| {
                    std.debug.print("{any}", .{err});
                    std.process.exit(2);
                };
            },
            '\'', '\"' => {
                return self.scanStr(c) catch |err| {
                    switch (err) {
                        error.missingClosingQuote => std.debug.print("Missing closing brace", .{}), // TODO eventually add better error handling (whenever i decide to add an error class yknow)
                        else => std.debug.print("{any}", .{err})
                    }

                    std.process.exit(2);
                };
            },
            ' ', '\n', '\t', '\r' => null,
            else => Token{ .Unknown = c }
        };
    }

    pub fn tokenize(self: *Tokenizer, input: []const u8) ![]const Token {
        self.input = input;

        while(self.i < self.input.len) {
            if (self.getToken(self.input[self.i])) |token| {
                try self.out.append(token);
            }

            self.i += 1;
        }

        return self.out.items;
    }
};