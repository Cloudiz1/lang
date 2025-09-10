const std = @import("std");
const lexer = @import("lexer.zig");
const debug = @import("debug.zig");
// const ast = @import("ast.zig");

pub const AST = union(enum) {
    Int: i64,
    Float: f64,
    String: []const u8,
    Char: u8,
    Bool: bool,
    Unary: struct {
        operator: lexer.Token,
        rhs: *AST,
    },
    Postfix: struct {
        lhs: *AST,
        operator: lexer.Token,
    },
    Binary: struct {
        lhs: *AST,
        operator: lexer.Token,
        rhs: *AST,
    },
};

pub const Parser = struct {
    input: []const lexer.Token,
    allocator: std.mem.Allocator,
    i: u8,

    pub fn init(allocator: std.mem.Allocator) Parser {
        return .{ .input = undefined, .allocator = allocator, .i = 0 };
    }

    fn previous(self: *Parser) lexer.Token {
        if (self.i == 0) {
            std.debug.print("{}: Out of bounds access from parser (called previous() on index 0)", .{error.InternalError});
            std.process.abort();
        }

        return self.input[self.i - 1];
    }

    fn current(self: *Parser) lexer.Token {
        if (self.i >= self.input.len) {
            std.debug.print("{}: Out of bounds access from parser. \n", .{error.InternalError});
            std.process.abort();
        }

        return self.input[self.i];
    }

    fn peek(self: *Parser) lexer.Token {
        if (self.current() == lexer.Token.EOF) {
            std.debug.print("{}: Out of bounds peek from parser. \n", .{error.InternalError});
            std.process.abort();
        }

        return self.input[self.i + 1];
    }

    fn match(self: *Parser, tokens: []const lexer.Token) bool {
        for (tokens) |token| {
            if (std.mem.eql(u8, @tagName(self.current()), @tagName(token))) {
                return true;
            }
        }

        return false;
    }

    fn matchAdvance(self: *Parser, tokens: []const lexer.Token) bool {
        const out = self.match(tokens);
        if (out) self.i += 1;
        return out;
    }

    fn handleAllocError(self: *Parser, e: []const u8) *AST {
        _ = self;
        std.debug.print("{}: Allocation error: {s}", .{ error.InternalError, e });
        std.process.abort();
    }

    fn createBinary(self: *Parser, lhs: AST, operator: lexer.Token, rhs: AST) AST {
        const left = self.allocator.create(AST) catch self.handleAllocError("OutOfMemory");
        left.* = lhs;

        const right = self.allocator.create(AST) catch self.handleAllocError("OutOfMemory");
        right.* = rhs;

        return AST{ .Binary = .{ .lhs = left, .operator = operator, .rhs = right } };
    }

    fn expression(self: *Parser) AST {
        return self.logicalOR();
    }

    fn logicalOR(self: *Parser) AST {
        var expr: AST = self.logicalAND();

        while (self.matchAdvance(&[_]lexer.Token{lexer.Token.DoublePipe})) {
            const operator = self.previous();
            const rhs = self.logicalAND();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn logicalAND(self: *Parser) AST {
        var expr: AST = self.comparison();

        while (self.matchAdvance(&[_]lexer.Token{lexer.Token.DoubleAmpersand})) {
            const operator = self.previous();
            const rhs = self.comparison();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn comparison(self: *Parser) AST {
        var expr: AST = self.bitwise();

        if (self.matchAdvance(&[_]lexer.Token{ lexer.Token.EqualEqual, lexer.Token.BangEqual, lexer.Token.LeftCaret, lexer.Token.LeftCaretEqual, lexer.Token.RightCaret, lexer.Token.RightCaretEqual })) {
            const operator = self.previous();
            const rhs = self.bitwise();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn bitwise(self: *Parser) AST {
        var expr: AST = self.bitshift();

        while (self.matchAdvance(&[_]lexer.Token{ lexer.Token.Ampersand, lexer.Token.Pipe, lexer.Token.Caret })) {
            const operator = self.previous();
            const rhs = self.bitshift();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn bitshift(self: *Parser) AST {
        var expr: AST = self.term();

        while (self.matchAdvance(&[_]lexer.Token{
            lexer.Token.DoubleLeftCaret,
            lexer.Token.DoubleRightCaret,
        })) {
            const operator = self.previous();
            const rhs = self.term();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn term(self: *Parser) AST {
        var expr: AST = self.factor();

        while (self.matchAdvance(&[_]lexer.Token{ lexer.Token.Plus, lexer.Token.Minus })) {
            const operator = self.previous();
            const rhs = self.factor();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn factor(self: *Parser) AST {
        var expr: AST = self.unary();

        while (self.matchAdvance(&[_]lexer.Token{ lexer.Token.Star, lexer.Token.Slash, lexer.Token.Percent })) {
            const operator = self.previous();
            const rhs = self.unary();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn unary(self: *Parser) AST { // TODO: add stuff like & (address of) and .* (dereference)
        if (self.matchAdvance(&[_]lexer.Token{ lexer.Token.Bang, lexer.Token.Minus, lexer.Token.Ampersand })) {
            const operator = self.previous();

            const rhs = self.allocator.create(AST) catch self.handleAllocError("OutOfMemory");
            rhs.* = self.unary();

            return AST{ .Unary = .{ .operator = operator, .rhs = rhs } };
        }

        return self.postfix();
    }

    fn postfix(self: *Parser) AST {
        var expr = self.primary();

        if (self.match(&[_]lexer.Token{
            lexer.Token.Dot,
            lexer.Token.DotStar,
            lexer.Token.LBrace,
            lexer.Token.LParen,
        })) {
            const token = self.current();
            switch (token) {
                .DotStar => {
                    const lhs = self.allocator.create(AST) catch self.handleAllocError("OutOfMemory");
                    lhs.* = expr;

                    expr = AST{ .Postfix = .{ .operator = token, .lhs = lhs } };
                },
                else => |a| std.debug.print("{} {}", .{ a, expr }),
            }
        }

        return expr;
    }

    fn primary(self: *Parser) AST {
        const token = self.current();
        self.i += 1;
        return switch (token) {
            .IntLit => |n| AST{ .Int = n },
            .FloatLit => |f| AST{ .Float = f },
            .Bool => |b| AST{ .Bool = b },
            .StringLit, .Identifier => |s| AST{ .String = s },

            .LParen => {
                const expr = self.expression();
                if (self.current() != lexer.Token.RParen) { // todo definitely need better error handling later
                    std.debug.print("expected closing ')' after expression. \n", .{});
                    std.process.abort();
                }

                self.i += 1; // ignore RParen
                return expr;
            },
            else => {
                std.debug.print("expected expression. \n", .{});
                std.process.abort();
            },
        };
    }

    pub fn parse(self: *Parser, tokens: []const lexer.Token) AST {
        self.input = tokens;
        const expressions = self.expression();
        return expressions;
    }
};

