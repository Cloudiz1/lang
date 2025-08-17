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
        rhs: *AST
    },
    Binary: struct {
        lhs: *AST,
        operator: lexer.Token,
        rhs: *AST
    }
};

pub const Parser = struct {
    input: []const lexer.Token,
    allocator: std.mem.Allocator,
    i: u8,

    pub fn init(allocator: std.mem.Allocator) Parser {
        return .{
            .input = undefined,
            .allocator = allocator,
            .i = 0
        };
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

    // increments then gets
    fn advance(self: *Parser) lexer.Token {
        self.i += 1;
        return self.current();
    }

    // gets then increments
    fn consume(self: *Parser) lexer.Token {
        const out = self.current();
        self.i += 1;
        return out;
    }

    fn matchCurr(self: *Parser, tokens: []const lexer.Token) bool {
        for (tokens) |token| {
            if (std.mem.eql(u8, @tagName(self.current()), @tagName(token))) {
                return true;
            }
        }

        return false;
    }

    fn matchNext(self: *Parser, tokens: []const lexer.Token) bool {
        for (tokens) |token| {
            if (std.mem.eql(u8, @tagName(self.peek()), @tagName(token))) {
                return true;
            }
        }

        return false;
    }

    fn handleAllocError(self: *Parser, e: []const u8) *AST {
        _ = self;
        std.debug.print("{}: Allocation error: {s}", .{error.InternalError, e});
        std.process.abort();
    }

    fn createBinary(self: *Parser, lhs: AST, operator: lexer.Token, rhs: AST) AST {
        const left = self.allocator.create(AST) catch self.handleAllocError("OutOfMemory");
        left.* = lhs;

        const right = self.allocator.create(AST) catch self.handleAllocError("OutOfMemory");
        right.* = rhs;

        return AST{ .Binary = .{
            .lhs = left, 
            .operator = operator, 
            .rhs = right   
        }};
    }

    fn expression(self: *Parser) AST {
        return self.comparison();
    }

    fn comparison(self: *Parser) AST {
        var expr: AST = self.term();

        if (self.matchNext(&[_]lexer.Token {
            lexer.Token.EqualEqual, 
            lexer.Token.BangEqual, 
            lexer.Token.LeftCaret,
            lexer.Token.DoubleLeftCaret,
            lexer.Token.RightCaret,
            lexer.Token.DoubleRightCaret
        })) {
            const operator = self.advance();
            self.i += 1;
            const rhs = self.term();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn term(self: *Parser) AST {
        var expr: AST = self.factor();

        while (self.matchNext(&[_]lexer.Token{lexer.Token.Plus, lexer.Token.Minus})) {
            const operator = self.advance();
            self.i += 1;
            const rhs = self.factor();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn factor(self: *Parser) AST {
        var expr: AST = self.unary();

        while (self.matchNext(&[_]lexer.Token{lexer.Token.Star, lexer.Token.Slash, lexer.Token.Percent})) {
            const operator = self.advance();
            self.i += 1;
            const rhs = self.unary();
            expr = self.createBinary(expr, operator, rhs);
        }

        return expr;
    }

    fn unary(self: *Parser) AST { // TODO add stuff like & (address of) and .* (dereference)
        if (self.matchCurr(&[_]lexer.Token{lexer.Token.Bang, lexer.Token.Minus})) {
            const operator = self.consume();
            
            const rhs = self.allocator.create(AST) catch self.handleAllocError("OutOfMemory");
            rhs.* = self.primary();

            return AST{ .Unary = .{
                .operator = operator, 
                .rhs = rhs 
            }};
        }

        return self.primary();
    }

    fn primary(self: *Parser) AST {
        const next = self.current();
        return switch (next) {
            .IntLit => |n| AST{ .Int = n },
            .FloatLit => |f| AST { .Float = f },
            .Bool => |b| AST { .Bool = b },
            .StringLit => |s| AST { .String = s },

            .LParen => { 
                self.i += 1;
                const expr = self.expression();
                if (self.peek() != lexer.Token.RParen) { // todo definitely need better error handling later
                    std.debug.print("expected closing ')' after expression. \n", .{});
                    std.process.abort();
                }

                self.i += 1;
                return expr;
            },
            else => {
                std.debug.print("expected expression. \n", .{});
                std.process.abort();
            }
        };
    }

    pub fn parse(self: *Parser, tokens: []const lexer.Token) AST {
        self.input = tokens;
        const expressions = self.expression();
        return expressions;
    }
};