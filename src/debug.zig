const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

pub fn printToken(token: lexer.Token) void {
    switch (token) {
        .StringLit, .Identifier => |str| std.debug.print("{s}: {s}\n", .{@tagName(token), str}),
        .EscChar => |c| std.debug.print("{s}: {c}\n", .{@tagName(token), c}),
        .IntLit => |n| std.debug.print("{s}: {}\n", .{@tagName(token), n}),
        .FloatLit => |n| std.debug.print("{s}: {}\n", .{@tagName(token), n}),
        else => std.debug.print("{s}\n", .{@tagName(token)})
    }
}

pub fn printTokens(tokens: []const lexer.Token) void {
    for (tokens) |token| {
        printToken(token);
    }
}

pub fn pprint(tree: parser.AST) void {
    switch (tree) {
        .Binary => {
            print("(");
            pprintToken(tree.Binary.operator);
            print(" ");
            pprint(tree.Binary.lhs.*);
            print(" ");
            pprint(tree.Binary.rhs.*);
            print(")");
        },
        .Unary => {
            // std.debug.print("{}", .{tree});
            print("(");
            pprintToken(tree.Unary.operator);
            print(" ");
            pprint(tree.Unary.rhs.*);
            print(")");
        },
        .Int => std.debug.print("{}", .{tree.Int}),
        .Float => std.debug.print("{}", .{tree.Float}),
        else => {}
    }
}

// print token helper for pprint
pub fn pprintToken(token: lexer.Token) void {
    switch (token) {
        .Plus => print("+"),
        .Minus => print("-"),
        .Star => print("*"),
        .Slash => print("/"),
        .Percent => print("%"),
        .LeftCaret => print("<"),
        .LeftCaretEqual => print("<="),
        .RightCaret => print(">"),
        .RightCaretEqual => print(">="),
        .EqualEqual => print("=="),
        .BangEqual => print("!="),
        .Bang => print("!"),
        .DoubleLeftCaret => print("<<"),
        .DoubleRightCaret => print(">>"),
        .Ampersand => print("&"),
        .Pipe => print("|"),
        .Caret => print("^"),
        .DoubleAmpersand => print("&&"),
        .DoublePipe => print("||"),
        else => {}
    }
}

// just a shorthand used in pprint lol
fn print(str: []const u8) void {
    std.debug.print("{s}", .{str});
}