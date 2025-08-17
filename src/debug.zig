const std = @import("std");
const lexer = @import("lexer.zig");
const AST = @import("AST.zig");

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

pub const pprint = struct {
    i: u64,

    pub fn init() pprint {
        return .{
            .i = 0,  
        };
    }

    fn printToken(self: *pprint, token: lexer.Token) void {
        _ = self;
        switch (token) {
            .Plus => std.debug.print("+", .{}),
            .Minus => std.debug.print("-", .{}),
            .Star => std.debug.print("*", .{}),
            .Slash => std.debug.print("/", .{}),
            .Percent => std.debug.print("%", .{}),
            else => {}
        }
    }

    fn cprint(self: *pprint, c: u8) void {
        _ = self;
        std.debug.print("{c}", .{c});
    }

    pub fn print(self: *pprint, tree: AST.AST) void {
        switch (tree) {
            .Binary => {
                self.cprint('(');
                self.printToken(tree.Binary.operator);
                self.cprint(' ');
                self.print(tree.Binary.lhs.*);
                self.cprint(' ');
                self.print(tree.Binary.rhs.*);
                self.cprint(')');
            },
            .Int => std.debug.print("{}", .{tree.Int}),
            .Float => std.debug.print("{}", .{tree.Float}),
            else => {}
        }
    }
};