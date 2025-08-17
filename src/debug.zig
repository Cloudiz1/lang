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
    input: AST.AST,
    // out: [8192]u8,

    pub fn init() pprint {
        return .{
            .i = 0,  
            // .out = undefined
        };
    }

    // fn current(self: *pprint) AST.AST {
    //     return self.input[self.i];
    // }

    pub fn print(self: *pprint, tree: AST.AST) void {
        _ = self;
        switch (tree) {
            else => {}
        }
    }
};


// fn stringifyAst(tree: AST.AST, buffer: *[]const u8) void {
    
// }

// pub fn printAST(tree: AST.AST) void {
//     const buffer: [8192]u8 = undefined;

// }