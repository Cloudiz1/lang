const std = @import("std");
const lexer = @import("lexer.zig");

pub fn printToken(token: lexer.Token) !void {
    switch (token) {
        .StringLit, .Identifier => |str| std.debug.print("{s}: {s}\n", .{@tagName(token), str}),
        .EscChar => |c| std.debug.print("{s}: {c}\n", .{@tagName(token), c}),
        .IntLit => |n| std.debug.print("{s}: {}\n", .{@tagName(token), try std.fmt.parseInt(u64, n, 10)}),
        .FloatLit => |n| std.debug.print("{s}: {}\n", .{@tagName(token), try std.fmt.parseFloat(f128, n)}),
        else => std.debug.print("{s}\n", .{@tagName(token)})
    }
}

pub fn printTokens(tokens: []const lexer.Token) void {
    for (tokens) |token| {
        printToken(token) catch std.process.exit(1);
    }
}