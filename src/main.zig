const std = @import("std");
const stdout = std.io.getStdOut().writer();

const lexer = @import("lexer.zig");
const debug = @import("debug.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("test.txt", .{});
    defer file.close();

    const buffer = try allocator.alloc(u8, try file.getEndPos());
    _ = try file.readAll(buffer);

    var tokenizer = try lexer.Tokenizer.init(allocator);
    const tokens = try tokenizer.tokenize(buffer);
    // _ = tokens;
    // std.debug.print("{any}", .{tokens.items});
    debug.printTokens(tokens);
}