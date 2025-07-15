const std = @import("std");

const repl = @import("repl.zig");
const parser = @import("parser.zig");
const lexer = @import("lexer.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    // start repl
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var lex = lexer.Lexer.init("potato");
    var par = parser.Parser.init(&lex);
    _ = par.parse_program(allocator);

    try repl.start(@TypeOf(stdin), @TypeOf(stdout), stdin, stdout);
}
