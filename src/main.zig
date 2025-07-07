const std = @import("std");

const repl = @import("repl.zig");

pub fn main() !void {
    // start repl
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try repl.start(@TypeOf(stdin), @TypeOf(stdout), stdin, stdout);
}
