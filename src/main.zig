const std = @import("std");

const repl = @import("repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    // start repl
    try repl.start(@TypeOf(stdin), @TypeOf(stdout), stdin, stdout);
}
