const std = @import("std");

const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");

const PROMPT = ">> ";

pub fn start(
    reader: anytype,
    writer: anytype,
) !void {
    var buf_reader = std.io.bufferedReader(reader);
    const buffered = buf_reader.reader();

    const allocator = std.heap.page_allocator;

    while (true) {
        try writer.writeAll(PROMPT);

        var line_buf = std.ArrayList(u8).init(allocator);
        defer line_buf.deinit();

        // Read line until newline or EOF
        while (true) {
            const read_result = buffered.readByte();

            if (read_result) |b| {
                if (b == '\n') break;
                try line_buf.append(b);
            } else |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            }
        }

        const line = line_buf.items;
        if (line.len == 0) continue;

        var lex = lexer.Lexer.init(line);
        var p = try parser.Parser.init(allocator, &lex);
        var program = try p.parse_program();
        if (p.errors.items.len != 0) {
            try print_parser_errors(writer, p.errors.items, allocator);
            continue;
        }

        const program_str = try program.string();
        const output = try std.fmt.allocPrint(allocator, "{s}\n", .{program_str});
        try writer.writeAll(output);
    }
}

fn print_parser_errors(writer: anytype, errors: [][]const u8, allocator: std.mem.Allocator) !void {
    for (errors) |err| {
        const formattedErr = try std.fmt.allocPrint(allocator, "\t{s}\n", .{err});
        try writer.writeAll(formattedErr);
    }
}
