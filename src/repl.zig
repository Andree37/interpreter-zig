const std = @import("std");

const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");
const object = @import("object.zig");

const PROMPT = ">> ";

pub fn start(
    reader: anytype,
    writer: anytype,
) !void {
    var buf_reader = std.io.bufferedReader(reader);
    const buffered = buf_reader.reader();

    const allocator = std.heap.page_allocator;

    var env = try object.Environment.init(allocator);
    defer env.deinit();

    var input_lines = std.ArrayList([]u8).init(allocator);
    defer {
        for (input_lines.items) |line| {
            allocator.free(line);
        }
        input_lines.deinit();
    }

    while (true) {
        try writer.writeAll(PROMPT);

        var line_buf = std.ArrayList(u8).init(allocator);

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

        const line = try allocator.dupe(u8, line_buf.items);
        line_buf.deinit(); // Free the temporary buffer

        if (line.len == 0) {
            allocator.free(line);
            continue;
        }

        try input_lines.append(line);

        var lex = lexer.Lexer.init(line);
        var p = try parser.Parser.init(allocator, &lex);
        var program = try p.parse_program();
        if (p.errors.items.len != 0) {
            try print_parser_errors(writer, p.errors.items, allocator);
            continue;
        }

        var evaluated = evaluator.eval_program(&program, allocator, env);
        var output: []const u8 = "ERROR Something in the input I cannot parse...\n";
        if (evaluated != null) {
            const evaluated_str = try evaluated.?.inspect(allocator);
            output = try std.fmt.allocPrint(allocator, "{s}\n", .{evaluated_str});
        }
        try writer.writeAll(output);
    }
}

fn print_parser_errors(writer: anytype, errors: [][]const u8, allocator: std.mem.Allocator) !void {
    for (errors) |err| {
        const formattedErr = try std.fmt.allocPrint(allocator, "\t{s}\n", .{err});
        try writer.writeAll(formattedErr);
    }
}
