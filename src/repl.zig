const std = @import("std");

const lexer = @import("lexer.zig");
const token = @import("token.zig");

const PROMPT = ">> ";

pub fn start(
    comptime ReaderType: type,
    comptime WriterType: type,
    reader: ReaderType,
    writer: WriterType,
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

        while (true) {
            const tok = lex.next_token();
            if (tok.type == token.TokenType.eof) break;

            try writer.writeAll(std.fmt.allocPrint(allocator, "{any} {s}\n", .{ tok.type, tok.literal }) catch return error.OutOfMemory);
        }
    }
}
