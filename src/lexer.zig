const std = @import("std");
const token = @import("token.zig");

const Lexer = struct {
    input: []const u8,
    position: u32,
    read_position: u32,
    ch: u8,

    fn new(input: []const u8) Lexer {
        var l: Lexer = .{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = '0',
        };
        l.read_char();
        return l;
    }

    fn read_char(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = '0';
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(self: *Lexer) token.Token {
        var tok: token.Token = .{
            .type = token.TokenType.illegal,
            .literal = '0',
        };

        switch (self.ch) {
            '=' => tok = .{ .type = token.TokenType.assign, .literal = self.ch },
            ';' => tok = .{ .type = token.TokenType.semicolon, .literal = self.ch },
            '(' => tok = .{ .type = token.TokenType.l_paren, .literal = self.ch },
            ')' => tok = .{ .type = token.TokenType.r_paren, .literal = self.ch },
            '{' => tok = .{ .type = token.TokenType.l_brace, .literal = self.ch },
            '}' => tok = .{ .type = token.TokenType.r_brace, .literal = self.ch },
            ',' => tok = .{ .type = token.TokenType.comma, .literal = self.ch },
            '+' => tok = .{ .type = token.TokenType.plus, .literal = self.ch },
            else => tok = .{ .type = token.TokenType.eof, .literal = '0' },
        }

        self.read_char();
        return tok;
    }
};

test "test next token" {
    const input = "=+(){},;";

    var tests = std.ArrayList(token.Token).init(std.testing.allocator);
    defer tests.deinit();

    try tests.append(.{ .type = token.TokenType.assign, .literal = '=' });
    try tests.append(.{ .type = token.TokenType.plus, .literal = '+' });
    try tests.append(.{ .type = token.TokenType.l_paren, .literal = '(' });
    try tests.append(.{ .type = token.TokenType.r_paren, .literal = ')' });
    try tests.append(.{ .type = token.TokenType.l_brace, .literal = '{' });
    try tests.append(.{ .type = token.TokenType.r_brace, .literal = '}' });
    try tests.append(.{ .type = token.TokenType.comma, .literal = ',' });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ';' });
    try tests.append(.{ .type = token.TokenType.eof, .literal = '0' });

    var l = Lexer.new(input);

    for (tests.items) |t| {
        const tok = l.next_token();

        try std.testing.expectEqual(t.type, tok.type);
        try std.testing.expectEqual(t.literal, tok.literal);
    }
}
