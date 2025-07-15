const std = @import("std");
const token = @import("token.zig");

pub const Lexer = struct {
    input: []const u8,
    position: u32,
    read_position: u32,
    ch: u8,

    pub fn init(input: []const u8) Lexer {
        var l: Lexer = .{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = 0,
        };
        l.read_char();
        return l;
    }

    fn read_char(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_number(self: *Lexer) []const u8 {
        const position = self.position;
        while (is_number(self.ch)) {
            self.read_char();
        }
        return self.input[position..self.position];
    }

    fn read_identifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (is_letter(self.ch)) {
            self.read_char();
        }
        return self.input[position..self.position];
    }

    fn peek_char(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    pub fn next_token(self: *Lexer) token.Token {
        var tok: token.Token = .{
            .type = token.TokenType.illegal,
            .literal = "",
        };

        self.skip_whitespaces();

        const lit: []const u8 = if (self.position < self.input.len) self.input[self.position .. self.position + 1] else "";

        switch (self.ch) {
            '=' => {
                if (self.peek_char() == '=') {
                    self.read_char();
                    tok = .{ .type = token.TokenType.eq, .literal = "==" };
                } else {
                    tok = .{ .type = token.TokenType.assign, .literal = lit };
                }
            },
            ';' => tok = .{ .type = token.TokenType.semicolon, .literal = lit },
            '(' => tok = .{ .type = token.TokenType.l_paren, .literal = lit },
            ')' => tok = .{ .type = token.TokenType.r_paren, .literal = lit },
            '{' => tok = .{ .type = token.TokenType.l_brace, .literal = lit },
            '}' => tok = .{ .type = token.TokenType.r_brace, .literal = lit },
            ',' => tok = .{ .type = token.TokenType.comma, .literal = lit },
            '+' => tok = .{ .type = token.TokenType.plus, .literal = lit },
            '-' => tok = .{ .type = token.TokenType.minus, .literal = lit },
            '!' => {
                if (self.peek_char() == '=') {
                    self.read_char();
                    tok = .{ .type = token.TokenType.not_eq, .literal = "!=" };
                } else {
                    tok = .{ .type = token.TokenType.bang, .literal = lit };
                }
            },
            '/' => tok = .{ .type = token.TokenType.slash, .literal = lit },
            '*' => tok = .{ .type = token.TokenType.asterisk, .literal = lit },
            '<' => tok = .{ .type = token.TokenType.lt, .literal = lit },
            '>' => tok = .{ .type = token.TokenType.gt, .literal = lit },
            0 => tok = .{ .type = token.TokenType.eof, .literal = "" },
            'a'...'z', 'A'...'Z', '_' => {
                tok.literal = self.read_identifier();
                tok.type = token.lookup_ident(tok.literal);
                return tok;
            },
            '0'...'9' => {
                tok.type = token.TokenType.int;
                tok.literal = self.read_number();
                return tok;
            },
            else => tok = .{ .type = token.TokenType.illegal, .literal = lit },
        }

        self.read_char();
        return tok;
    }

    fn skip_whitespaces(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            self.read_char();
        }
    }
};

fn is_letter(ch: u8) bool {
    return 'a' <= ch and ch <= 'z' or 'A' <= ch and ch <= 'Z' or ch == '_';
}

fn is_number(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

test "test next token" {
    const input = "=+(){},;";

    var tests = std.ArrayList(token.Token).init(std.testing.allocator);
    defer tests.deinit();

    try tests.append(.{ .type = token.TokenType.assign, .literal = "=" });
    try tests.append(.{ .type = token.TokenType.plus, .literal = "+" });
    try tests.append(.{ .type = token.TokenType.l_paren, .literal = "(" });
    try tests.append(.{ .type = token.TokenType.r_paren, .literal = ")" });
    try tests.append(.{ .type = token.TokenType.l_brace, .literal = "{" });
    try tests.append(.{ .type = token.TokenType.r_brace, .literal = "}" });
    try tests.append(.{ .type = token.TokenType.comma, .literal = "," });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.eof, .literal = "" });

    var l = Lexer.init(input);

    for (tests.items) |t| {
        const tok = l.next_token();

        try std.testing.expectEqual(t.type, tok.type);
        try std.testing.expect(std.mem.eql(u8, t.literal, tok.literal));
    }
}

test "some monkey code" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x,y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five,ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
    ;

    var tests = std.ArrayList(token.Token).init(std.testing.allocator);
    defer tests.deinit();

    try tests.append(.{ .type = token.TokenType.let, .literal = "let" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "five" });
    try tests.append(.{ .type = token.TokenType.assign, .literal = "=" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "5" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.let, .literal = "let" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "ten" });
    try tests.append(.{ .type = token.TokenType.assign, .literal = "=" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "10" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.let, .literal = "let" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "add" });
    try tests.append(.{ .type = token.TokenType.assign, .literal = "=" });
    try tests.append(.{ .type = token.TokenType.function, .literal = "fn" });
    try tests.append(.{ .type = token.TokenType.l_paren, .literal = "(" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "x" });
    try tests.append(.{ .type = token.TokenType.comma, .literal = "," });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "y" });
    try tests.append(.{ .type = token.TokenType.r_paren, .literal = ")" });
    try tests.append(.{ .type = token.TokenType.l_brace, .literal = "{" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "x" });
    try tests.append(.{ .type = token.TokenType.plus, .literal = "+" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "y" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.r_brace, .literal = "}" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.let, .literal = "let" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "result" });
    try tests.append(.{ .type = token.TokenType.assign, .literal = "=" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "add" });
    try tests.append(.{ .type = token.TokenType.l_paren, .literal = "(" });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "five" });
    try tests.append(.{ .type = token.TokenType.comma, .literal = "," });
    try tests.append(.{ .type = token.TokenType.ident, .literal = "ten" });
    try tests.append(.{ .type = token.TokenType.r_paren, .literal = ")" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.bang, .literal = "!" });
    try tests.append(.{ .type = token.TokenType.minus, .literal = "-" });
    try tests.append(.{ .type = token.TokenType.slash, .literal = "/" });
    try tests.append(.{ .type = token.TokenType.asterisk, .literal = "*" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "5" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "5" });
    try tests.append(.{ .type = token.TokenType.lt, .literal = "<" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "10" });
    try tests.append(.{ .type = token.TokenType.gt, .literal = ">" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "5" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.kif, .literal = "if" });
    try tests.append(.{ .type = token.TokenType.l_paren, .literal = "(" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "5" });
    try tests.append(.{ .type = token.TokenType.lt, .literal = "<" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "10" });
    try tests.append(.{ .type = token.TokenType.r_paren, .literal = ")" });
    try tests.append(.{ .type = token.TokenType.l_brace, .literal = "{" });
    try tests.append(.{ .type = token.TokenType.kreturn, .literal = "return" });
    try tests.append(.{ .type = token.TokenType.true, .literal = "true" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.r_brace, .literal = "}" });
    try tests.append(.{ .type = token.TokenType.kelse, .literal = "else" });
    try tests.append(.{ .type = token.TokenType.l_brace, .literal = "{" });
    try tests.append(.{ .type = token.TokenType.kreturn, .literal = "return" });
    try tests.append(.{ .type = token.TokenType.false, .literal = "false" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.r_brace, .literal = "}" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "10" });
    try tests.append(.{ .type = token.TokenType.eq, .literal = "==" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "10" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "10" });
    try tests.append(.{ .type = token.TokenType.not_eq, .literal = "!=" });
    try tests.append(.{ .type = token.TokenType.int, .literal = "9" });
    try tests.append(.{ .type = token.TokenType.semicolon, .literal = ";" });
    try tests.append(.{ .type = token.TokenType.eof, .literal = "" });

    var l = Lexer.init(input);

    for (tests.items) |t| {
        const tok = l.next_token();

        // std.debug.print("Expected: {s}, got: {s}\n", .{ t.literal, tok.literal });
        // std.debug.print("Expected: {any}, got: {any}\n", .{ t.type, tok.type });

        try std.testing.expectEqual(t.type, tok.type);
        try std.testing.expect(std.mem.eql(u8, t.literal, tok.literal));
    }
}
