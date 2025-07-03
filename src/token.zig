pub const TokenType = enum {
    illegal,
    eof,

    // identifiers + literals
    ident,
    int,

    // operators
    assign,
    plus,

    // delimiters
    comma,
    semicolon,

    l_paren,
    r_paren,
    l_brace,
    r_brace,

    // keywords
    function,
    let,

    pub fn name(self: TokenType) []const u8 {
        return switch (self) {
            .eof => "EOF",
            .assign => "=",
            .plus => "+",
            .comma => ",",
            .semicolon => ";",
            .l_paren => "(",
            .r_paren => ")",
            .l_brace => "[",
            .r_brace => "]",
            .function => "FUNCTION",
            .let => "LET",

            else => "ILLEGAL",
        };
    }
};

pub const Token = struct { type: TokenType, literal: []const u8 };
