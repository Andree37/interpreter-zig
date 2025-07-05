const std = @import("std");

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
            .function => "fn",
            .let => "let",

            else => "ILLEGAL",
        };
    }
};

pub const Token = struct { type: TokenType, literal: []const u8 };

pub fn lookup_ident(ident: []const u8) TokenType {
    if (std.mem.eql(u8, ident, "fn")) return TokenType.function;
    if (std.mem.eql(u8, ident, "let")) return TokenType.let;
    return TokenType.ident;
}
