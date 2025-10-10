const std = @import("std");

pub const TokenType = enum {
    illegal,
    eof,

    // identifiers + literals
    ident,
    int,
    str,

    // operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,

    lt,
    gt,
    eq,
    not_eq,

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
    true,
    false,
    kif,
    kelse,
    kreturn,
};

pub const Token = struct { type: TokenType, literal: []const u8 };

pub fn lookup_ident(ident: []const u8) TokenType {
    if (std.mem.eql(u8, ident, "fn")) return TokenType.function;
    if (std.mem.eql(u8, ident, "let")) return TokenType.let;
    if (std.mem.eql(u8, ident, "true")) return TokenType.true;
    if (std.mem.eql(u8, ident, "false")) return TokenType.false;
    if (std.mem.eql(u8, ident, "if")) return TokenType.kif;
    if (std.mem.eql(u8, ident, "else")) return TokenType.kelse;
    if (std.mem.eql(u8, ident, "return")) return TokenType.kreturn;
    return TokenType.ident;
}
