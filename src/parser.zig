const std = @import("std");

const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");

pub const Parser = struct {
    lexer: *lexer.Lexer,
    cur_token: token.Token,
    peek_token: token.Token,

    pub fn init(lex: *lexer.Lexer) Parser {
        var p = Parser{ .lexer = lex, .cur_token = .{ .type = token.TokenType.eof, .literal = "" }, .peek_token = .{ .type = token.TokenType.eof, .literal = "" } };

        // read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        return p;
    }

    fn next_token(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(_: *Parser, allocator: std.mem.Allocator) ast.Program {
        return ast.Program.init(allocator);
    }
};
