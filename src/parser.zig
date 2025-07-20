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

    fn expect_peek(self: *Parser, t: token.TokenType) bool {
        if (self.peek_token.type == t) {
            self.next_token();
            return true;
        }
        return false;
    }

    fn next_token(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.next_token();
    }

    fn parse_let_statement(self: *Parser) ?ast.LetStatement {
        const let_token = self.cur_token;
        if (!self.expect_peek(token.TokenType.ident)) {
            return null;
        }

        const ident_token = self.cur_token;
        if (!self.expect_peek(token.TokenType.assign)) {
            return null;
        }

        const stmt: ast.LetStatement = .{
            .token = let_token,
            .name = .{
                .token = ident_token,
                .value = ident_token.literal,
            },
            .value = .{
                .identifier = .{ .token = .{ .type = token.TokenType.ident, .literal = "" }, .value = "" },
            },
        };

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        while (!(self.cur_token.type == token.TokenType.semicolon)) {
            self.next_token();
        }

        return stmt;
    }

    fn parse_statement(self: *Parser) ?ast.Statement {
        switch (self.cur_token.type) {
            .let => {
                const parsed = self.parse_let_statement();
                std.debug.print("Parsed let statement: {any}\n", .{parsed});
                if (parsed != null) {
                    return .{ .let = parsed.? };
                }
                return null;
            },
            else => return null,
        }
    }

    pub fn parse_program(self: *Parser, allocator: std.mem.Allocator) !ast.Program {
        var program = ast.Program.init(allocator);

        while (self.cur_token.type != token.TokenType.eof) {
            const stmt = self.parse_statement();
            std.debug.print("Found stmt: {any}\n", .{stmt});
            if (stmt != null) {
                try program.statements.append(stmt.?);
            }
            self.next_token();
        }

        return program;
    }
};

test "test let statement" {
    const input =
        \\\let x =5;
        \\\let y = 10;
        \\\let foobar = 838383;
    ;

    var lex = lexer.Lexer.init(input);
    var p = Parser.init(&lex);

    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    std.debug.print("Expected statements 3; found: {any}\n", .{program.statements.items.len});
    try std.testing.expect(program.statements.items.len == 3);

    const expectedIdentifiers = [_][]const u8{ "x", "y", "foobar" };
    for (expectedIdentifiers, 0..) |ei, i| {
        var stmt = program.statements.items[i];
        try std.testing.expectEqualStrings("let", stmt.token_literal());

        var letStmt = ast.LetStatement.init(stmt);

        try std.testing.expectEqualStrings(ei, letStmt.name.value);

        try std.testing.expectEqualStrings(ei, letStmt.name.token_literal());
    }
}
