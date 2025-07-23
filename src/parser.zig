const std = @import("std");

const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *lexer.Lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    errors: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, lex: *lexer.Lexer) Parser {
        var p = Parser{
            .allocator = allocator,
            .lexer = lex,
            .cur_token = .{ .type = token.TokenType.eof, .literal = "" },
            .peek_token = .{ .type = token.TokenType.eof, .literal = "" },
            .errors = std.ArrayList([]const u8).init(allocator),
        };

        // read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        return p;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    fn expect_peek(self: *Parser, t: token.TokenType) bool {
        if (self.peek_token.type == t) {
            self.next_token();
            return true;
        }
        self.peek_error(t);
        return false;
    }

    fn peek_error(self: *Parser, t: token.TokenType) void {
        var buf: [128]u8 = undefined; //meh?
        const msg = std.fmt.bufPrint(
            &buf,
            "expected next token to be: {s}, but got: {s} instead",
            .{ @tagName(t), @tagName(self.peek_token.type) },
        ) catch return;

        _ = self.errors.append(msg) catch {};
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
            .allocator = self.allocator,
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

    fn parse_return_statement(self: *Parser) ast.ReturnStatement {
        const return_token = self.cur_token;
        const stmt: ast.ReturnStatement = .{
            .allocator = self.allocator,
            .token = return_token,
            .return_value = .{
                .identifier = .{
                    .token = .{
                        .literal = "",
                        .type = token.TokenType.ident,
                    },
                    .value = "",
                },
            },
        };

        self.next_token();

        //TODO: we're skipping the expressions until we encounter a semicolon
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
                    return .{ .let_stmt = parsed.? };
                }
                return null;
            },
            .kreturn => {
                const parsed = self.parse_return_statement();
                std.debug.print("Parsed return statement: {any}\n", .{parsed});
                return .{
                    .return_stmt = parsed,
                };
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
        \\\let x = 5;
        \\\let y = 10;
        \\\let foobar = 838383;
    ;

    var lex = lexer.Lexer.init(input);
    var p = Parser.init(std.testing.allocator, &lex);
    defer p.deinit();

    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    // check parser errors
    for (p.errors.items) |err| {
        std.debug.print("found bad err: {any}", .{err});
    }

    try std.testing.expect(p.errors.items.len == 0);

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

test "test let bad input" {
    const input =
        \\\let x 5;
        \\\let y = 10;
        \\\let foobar = 838383;
    ;

    var lex = lexer.Lexer.init(input);
    var p = Parser.init(std.testing.allocator, &lex);
    defer p.deinit();

    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    // check parser errors
    for (p.errors.items) |err| {
        // should find one :D
        std.debug.print("found bad err: {any}", .{err});
    }

    try std.testing.expect(p.errors.items.len == 1);
}

test "test return statements" {
    const input =
        \\\return 5;
        \\\return 10;
        \\\return 838383;
    ;

    var lex = lexer.Lexer.init(input);
    var p = Parser.init(std.testing.allocator, &lex);
    defer p.deinit();

    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    try std.testing.expect(p.errors.items.len == 0);

    try std.testing.expect(program.statements.items.len == 3);

    for (program.statements.items) |stmt| {
        var return_stmt = ast.ReturnStatement.init(stmt);
        try std.testing.expectEqualStrings("return", return_stmt.token_literal());
    }
}
