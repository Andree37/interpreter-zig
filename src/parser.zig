const std = @import("std");

const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");

const prefixParseFn = *const fn (*Parser) ast.Expression;
const infixParseFn = *const fn (*Parser, ast.Expression) ast.Expression;

pub const ExprOrder = enum {
    lowest,
    equals, // ==
    lessgreater, // > or <
    sum, // +
    product, // *
    prefix, // -X or !X
    call, // my_function(X)
};

pub const Parser = struct {
    allocator: std.mem.Allocator,

    lexer: *lexer.Lexer,
    errors: std.ArrayList([]const u8),

    cur_token: token.Token,
    peek_token: token.Token,

    prefix_parse_fns: std.AutoHashMap(token.TokenType, prefixParseFn),
    infix_parse_fns: std.AutoHashMap(token.TokenType, infixParseFn),

    pub fn init(allocator: std.mem.Allocator, lex: *lexer.Lexer) !Parser {
        var p = Parser{
            .allocator = allocator,
            .lexer = lex,
            .cur_token = .{ .type = token.TokenType.eof, .literal = "" },
            .peek_token = .{ .type = token.TokenType.eof, .literal = "" },
            .errors = std.ArrayList([]const u8).init(allocator),
            .prefix_parse_fns = std.AutoHashMap(token.TokenType, prefixParseFn).init(allocator),
            .infix_parse_fns = std.AutoHashMap(token.TokenType, infixParseFn).init(allocator),
        };

        try p.register_prefix(token.TokenType.ident, parse_identifier);
        try p.register_prefix(token.TokenType.int, parse_integer_literal);
        try p.register_prefix(token.TokenType.bang, parse_prefix_expression);
        try p.register_prefix(token.TokenType.minus, parse_prefix_expression);

        // read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        return p;
    }

    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |err| {
            self.allocator.free(err);
        }
        self.errors.deinit();
        self.prefix_parse_fns.deinit();
        self.infix_parse_fns.deinit();
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
        const msg = std.fmt.allocPrint(
            self.allocator,
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

    fn parse_expression_statement(self: *Parser) ?ast.ExpressionStatement {
        const expression_token = self.cur_token;
        const expr = self.parse_expression(ExprOrder.lowest);
        if (expr == null) {
            return null;
        }
        const stmt: ast.ExpressionStatement = .{
            .token = expression_token,
            .expression = expr.?,
        };

        if (self.peek_token.type == token.TokenType.semicolon) {
            self.next_token();
        }

        return stmt;
    }

    fn parse_integer_literal(self: *Parser) ast.Expression {
        const int_token = self.cur_token;

        const value = std.fmt.parseInt(i64, int_token.literal, 10) catch 0;

        const expr: ast.Expression = .{
            .integer_literal = .{
                .token = int_token,
                .value = value,
            },
        };

        return expr;
    }

    fn parse_prefix_expression(self: *Parser) ast.Expression {
        const pre_token = self.cur_token;

        self.next_token();

        std.debug.print("found operator: {s}\n", .{pre_token.literal});

        const right_expr = self.allocator.create(ast.Expression) catch {
            return .{
                .identifier = .{
                    .token = pre_token,
                    .value = pre_token.literal,
                },
            };
        };
        right_expr.* = self.parse_expression(ExprOrder.prefix).?;

        std.debug.print("parse_prefix_expression right_expr: {s}\n", .{right_expr.token_literal()});
        std.debug.print("parse_prefix_expression right_expr: {s}\n", .{right_expr.string()});

        const expr: ast.Expression = .{
            .prefix_expr = .{
                .token = pre_token,
                .operator = pre_token.literal,
                .right = right_expr,
            },
        };

        return expr;
    }

    fn no_prefix_parser_fn_error(self: *Parser, token_type: token.TokenType) void {
        const msg = std.fmt.allocPrint(self.allocator, "no prefix parse function for {any} found, {s}", .{ token_type, self.*.cur_token.literal }) catch "unknown";
        self.errors.append(msg) catch {};
    }

    fn parse_expression(self: *Parser, _: ExprOrder) ?ast.Expression {
        std.debug.print("ParseExpression finding a prefix function for: {any}\n", .{self.cur_token.type});
        const prefix = self.prefix_parse_fns.get(self.cur_token.type);

        if (prefix == null) {
            self.no_prefix_parser_fn_error(self.cur_token.type);
            return null;
        }

        const leftExp = prefix.?(self);
        std.debug.print("parse expression: {s}\n", .{leftExp.string()});

        return leftExp;
    }

    fn parse_identifier(self: *Parser) ast.Expression {
        return .{
            .identifier = .{
                .token = self.cur_token,
                .value = self.cur_token.literal,
            },
        };
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
            else => {
                const parsed = self.parse_expression_statement();
                std.debug.print("Parsed expr statement: {any}\n", .{parsed});
                if (parsed != null) {
                    return .{ .expr_stmt = parsed.? };
                }
                return null;
            },
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

    pub fn register_prefix(self: *Parser, token_type: token.TokenType, prefix_parser_fn: prefixParseFn) !void {
        try self.prefix_parse_fns.put(token_type, prefix_parser_fn);
    }

    pub fn register_infix(self: *Parser, token_type: token.TokenType, infix_parser_fn: infixParseFn) void {
        self.infix_parse_fns.put(token_type, infix_parser_fn);
    }
};

test "test let statement" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
    defer p.deinit();

    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    // check parser errors
    for (p.errors.items) |err| {
        std.debug.print("found bad err: {s}", .{err});
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
        \\let x 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
    defer p.deinit();

    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    // check parser errors
    for (p.errors.items) |err| {
        // should find one :D
        std.debug.print("found bad err: {s}", .{err});
    }

    try std.testing.expect(p.errors.items.len == 1);
}

test "test return statements" {
    const input =
        \\return 5;
        \\return 10;
        \\return 838383;
    ;

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
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

test "test identifier expression" {
    const input = "foobar;";

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
    defer p.deinit();
    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    try std.testing.expect(p.errors.items.len == 0);

    std.debug.print("{any}\n", .{program.statements.items});
    try std.testing.expect(program.statements.items.len == 1);

    const stmt: ast.Statement = program.statements.getLast();
    const expr_stmt = ast.ExpressionStatement.init(stmt);

    const ident = expr_stmt.expression.identifier;

    try std.testing.expectEqualStrings("foobar", ident.value);
    try std.testing.expectEqualStrings("foobar", ident.token_literal());
}

test "test integer literal expression" {
    const input = "5;";

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
    defer p.deinit();
    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    try std.testing.expect(p.errors.items.len == 0);

    std.debug.print("{any}\n", .{program.statements.items});
    try std.testing.expect(program.statements.items.len == 1);

    const stmt: ast.Statement = program.statements.getLast();
    const int_literal = ast.IntegerLiteral.init(&stmt.expr_stmt.expression);

    try std.testing.expect(int_literal.value == 5);

    try std.testing.expectEqualStrings("5", int_literal.token_literal());
}

test "test parsing prefix expression" {
    std.debug.print("started test parsing prefix expression\n", .{});
    const prefix_tests = [_]struct {
        input: []const u8,
        operator: []const u8,
        int_value: i64,
    }{
        .{
            .input = "!5;",
            .operator = "!",
            .int_value = 5,
        },
        .{
            .input = "-15;",
            .operator = "-",
            .int_value = 15,
        },
    };

    for (prefix_tests) |p_test| {
        var lex = lexer.Lexer.init(p_test.input);
        var p = try Parser.init(std.testing.allocator, &lex);
        defer p.deinit();
        var program = try p.parse_program(std.testing.allocator);
        defer program.deinit();

        try std.testing.expect(p.errors.items.len == 0);
        try std.testing.expect(program.statements.items.len == 1);

        const stmt: ast.Statement = program.statements.getLast();
        const exp_stmt = stmt.expr_stmt;
        const pre_expr = ast.PrefixExpression.init(&exp_stmt.expression);

        try std.testing.expectEqualStrings(p_test.operator, pre_expr.operator);

        const int_literal = ast.IntegerLiteral.init(pre_expr.right);
        try std.testing.expectEqual(p_test.int_value, int_literal.value);
        const str_token_literal = try std.fmt.allocPrint(std.testing.allocator, "{d}", .{p_test.int_value});
        defer std.testing.allocator.free(str_token_literal);

        try std.testing.expectEqualStrings(str_token_literal, int_literal.token_literal());
    }
}
