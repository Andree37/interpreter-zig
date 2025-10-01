const std = @import("std");

const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");

const prefixParseFn = *const fn (*Parser) ?ast.Expression;
const infixParseFn = *const fn (*Parser, ast.Expression) ?ast.Expression;

pub const ExprOrder = enum {
    lowest,
    equals, // ==
    lessgreater, // > or <
    sum, // +
    product, // *
    prefix, // -X or !X
    call, // my_function(X)
};
const PrecendecesEntry = struct {
    key: token.TokenType,
    value: ExprOrder,
};

const precendences = [_]PrecendecesEntry{
    .{ .key = .eq, .value = .equals },
    .{ .key = .not_eq, .value = .equals },
    .{ .key = .lt, .value = .lessgreater },
    .{ .key = .gt, .value = .lessgreater },
    .{ .key = .plus, .value = .sum },
    .{ .key = .minus, .value = .sum },
    .{ .key = .slash, .value = .product },
    .{ .key = .asterisk, .value = .product },
    .{ .key = .l_paren, .value = .call },
};

fn lookup(key: token.TokenType) ?ExprOrder {
    inline for (precendences) |entry| {
        if (entry.key == key) return entry.value;
    }
    return null;
}

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

        // prefix
        try p.register_prefix(token.TokenType.ident, parse_identifier);
        try p.register_prefix(token.TokenType.int, parse_integer_literal);
        try p.register_prefix(token.TokenType.bang, parse_prefix_expression);
        try p.register_prefix(token.TokenType.minus, parse_prefix_expression);
        try p.register_prefix(token.TokenType.true, parse_boolean);
        try p.register_prefix(token.TokenType.false, parse_boolean);
        try p.register_prefix(token.TokenType.l_paren, parse_grouped_expression);
        try p.register_prefix(token.TokenType.kif, parse_if_expression);
        try p.register_prefix(token.TokenType.function, parse_function_literal);

        // infix
        try p.register_infix(token.TokenType.plus, parse_infix_expression);
        try p.register_infix(token.TokenType.minus, parse_infix_expression);
        try p.register_infix(token.TokenType.slash, parse_infix_expression);
        try p.register_infix(token.TokenType.asterisk, parse_infix_expression);
        try p.register_infix(token.TokenType.eq, parse_infix_expression);
        try p.register_infix(token.TokenType.not_eq, parse_infix_expression);
        try p.register_infix(token.TokenType.lt, parse_infix_expression);
        try p.register_infix(token.TokenType.gt, parse_infix_expression);
        try p.register_infix(token.TokenType.l_paren, parse_call_expression);

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

    fn peek_precendence(self: *Parser) ExprOrder {
        if (lookup(self.peek_token.type)) |l| {
            return l;
        }

        return ExprOrder.lowest;
    }

    fn cur_precendence(self: *Parser) ExprOrder {
        if (lookup(self.cur_token.type)) |l| {
            return l;
        }

        return ExprOrder.lowest;
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
        const let_stmt_token = self.cur_token;

        if (!self.expect_peek(token.TokenType.ident)) {
            return null;
        }

        const identifier_token = self.cur_token;

        if (!self.expect_peek(token.TokenType.assign)) {
            return null;
        }

        self.next_token();

        const value_expr = self.parse_expression(ExprOrder.lowest) orelse return null;

        if (self.peek_token.type == token.TokenType.semicolon) {
            self.next_token();
        }

        return ast.LetStatement{
            .token = let_stmt_token,
            .name = .{
                .token = identifier_token,
                .value = identifier_token.literal,
            },
            .value = value_expr,
        };
    }

    fn parse_return_statement(self: *Parser) ?ast.ReturnStatement {
        const return_token = self.cur_token;

        self.next_token();

        const return_value = self.parse_expression(ExprOrder.lowest) orelse return null;

        if (self.expect_peek(token.TokenType.semicolon)) {
            self.next_token();
        }

        return ast.ReturnStatement{
            .token = return_token,
            .return_value = return_value,
        };
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

    fn parse_integer_literal(self: *Parser) ?ast.Expression {
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

    fn parse_infix_expression(self: *Parser, left: ast.Expression) ?ast.Expression {
        const infix_token = self.cur_token;
        const precedence = self.cur_precendence();

        self.next_token();

        const right_expr = self.allocator.create(ast.Expression) catch {
            return .{
                .identifier = .{
                    .token = infix_token,
                    .value = infix_token.literal,
                },
            };
        };
        right_expr.* = self.parse_expression(precedence).?;

        const left_expr = self.allocator.create(ast.Expression) catch {
            return .{
                .identifier = .{
                    .token = infix_token,
                    .value = infix_token.literal,
                },
            };
        };
        left_expr.* = left;

        const expr: ast.Expression = .{
            .infix_expr = .{
                .left = left_expr,
                .token = infix_token,
                .operator = infix_token.literal,
                .right = right_expr,
            },
        };

        return expr;
    }

    fn parse_call_expression(self: *Parser, function: ast.Expression) ?ast.Expression {
        const cur_token = self.cur_token;

        const args = (self.parse_call_arguments() catch return null) orelse return null;

        const call_function = self.allocator.create(ast.Expression) catch return null;
        call_function.* = function;

        return ast.Expression{
            .call_expr = .{
                .arguments = args,
                .function = call_function,
                .token = cur_token,
            },
        };
    }

    fn parse_call_arguments(self: *Parser) !?std.ArrayList(ast.Expression) {
        var args = std.ArrayList(ast.Expression).init(self.allocator);

        if (self.peek_token.type == token.TokenType.r_paren) {
            self.next_token();
            return args;
        }

        self.next_token();
        const expr = self.parse_expression(ExprOrder.lowest) orelse return null;
        try args.append(expr);

        while (self.peek_token.type == token.TokenType.comma) {
            self.next_token();
            self.next_token();

            const arg_expr = self.parse_expression(ExprOrder.lowest) orelse return null;
            try args.append(arg_expr);
        }

        if (!self.expect_peek(token.TokenType.r_paren)) {
            return null;
        }

        return args;
    }

    fn parse_prefix_expression(self: *Parser) ?ast.Expression {
        const pre_token = self.cur_token;

        self.next_token();

        const right_expr = self.allocator.create(ast.Expression) catch {
            return .{
                .identifier = .{
                    .token = pre_token,
                    .value = pre_token.literal,
                },
            };
        };
        right_expr.* = self.parse_expression(ExprOrder.prefix).?;

        const expr: ast.Expression = .{
            .prefix_expr = .{
                .token = pre_token,
                .operator = pre_token.literal,
                .right = right_expr,
            },
        };

        return expr;
    }

    fn parse_boolean(self: *Parser) ?ast.Expression {
        const pre_token = self.cur_token;

        const expr: ast.Expression = .{
            .bool_expr = .{
                .token = pre_token,
                .value = pre_token.type == token.TokenType.true,
            },
        };

        return expr;
    }

    fn parse_grouped_expression(self: *Parser) ?ast.Expression {
        self.next_token();

        const expr = self.parse_expression(ExprOrder.lowest);
        if (expr == null) {
            return null;
        }

        if (!self.expect_peek(token.TokenType.r_paren)) {
            return null;
        }

        return expr;
    }

    fn parse_if_expression(self: *Parser) ?ast.Expression {
        const cur_token = self.cur_token;

        if (!self.expect_peek(token.TokenType.l_paren)) {
            return null;
        }

        self.next_token();
        const condition = self.allocator.create(ast.Expression) catch {
            return null;
        };
        const par_expr = self.parse_expression(ExprOrder.lowest);
        if (par_expr == null) {
            return null;
        }

        condition.* = par_expr.?;

        if (!self.expect_peek(token.TokenType.r_paren)) {
            return null;
        }

        if (!self.expect_peek(token.TokenType.l_brace)) {
            return null;
        }

        const block_statement = self.parse_block_statement();
        if (block_statement == null) {
            return null;
        }

        var alternative: ?*ast.BlockStatement = null;
        if (self.peek_token.type == token.TokenType.kelse) {
            self.next_token();

            if (!self.expect_peek(token.TokenType.l_brace)) {
                return null;
            }

            std.debug.print("I AM A POTATO", .{});
            alternative = self.parse_block_statement();
        }

        const if_expr = ast.IfExpression{
            .token = cur_token,
            .condition = condition,
            .consequence = block_statement.?,
            .alternative = alternative,
        };

        const expr = ast.Expression{ .if_expr = if_expr };

        return expr;
    }

    fn parse_function_literal(self: *Parser) ?ast.Expression {
        const cur_token = self.cur_token;

        if (!self.expect_peek(token.TokenType.l_paren)) {
            return null;
        }

        const params = self.parse_function_params() catch {
            return null;
        };

        if (params == null) {
            return null;
        }

        if (!self.expect_peek(token.TokenType.l_brace)) {
            return null;
        }

        const body = self.parse_block_statement();
        if (body == null) {
            return null;
        }

        const fun_lit = ast.FunctionLiteral{
            .body = body.?,
            .parameters = params.?,
            .token = cur_token,
        };

        return ast.Expression{ .func_literal = fun_lit };
    }

    fn parse_function_params(self: *Parser) !?std.ArrayList(ast.Identifier) {
        var identifiers = std.ArrayList(ast.Identifier).init(self.allocator);

        if (self.peek_token.type == token.TokenType.r_paren) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        const cur_token = self.cur_token;
        const ident = ast.Identifier{ .token = cur_token, .value = cur_token.literal };

        try identifiers.append(ident);

        while (self.peek_token.type == token.TokenType.comma) {
            self.next_token();
            self.next_token();
            try identifiers.append(ast.Identifier{ .token = self.cur_token, .value = self.cur_token.literal });
        }

        if (!self.expect_peek(token.TokenType.r_paren)) {
            return null;
        }

        return identifiers;
    }

    fn parse_block_statement(self: *Parser) ?*ast.BlockStatement {
        const cur_token = self.cur_token;

        var statements = std.ArrayList(ast.Statement).init(self.allocator);

        self.next_token();

        while (!(self.cur_token.type == token.TokenType.r_brace) and !(self.cur_token.type == token.TokenType.eof)) {
            const stmt = self.parse_statement();

            if (stmt != null) {
                statements.append(stmt.?) catch {
                    continue;
                };
            }
            self.next_token();
        }

        const block_statement = self.allocator.create(ast.BlockStatement) catch {
            return null;
        };

        block_statement.statements = statements;
        block_statement.token = cur_token;

        return block_statement;
    }

    fn no_prefix_parser_fn_error(self: *Parser, token_type: token.TokenType) void {
        const msg = std.fmt.allocPrint(self.allocator, "no prefix parse function for {any} found, {s}", .{ token_type, self.*.cur_token.literal }) catch "unknown";
        self.errors.append(msg) catch {};
    }

    fn parse_expression(self: *Parser, precedence: ExprOrder) ?ast.Expression {
        std.debug.print("ParseExpression finding a prefix function for: {any}\n", .{self.cur_token.type});
        const prefix = self.prefix_parse_fns.get(self.cur_token.type);

        if (prefix == null) {
            self.no_prefix_parser_fn_error(self.cur_token.type);
            return null;
        }

        var leftExp = prefix.?(self);

        while (!(self.peek_token.type == token.TokenType.semicolon) and @intFromEnum(precedence) < @intFromEnum(self.peek_precendence())) {
            const infix = self.infix_parse_fns.get(self.peek_token.type);

            if (infix == null) {
                return leftExp;
            }

            self.next_token();

            leftExp = infix.?(self, leftExp.?);
            if (leftExp == null) {
                return null;
            }
        }

        return leftExp;
    }

    fn parse_identifier(self: *Parser) ?ast.Expression {
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
                const parsed = self.parse_return_statement() orelse return null;
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

    pub fn register_infix(self: *Parser, token_type: token.TokenType, infix_parser_fn: infixParseFn) !void {
        try self.infix_parse_fns.put(token_type, infix_parser_fn);
    }
};

test "test let statement" {
    const tests = [_]struct {
        input: []const u8,
        expectedIdentifier: []const u8,
        expectedValue: []const u8,
    }{
        .{ .input = "let x = 5;", .expectedIdentifier = "x", .expectedValue = "5" },
        .{ .input = "let y = true;", .expectedIdentifier = "y", .expectedValue = "true" },
        .{ .input = "let foobar = y;", .expectedIdentifier = "foobar", .expectedValue = "y" },
    };

    for (tests) |t| {
        var lex = lexer.Lexer.init(t.input);
        var p = try Parser.init(std.testing.allocator, &lex);
        defer p.deinit();

        var program = try p.parse_program(std.testing.allocator);
        defer program.deinit();

        // check parser errors
        for (p.errors.items) |err| {
            std.debug.print("found bad err: {s}", .{err});
        }

        try std.testing.expect(p.errors.items.len == 0);

        try std.testing.expect(program.statements.items.len == 1);

        const stmt = program.statements.items[0];
        try std.testing.expectEqualStrings("let", stmt.token_literal());

        const val = stmt.let_stmt;

        try std.testing.expectEqualStrings(t.expectedIdentifier, val.name.value);
        try std.testing.expectEqualStrings(t.expectedValue, val.value.token_literal());
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
    const tests = [_]struct {
        input: []const u8,
        expectedValue: []const u8,
    }{
        .{ .input = "return 5;", .expectedValue = "5" },
        .{ .input = "return 10;", .expectedValue = "10" },
        .{ .input = "return 83838383;", .expectedValue = "83838383" },
    };

    for (tests) |t| {
        var lex = lexer.Lexer.init(t.input);
        var p = try Parser.init(std.testing.allocator, &lex);
        defer p.deinit();

        var program = try p.parse_program(std.testing.allocator);
        defer program.deinit();

        // check parser errors
        for (p.errors.items) |err| {
            std.debug.print("found bad err: {s}", .{err});
        }

        try std.testing.expect(p.errors.items.len == 0);

        try std.testing.expect(program.statements.items.len == 1);

        const stmt = program.statements.items[0];
        try std.testing.expectEqualStrings("return", stmt.token_literal());

        const val = stmt.return_stmt;
        try std.testing.expectEqualStrings(t.expectedValue, val.return_value.token_literal());
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

test "test parsing boolean prefix expression" {
    const prefix_tests = [_]struct {
        input: []const u8,
        operator: []const u8,
        int_value: bool,
    }{
        .{
            .input = "!true;",
            .operator = "!",
            .int_value = true,
        },
        .{
            .input = "!false;",
            .operator = "!",
            .int_value = false,
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

        const bool_literal = ast.Boolean.init(pre_expr.right);
        try std.testing.expectEqual(p_test.int_value, bool_literal.value);
        const str_token_literal = try std.fmt.allocPrint(std.testing.allocator, "{}", .{p_test.int_value});
        defer std.testing.allocator.free(str_token_literal);

        try std.testing.expectEqualStrings(str_token_literal, bool_literal.token_literal());
    }
}

test "test parsing infix expressions" {
    std.debug.print("started test parsing infix expressions\n", .{});
    const inxfix_tests = [_]struct {
        input: []const u8,
        left_value: i64,
        operator: []const u8,
        right_value: i64,
    }{
        .{ .input = "5+5;", .left_value = 5, .operator = "+", .right_value = 5 },
        .{ .input = "5-5;", .left_value = 5, .operator = "-", .right_value = 5 },
        .{ .input = "5*5;", .left_value = 5, .operator = "*", .right_value = 5 },
        .{ .input = "5/5;", .left_value = 5, .operator = "/", .right_value = 5 },
        .{ .input = "5>5;", .left_value = 5, .operator = ">", .right_value = 5 },
        .{ .input = "5<5;", .left_value = 5, .operator = "<", .right_value = 5 },
        .{ .input = "5==5;", .left_value = 5, .operator = "==", .right_value = 5 },
        .{ .input = "5!=5;", .left_value = 5, .operator = "!=", .right_value = 5 },
    };

    for (inxfix_tests) |p_test| {
        var lex = lexer.Lexer.init(p_test.input);
        var p = try Parser.init(std.testing.allocator, &lex);
        defer p.deinit();
        var program = try p.parse_program(std.testing.allocator);
        defer program.deinit();

        try std.testing.expect(p.errors.items.len == 0);
        try std.testing.expect(program.statements.items.len == 1);

        const stmt: ast.Statement = program.statements.getLast();
        const exp_stmt = stmt.expr_stmt;
        const inf_expr = ast.InfixExpression.init(&exp_stmt.expression);

        try std.testing.expectEqualStrings(p_test.operator, inf_expr.operator);

        const left_int_literal = ast.IntegerLiteral.init(inf_expr.left);
        try std.testing.expectEqual(p_test.left_value, left_int_literal.value);
        const left_str_token_literal = try std.fmt.allocPrint(std.testing.allocator, "{d}", .{p_test.left_value});
        defer std.testing.allocator.free(left_str_token_literal);
        try std.testing.expectEqualStrings(left_str_token_literal, left_int_literal.token_literal());

        const right_int_literal = ast.IntegerLiteral.init(inf_expr.right);
        try std.testing.expectEqual(p_test.right_value, right_int_literal.value);
        const right_str_token_literal = try std.fmt.allocPrint(std.testing.allocator, "{d}", .{p_test.right_value});
        defer std.testing.allocator.free(right_str_token_literal);
        try std.testing.expectEqualStrings(right_str_token_literal, right_int_literal.token_literal());
    }
}

test "test parsing boolean infix expressions" {
    std.debug.print("started test parsing infix expressions\n", .{});
    const inxfix_tests = [_]struct {
        input: []const u8,
        left_value: bool,
        operator: []const u8,
        right_value: bool,
    }{
        .{ .input = "true == true;", .left_value = true, .operator = "==", .right_value = true },
        .{ .input = "true != false;", .left_value = true, .operator = "!=", .right_value = false },
        .{ .input = "false == false;", .left_value = false, .operator = "==", .right_value = false },
    };

    for (inxfix_tests) |p_test| {
        var lex = lexer.Lexer.init(p_test.input);
        var p = try Parser.init(std.testing.allocator, &lex);
        defer p.deinit();
        var program = try p.parse_program(std.testing.allocator);
        defer program.deinit();

        try std.testing.expect(p.errors.items.len == 0);
        try std.testing.expect(program.statements.items.len == 1);

        const stmt: ast.Statement = program.statements.getLast();
        const exp_stmt = stmt.expr_stmt;
        const inf_expr = ast.InfixExpression.init(&exp_stmt.expression);

        try std.testing.expectEqualStrings(p_test.operator, inf_expr.operator);

        const left_bool_literal = ast.Boolean.init(inf_expr.left);
        try std.testing.expectEqual(p_test.left_value, left_bool_literal.value);
        const left_str_token_literal = try std.fmt.allocPrint(std.testing.allocator, "{}", .{p_test.left_value});
        defer std.testing.allocator.free(left_str_token_literal);
        try std.testing.expectEqualStrings(left_str_token_literal, left_bool_literal.token_literal());

        const right_bool_literal = ast.Boolean.init(inf_expr.right);
        try std.testing.expectEqual(p_test.right_value, right_bool_literal.value);
        const right_str_token_literal = try std.fmt.allocPrint(std.testing.allocator, "{}", .{p_test.right_value});
        defer std.testing.allocator.free(right_str_token_literal);
        try std.testing.expectEqualStrings(right_str_token_literal, right_bool_literal.token_literal());
    }
}

test "test operator precedence parsing" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "-a * b", .expected = "((-a) * b)" },
        .{ .input = "!-a", .expected = "(!(-a))" },
        .{ .input = "a + b + c", .expected = "((a + b) + c)" },
        .{ .input = "a + b - c", .expected = "((a + b) - c)" },
        .{ .input = "a * b * c", .expected = "((a * b) * c)" },
        .{ .input = "a * b / c", .expected = "((a * b) / c)" },
        .{ .input = "a + b / c", .expected = "(a + (b / c))" },
        .{ .input = "a + b * c + d / e - f", .expected = "(((a + (b * c)) + (d / e)) - f)" },
        .{ .input = "3 + 4; -5 * 5", .expected = "(3 + 4)((-5) * 5)" },
        .{ .input = "5 > 4 == 3 < 4", .expected = "((5 > 4) == (3 < 4))" },
        .{ .input = "5 < 4 != 3 > 4", .expected = "((5 < 4) != (3 > 4))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ .input = "true", .expected = "true" },
        .{ .input = "false", .expected = "false" },
        .{ .input = "3 > 5 == false", .expected = "((3 > 5) == false)" },
        .{ .input = "3 < 5 == true", .expected = "((3 < 5) == true)" },
        .{ .input = "1 + (2 + 3) + 4", .expected = "((1 + (2 + 3)) + 4)" },
        .{ .input = "(5 + 5) * 2", .expected = "((5 + 5) * 2)" },
        .{ .input = "2 / (5 + 5)", .expected = "(2 / (5 + 5))" },
        .{ .input = "-(5 + 5)", .expected = "(-(5 + 5))" },
        .{ .input = "!(true == true)", .expected = "(!(true == true))" },
        .{ .input = "a + add(b * c) + d", .expected = "((a + add((b * c))) + d)" },
        .{ .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
        .{ .input = "add(a + b + c * d / f + g)", .expected = "add((((a + b) + ((c * d) / f)) + g))" },
    };

    for (tests) |t| {
        var lex = lexer.Lexer.init(t.input);
        var p = try Parser.init(std.testing.allocator, &lex);
        defer p.deinit();
        var program = try p.parse_program(std.testing.allocator);
        defer program.deinit();

        for (p.errors.items) |err| {
            std.debug.print("dumb err: {s}\n", .{err});
        }
        try std.testing.expect(p.errors.items.len == 0);

        const actual = try program.string();
        defer std.testing.allocator.free(actual);

        try std.testing.expectEqualStrings(t.expected, actual);
    }
}

test "test boolean literal" {
    const input = "true;";

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
    defer p.deinit();
    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    try std.testing.expect(p.errors.items.len == 0);

    std.debug.print("{any}\n", .{program.statements.items});
    try std.testing.expect(program.statements.items.len == 1);

    const stmt: ast.Statement = program.statements.getLast();
    const bool_literal = ast.Boolean.init(&stmt.expr_stmt.expression);

    try std.testing.expect(bool_literal.value);
    try std.testing.expectEqualStrings("true", bool_literal.token_literal());
}

test "test if expression" {
    const input = "if (x < y) { x } else { y }";

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
    defer p.deinit();
    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    try std.testing.expect(p.errors.items.len == 0);

    std.debug.print("{any}\n", .{program.statements.items});
    try std.testing.expect(program.statements.items.len == 1);

    const stmt: ast.Statement = program.statements.getLast();
    const expr = stmt.expr_stmt;
    const if_expr = ast.IfExpression.init(&expr.expression);

    try std.testing.expectEqualStrings("x", if_expr.condition.infix_expr.left.*.token_literal());
    try std.testing.expectEqualStrings("<", if_expr.condition.token_literal());
    try std.testing.expectEqualStrings("y", if_expr.condition.infix_expr.right.*.token_literal());

    try std.testing.expect(if_expr.consequence.statements.items.len == 1);

    const consequence = if_expr.consequence.statements.items[0];
    try std.testing.expectEqualStrings("x", consequence.expr_stmt.expression.token_literal());

    try std.testing.expect(if_expr.alternative.?.statements.items.len == 1);
    const alternative = if_expr.alternative.?.statements.items[0];
    try std.testing.expectEqualStrings("y", alternative.expr_stmt.expression.token_literal());
}

test "test function literal parsing" {
    const input = "fn(x, y) { x + y; }";

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
    defer p.deinit();
    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    for (p.errors.items) |err| {
        std.debug.print("Found error: {s}\n", .{err});
    }
    try std.testing.expect(p.errors.items.len == 0);

    std.debug.print("{any}\n", .{program.statements.items});
    try std.testing.expect(program.statements.items.len == 1);

    const stmt: ast.Statement = program.statements.getLast();
    const expr = stmt.expr_stmt;
    const fun_literal = ast.FunctionLiteral.init(&expr.expression);

    try std.testing.expect(fun_literal.parameters.items.len == 2);

    try std.testing.expectEqualStrings("x", fun_literal.parameters.items[0].value);
    try std.testing.expectEqualStrings("y", fun_literal.parameters.items[1].value);

    try std.testing.expect(fun_literal.body.statements.items.len == 1);

    const body_stmt = ast.ExpressionStatement.init(fun_literal.body.statements.items[0]);
    try std.testing.expectEqualStrings("x", body_stmt.expression.infix_expr.left.*.token_literal());
    try std.testing.expectEqualStrings("+", body_stmt.expression.infix_expr.operator);
    try std.testing.expectEqualStrings("y", body_stmt.expression.infix_expr.right.*.token_literal());
}

test "test function parameter parsing" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const []const u8,
    }{
        .{ .input = "fn() {};", .expected = &.{} },
        .{ .input = "fn(x) {};", .expected = &.{"x"} },
        .{ .input = "fn(x, y, z) {};", .expected = &.{ "x", "y", "z" } },
    };

    for (tests) |t| {
        var lex = lexer.Lexer.init(t.input);
        var p = try Parser.init(std.testing.allocator, &lex);
        defer p.deinit();
        var program = try p.parse_program(std.testing.allocator);
        defer program.deinit();

        for (p.errors.items) |err| {
            std.debug.print("dumb err: {s}\n", .{err});
        }
        try std.testing.expect(p.errors.items.len == 0);

        const stmt = program.statements.items[0];
        const expr_stmt = stmt.expr_stmt;
        const function = ast.FunctionLiteral.init(&expr_stmt.expression);

        try std.testing.expect(function.parameters.items.len == t.expected.len);

        for (function.parameters.items, 0..) |ident, i| {
            try std.testing.expectEqualStrings(t.expected[i], ident.value);
        }
    }
}

test "test call expression parsing" {
    const input = "add(1, 2 * 3, 4 + 5);";

    var lex = lexer.Lexer.init(input);
    var p = try Parser.init(std.testing.allocator, &lex);
    defer p.deinit();
    var program = try p.parse_program(std.testing.allocator);
    defer program.deinit();

    for (p.errors.items) |err| {
        std.debug.print("Found error: {s}\n", .{err});
    }
    try std.testing.expect(p.errors.items.len == 0);

    std.debug.print("{any}\n", .{program.statements.items});
    try std.testing.expect(program.statements.items.len == 1);

    const stmt: ast.Statement = program.statements.getLast();
    const expr = stmt.expr_stmt;
    const call_expr = ast.CallExpression.init(&expr.expression);

    try std.testing.expectEqualStrings("add", call_expr.function.identifier.value);
    try std.testing.expect(call_expr.arguments.items.len == 3);

    try std.testing.expectEqual(1, call_expr.arguments.items[0].integer_literal.value);

    const infix_expr_1 = call_expr.arguments.items[1].infix_expr;
    try std.testing.expectEqual(2, infix_expr_1.left.integer_literal.value);
    try std.testing.expectEqualStrings("*", infix_expr_1.operator);
    try std.testing.expectEqual(3, infix_expr_1.right.integer_literal.value);

    const infix_expr_2 = call_expr.arguments.items[2].infix_expr;
    try std.testing.expectEqual(4, infix_expr_2.left.integer_literal.value);
    try std.testing.expectEqualStrings("+", infix_expr_2.operator);
    try std.testing.expectEqual(5, infix_expr_2.right.integer_literal.value);
}
