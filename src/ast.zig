const std = @import("std");

const token = @import("token.zig");

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
};

pub const Statement = union(enum) {
    let_stmt: LetStatement,
    return_stmt: ReturnStatement,
    expr_stmt: ExpressionStatement,
    // add return, expression, etc. later

    pub fn token_literal(self: *const Statement) []const u8 {
        return switch (self.*) {
            .let_stmt => |stmt| stmt.token_literal(),
            .return_stmt => |stmt| stmt.token_literal(),
            .expr_stmt => |stmt| stmt.token_literal(),
        };
    }

    pub fn string(self: *const Statement, writer: anytype) !void {
        return switch (self.*) {
            .let_stmt => |stmt| try stmt.string(writer),
            .return_stmt => |stmt| try stmt.string(writer),
            .expr_stmt => |stmt| try stmt.string(writer),
        };
    }

    pub fn deinit(self: *const Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .let_stmt => |stmt| stmt.value.deinit(allocator),
            .return_stmt => |stmt| stmt.return_value.deinit(allocator),
            .expr_stmt => |stmt| stmt.expression.deinit(allocator),
        }
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    prefix_expr: PrefixExpression,
    infix_expr: InfixExpression,
    bool_expr: Boolean,
    if_expr: IfExpression,
    func_literal: FunctionLiteral,
    call_expr: CallExpression,

    pub fn token_literal(self: *const Expression) []const u8 {
        return switch (self.*) {
            .identifier => |ident| ident.token_literal(),
            .integer_literal => |int_lit| int_lit.token_literal(),
            .prefix_expr => |expr| expr.token_literal(),
            .infix_expr => |expr| expr.token_literal(),
            .bool_expr => |expr| expr.token_literal(),
            .if_expr => |expr| expr.token_literal(),
            .func_literal => |expr| expr.token_literal(),
            .call_expr => |expr| expr.token_literal(),
        };
    }

    pub fn string(self: *const Expression, writer: anytype) anyerror!void {
        switch (self.*) {
            .identifier => |ident| try ident.string(writer),
            .integer_literal => |int_lit| try int_lit.string(writer),
            .prefix_expr => |expr| try expr.string(writer),
            .infix_expr => |expr| try expr.string(writer),
            .bool_expr => |expr| try expr.string(writer),
            .if_expr => |expr| try expr.string(writer),
            .func_literal => |expr| try expr.string(writer),
            .call_expr => |expr| try expr.string(writer),
        }
    }

    pub fn deinit(self: *const Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .identifier => {},
            .integer_literal => {},
            .prefix_expr => |prefix| {
                prefix.right.deinit(allocator);
                allocator.destroy(prefix.right);
            },
            .infix_expr => |infix| {
                infix.right.deinit(allocator);
                allocator.destroy(infix.right);

                infix.left.deinit(allocator);
                allocator.destroy(infix.left);
            },
            .bool_expr => {},
            .if_expr => |expr| {
                expr.condition.deinit(allocator);
                allocator.destroy(expr.condition);

                expr.consequence.deinit(allocator);
                allocator.destroy(expr.consequence);

                if (expr.alternative != null) {
                    expr.alternative.?.deinit(allocator);
                    allocator.destroy(expr.alternative.?);
                }
            },
            .func_literal => |fun| {
                fun.deinit(allocator);
            },
            .call_expr => |call_expr| {
                call_expr.deinit(allocator);
            },
        }
    }
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) Program {
        return Program{
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.statements.items) |stmt| {
            stmt.deinit(self.allocator);
        }
        self.statements.deinit();
    }

    pub fn string(self: *Program) ![]const u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        defer list.deinit();

        const stream = list.writer();
        for (self.statements.items) |item| {
            try item.string(stream);
        }

        return list.toOwnedSlice();
    }

    fn token_literal(self: *Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].node.token_literal();
        } else {
            return "";
        }
    }
};

pub const Identifier = struct {
    token: token.Token, // the token.IDENT token
    value: []const u8,

    pub fn token_literal(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const Identifier, writer: anytype) !void {
        try writer.writeAll(self.value);
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    value: Expression,

    // fn statement_node(self: *LetStatement) void!anyerror {}

    pub fn init(stmt: Statement) LetStatement {
        return stmt.let_stmt;
    }

    pub fn string(self: *const LetStatement, writer: anytype) !void {
        // TODO: need to add expressions
        try writer.writeAll(self.token_literal());
        try writer.writeByte(' ');
        try self.name.string(writer);
        try writer.writeAll(" = ");
        try self.value.string(writer);
        try writer.writeByte(';');
    }

    fn token_literal(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    return_value: Expression,

    // fn statement_node(self: *ReturnStatement) void!anyerror {}

    pub fn init(stmt: Statement) ReturnStatement {
        return stmt.return_stmt;
    }

    pub fn string(self: *const ReturnStatement, writer: anytype) !void {
        // TODO: add expression
        try writer.writeAll(self.token_literal());
        try writer.writeByte(' ');
        try writer.writeByte(';');
    }

    pub fn token_literal(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ExpressionStatement = struct {
    token: token.Token,
    expression: Expression,

    // fn statement_node(self: *ExpressionStatement) void!anyerror {}

    pub fn init(stmt: Statement) ExpressionStatement {
        return stmt.expr_stmt;
    }

    pub fn token_literal(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ExpressionStatement, writer: anytype) !void {
        try self.expression.string(writer);
    }
};

pub const IntegerLiteral = struct {
    token: token.Token,
    value: i64,

    pub fn init(expr: *const Expression) IntegerLiteral {
        return expr.integer_literal;
    }

    pub fn token_literal(self: *const IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const IntegerLiteral, writer: anytype) !void {
        try writer.writeAll(self.token_literal());
    }
};

pub const PrefixExpression = struct {
    token: token.Token,
    operator: []const u8,
    right: *const Expression,

    pub fn init(expr: *const Expression) PrefixExpression {
        return expr.prefix_expr;
    }

    pub fn token_literal(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn expression_node(self: *const PrefixExpression) Expression {
        return self.right.*;
    }

    pub fn string(self: *const PrefixExpression, writer: anytype) !void {
        try writer.writeByte('(');
        try writer.writeAll(self.operator);
        try self.right.string(writer);
        try writer.writeByte(')');
    }
};

pub const InfixExpression = struct {
    token: token.Token,
    left: *const Expression,
    operator: []const u8,
    right: *const Expression,

    pub fn init(expr: *const Expression) InfixExpression {
        return expr.infix_expr;
    }

    pub fn token_literal(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn expression_node(self: *const InfixExpression) Expression {
        return self.right.*; // unsure whether this is true...
    }

    pub fn string(self: *const InfixExpression, writer: anytype) !void {
        try writer.writeByte('(');
        try self.left.string(writer);
        try writer.writeByte(' ');
        try writer.writeAll(self.operator);
        try writer.writeByte(' ');
        try self.right.string(writer);
        try writer.writeByte(')');
    }
};

pub const Boolean = struct {
    token: token.Token,
    value: bool,

    pub fn init(expr: *const Expression) Boolean {
        return expr.bool_expr;
    }

    pub fn expression_node(_: *const Boolean) void {
        return;
    }

    pub fn token_literal(self: *const Boolean) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const Boolean, writer: anytype) !void {
        try writer.writeAll(self.token_literal());
    }
};

pub const IfExpression = struct {
    token: token.Token,
    condition: *const Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,

    pub fn init(expr: *const Expression) IfExpression {
        return expr.if_expr;
    }

    pub fn expression_node(_: *const IfExpression) void {
        return;
    }

    pub fn token_literal(self: *const IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const IfExpression, writer: anytype) !void {
        try writer.writeAll("if");

        try self.condition.string(writer);

        try writer.writeByte(' ');
        try self.consequence.string(writer);

        if (self.alternative != null) {
            try writer.writeAll("else ");
            try self.alternative.?.string(writer);
        }
    }
};

pub const BlockStatement = struct {
    token: token.Token,
    statements: std.ArrayList(Statement),

    pub fn deinit(self: *BlockStatement, allocator: std.mem.Allocator) void {
        for (self.statements.items) |stmt| {
            stmt.deinit(allocator);
        }
        self.statements.deinit();
    }

    pub fn statement_node(_: *BlockStatement) void {
        return;
    }

    pub fn token_literal(self: *BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *BlockStatement, writer: anytype) !void {
        for (self.statements.items) |stmt| {
            try stmt.string(writer);
        }
    }
};

pub const FunctionLiteral = struct {
    token: token.Token,
    parameters: std.ArrayList(Identifier),
    body: *BlockStatement,

    pub fn init(expr: *const Expression) FunctionLiteral {
        return expr.func_literal;
    }

    pub fn deinit(self: *const FunctionLiteral, allocator: std.mem.Allocator) void {
        self.parameters.deinit();
        self.body.deinit(allocator);
        allocator.destroy(self.body);
    }

    pub fn expression_node(_: *const FunctionLiteral) void {
        return;
    }

    pub fn token_literal(self: *const FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const FunctionLiteral, writer: anytype) !void {
        try writer.writeAll(self.token_literal());
        try writer.writeByte('(');

        for (self.parameters.items, 0..) |param, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.writeAll(param.value);
        }

        try writer.writeByte(')');
        try self.body.string(writer);
    }
};

pub const CallExpression = struct {
    token: token.Token,
    function: *const Expression,
    arguments: std.ArrayList(Expression),

    pub fn init(expr: *const Expression) CallExpression {
        return expr.call_expr;
    }

    pub fn deinit(self: *const CallExpression, allocator: std.mem.Allocator) void {
        for (self.arguments.items) |args| {
            args.deinit(allocator);
        }

        self.arguments.deinit();

        self.function.deinit(allocator);
        allocator.destroy(self.function);
    }

    pub fn expression_node(_: *const CallExpression) void {
        return;
    }

    pub fn token_literal(self: *const CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const CallExpression, writer: anytype) !void {
        try self.function.string(writer);
        try writer.writeByte('(');

        for (self.arguments.items, 0..) |param, i| {
            if (i > 0) try writer.writeAll(", ");
            try param.string(writer);
        }

        try writer.writeByte(')');
    }
};

test "test string" {
    const alloc = std.testing.allocator;

    var program = Program.init(alloc);
    defer program.deinit();

    const let_statement: LetStatement = .{
        .token = .{
            .type = token.TokenType.let,
            .literal = "let",
        },
        .name = .{
            .token = .{
                .type = token.TokenType.ident,
                .literal = "my_var",
            },
            .value = "my_var",
        },
        .value = .{
            .identifier = .{
                .token = .{
                    .type = token.TokenType.ident,
                    .literal = "another_var",
                },
                .value = "another_var",
            },
        },
    };

    const stmt: Statement = .{ .let_stmt = let_statement };

    try program.statements.append(stmt);

    const actual = try program.string();
    try std.testing.expectEqualStrings("let my_var = another_var;", actual);
    alloc.free(actual);
}
