const std = @import("std");
const token = @import("token.zig");

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
            .expr_stmt => |stmt| stmt.string(),
        };
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,

    pub fn token_literal(self: *const Expression) []const u8 {
        return switch (self.*) {
            .identifier => |ident| ident.token_literal(),
        };
    }

    pub fn string(self: *const Expression) []const u8 {
        return self.identifier.string();
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

    pub fn string(self: *const Identifier) []const u8 {
        return self.value;
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
        try writer.print("{s} {s} = {s};", .{
            self.token_literal(),
            self.name.string(),
            self.value.string(),
        });
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
        try writer.print("{s} ;", .{
            self.token_literal(),
        });
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

    pub fn string(_: *const ExpressionStatement) void {
        // TODO: return self.expression.string();
        return;
    }
};

test "test string" {
    const alloc = std.testing.allocator;

    var program = Program.init(alloc);
    defer program.deinit();

    // let my_var = another_var;

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
