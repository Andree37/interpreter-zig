const std = @import("std");
const token = @import("token.zig");

pub const Statement = union(enum) {
    let_stmt: LetStatement,
    return_stmt: ReturnStatement,
    // add return, expression, etc. later

    pub fn token_literal(self: *const Statement) []const u8 {
        return switch (self.*) {
            .let_stmt => |stmt| stmt.token_literal(),
            .return_stmt => |stmt| stmt.token_literal(),
        };
    }

    pub fn string(self: *const Statement) ![]const u8 {
        return switch (self.*) {
            .let_stmt => |stmt| try stmt.string(),
            .return_stmt => |stmt| try stmt.string(),
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

        for (self.statements.items) |item| {
            const part = try item.string();
            defer self.allocator.free(part);
            try list.appendSlice(part);
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
    allocator: std.mem.Allocator,
    token: token.Token,
    name: Identifier,
    value: Expression,

    // fn statement_node(self: *LetStatement) void!anyerror {}

    pub fn init(stmt: Statement) LetStatement {
        return stmt.let_stmt;
    }

    pub fn string(self: *const LetStatement) ![]const u8 {
        // TODO: need to add expressions
        // TODO: need to change all allocPrint to using writer...
        return try std.fmt.allocPrint(self.allocator, "{s} {s} = {s};", .{ self.token_literal(), self.name.string(), self.value.string() });
    }

    fn token_literal(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ReturnStatement = struct {
    allocator: std.mem.Allocator,
    token: token.Token,
    return_value: Expression,

    // fn statement_node(self: *ReturnStatement) void!anyerror {}

    pub fn init(stmt: Statement) ReturnStatement {
        return stmt.return_stmt;
    }

    pub fn string(self: *const ReturnStatement) ![]const u8 {
        // TODO: add expression
        return try std.fmt.allocPrint(self.allocator, "{s} ;", .{self.token_literal()});
    }

    pub fn token_literal(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ExpressionStatement = struct {
    token: token.Token,
    expression: Expression,

    // fn statement_node(self: *ExpressionStatement) void!anyerror {}

    fn token_literal(self: *const ExpressionStatement) []const u8 {
        self.token.literal;
    }

    fn string(self: *const ExpressionStatement) []const u8 {
        // TODO: return self.expression.string();
        return self.token_literal();
    }
};

test "test string" {
    const alloc = std.testing.allocator;

    var program = Program.init(alloc);
    defer program.deinit();

    // let my_var = another_var;

    const let_statement: LetStatement = .{
        .allocator = alloc,
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
