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
};

pub const Expression = union(enum) {
    identifier: Identifier,

    pub fn token_literal(self: *const Expression) []const u8 {
        return switch (self.*) {
            .identifier => |ident| ident.token_literal(),
        };
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) Program {
        return Program{ .statements = std.ArrayList(Statement).init(allocator) };
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
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
};

pub const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    value: Expression,

    // fn statement_node(self: *LetStatement) void!anyerror {}

    pub fn init(stmt: Statement) LetStatement {
        return stmt.let_stmt;
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

    pub fn token_literal(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }
};
