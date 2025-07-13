const std = @import("std");
const token = @import("token.zig");

// Node interface
const Node = struct {
    token_literal: fn () []const u8,
};

// Statemenet interface
const Statement = struct {
    node: Node,
    statement_node: fn () void,
};

// Expression interface
const Expression = struct {
    node: Node,
    expression_node: fn () void,
};

const Program = struct {
    statements: std.ArrayList(Statement),

    fn token_literal(self: *Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].node.token_literal();
        } else {
            return "";
        }
    }
};

const Identifier = struct {
    token: token.Token, // the token.IDENT token
    value: []const u8,

    // fn expression_node(self: *Identifier) void {}

    fn token_literal(self: *Identifier) []const u8 {
        return self.token.literal;
    }
};

const LetStatement = struct {
    token: token.Token,
    name: *Identifier,
    value: Expression,

    // fn statement_node(self: *LetStatement) void!anyerror {}

    fn token_literal(self: *LetStatement) []const u8 {
        return self.token.literal;
    }
};
