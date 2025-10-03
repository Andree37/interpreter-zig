const std = @import("std");

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");

pub fn eval_program(program: *ast.Program) ?object.Object {
    return eval_statements(program.statements);
}

fn eval_statements(stmts: std.ArrayList(ast.Statement)) ?object.Object {
    var result: ?object.Object = null;

    for (stmts.items) |stmt| {
        result = eval_stmt(stmt);
    }

    return result;
}

fn eval_stmt(stmt: ast.Statement) ?object.Object {
    // TODO: need to eval other things if they are the one active
    switch (stmt) {
        .expr_stmt => |expr_stmt| return eval_expr_stmt(expr_stmt),
        .let_stmt => |_| return null,
        .return_stmt => |_| return null,
    }
}

fn eval_expr_stmt(expr_stmt: ast.ExpressionStatement) ?object.Object {
    return eval_expr(expr_stmt.expression);
}

fn eval_expr(expr: ast.Expression) ?object.Object {
    switch (expr) {
        .integer_literal => |int| return object.Object{
            .integer_obj = object.Integer{
                .value = int.value,
            },
        },
        .bool_expr => return null,
        .call_expr => return null,
        .func_literal => return null,
        .identifier => return null,
        .if_expr => return null,
        .infix_expr => return null,
        .prefix_expr => return null,
    }
}

fn test_eval(input: []const u8) !?object.Object {
    var l = lexer.Lexer.init(input);
    var p = try parser.Parser.init(std.testing.allocator, &l);
    defer p.deinit();

    var program = try p.parse_program();
    defer program.deinit();

    return eval_program(&program);
}

fn test_integer_object(obj: object.Object, expected: i64) bool {
    return obj.integer_obj.value == expected;
}

test "test eval integer expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "69", .expected = 69 },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        try std.testing.expect(test_integer_object(evaluated.?, t.expected));
    }
}
