const std = @import("std");

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");

const TRUE = object.Object{
    .boolean_obj = .{
        .value = true,
    },
};
const FALSE = object.Object{
    .boolean_obj = .{
        .value = false,
    },
};
const NULL = object.Object{
    .null_obj = .{},
};

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

fn native_bool_to_boolean_object(input: bool) object.Object {
    return if (input) TRUE else FALSE;
}

fn eval_bang_operator_expression(right: object.Object) object.Object {
    switch (right) {
        .boolean_obj => |bool_obj| return native_bool_to_boolean_object(!bool_obj.value),
        .integer_obj => return FALSE,
        .null_obj => return FALSE,
    }
}

fn eval_minus_operator_expression(right: object.Object) object.Object {
    switch (right) {
        .integer_obj => |int_obj| return object.Object{
            .integer_obj = .{
                .value = -int_obj.value,
            },
        },
        .boolean_obj => return NULL,
        .null_obj => return NULL,
    }
}

fn eval_prefix_expression(operator: []const u8, right: object.Object) ?object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return eval_bang_operator_expression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return eval_minus_operator_expression(right);
    }
    return NULL;
}

fn eval_infix_expression(operator: []const u8, left: object.Object, right: object.Object) object.Object {
    // left int, right int
    if (left.object_type() == object.ObjectType.integer_obj and right.object_type() == object.ObjectType.integer_obj) {
        return eval_integer_infix_expression(operator, left, right);
    }

    // left bool, right bool
    if (left.object_type() == object.ObjectType.boolean_obj and right.object_type() == object.ObjectType.boolean_obj) {
        return eval_boolean_infix_expression(operator, left, right);
    }

    return NULL;
}

fn eval_boolean_infix_expression(operator: []const u8, left: object.Object, right: object.Object) object.Object {
    const left_val = left.boolean_obj.value;
    const right_val = right.boolean_obj.value;

    if (std.mem.eql(u8, operator, "==")) {
        return object.Object{ .boolean_obj = .{ .value = left_val == right_val } };
    } else if (std.mem.eql(u8, operator, "!=")) {
        return object.Object{ .boolean_obj = .{ .value = left_val != right_val } };
    }

    return NULL;
}

fn eval_integer_infix_expression(operator: []const u8, left: object.Object, right: object.Object) object.Object {
    const left_val = left.integer_obj.value;
    const right_val = right.integer_obj.value;

    if (std.mem.eql(u8, operator, "+")) {
        return object.Object{ .integer_obj = .{ .value = left_val + right_val } };
    } else if (std.mem.eql(u8, operator, "-")) {
        return object.Object{ .integer_obj = .{ .value = left_val - right_val } };
    } else if (std.mem.eql(u8, operator, "*")) {
        return object.Object{ .integer_obj = .{ .value = left_val * right_val } };
    } else if (std.mem.eql(u8, operator, "/")) {
        return object.Object{ .integer_obj = .{ .value = @divFloor(left_val, right_val) } };
    } else if (std.mem.eql(u8, operator, "<")) {
        return native_bool_to_boolean_object(left_val < right_val);
    } else if (std.mem.eql(u8, operator, ">")) {
        return native_bool_to_boolean_object(left_val > right_val);
    } else if (std.mem.eql(u8, operator, "==")) {
        return native_bool_to_boolean_object(left_val == right_val);
    } else if (std.mem.eql(u8, operator, "!=")) {
        return native_bool_to_boolean_object(left_val != right_val);
    }

    return NULL;
}

fn eval_expr(expr: ast.Expression) ?object.Object {
    switch (expr) {
        .integer_literal => |int| return object.Object{
            .integer_obj = object.Integer{
                .value = int.value,
            },
        },
        .bool_expr => |boolean| return if (boolean.value) TRUE else FALSE,
        .call_expr => return null,
        .func_literal => return null,
        .identifier => return null,
        .if_expr => return null,
        .infix_expr => |infix_expr| {
            const left = eval_expr(infix_expr.left.*);
            const right = eval_expr(infix_expr.right.*);
            return eval_infix_expression(infix_expr.operator, left.?, right.?);
        },
        .prefix_expr => |prefix_expr| {
            const right = eval_expr(prefix_expr.right.*);
            return eval_prefix_expression(prefix_expr.operator, right.?);
        },
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

fn test_bool_object(obj: object.Object, expected: bool) bool {
    return obj.boolean_obj.value == expected;
}

test "test eval integer expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "69", .expected = 69 },
        .{ .input = "-69", .expected = -69 },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = 32 },
        .{ .input = "-50 + 100 + -50", .expected = 0 },
        .{ .input = "5 * 2 + 10", .expected = 20 },
        .{ .input = "5 + 2 * 10", .expected = 25 },
        .{ .input = "20 + 2 * -10", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        .{ .input = "2 * (5 + 10)", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        try std.testing.expect(test_integer_object(evaluated.?, t.expected));
    }
}

test "test eval boolean expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 < 1", .expected = false },
        .{ .input = "1 > 1", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "1 != 2", .expected = true },
        .{ .input = "1 == 2", .expected = false },
        .{ .input = "true == true", .expected = true },
        .{ .input = "false == false", .expected = true },
        .{ .input = "true == false", .expected = false },
        .{ .input = "false == true", .expected = false },
        .{ .input = "true != true", .expected = false },
        .{ .input = "true != false", .expected = true },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 < 2) == false", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 > 2) == false", .expected = true },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        try std.testing.expect(test_bool_object(evaluated.?, t.expected));
    }
}

test "test bang operator" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
        .{ .input = "!!5", .expected = true },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        try std.testing.expect(test_bool_object(evaluated.?, t.expected));
    }
}
