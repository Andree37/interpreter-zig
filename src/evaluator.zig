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

fn is_error(obj: object.Object) bool {
    return obj == .error_obj;
}

pub fn eval_program(program: *ast.Program, allocator: std.mem.Allocator) ?object.Object {
    return eval_statements(allocator, program.statements);
}

fn eval_statements(allocator: std.mem.Allocator, stmts: std.ArrayList(ast.Statement)) ?object.Object {
    var result: ?object.Object = null;

    for (stmts.items) |stmt| {
        result = eval_stmt(allocator, stmt);
        if (result == null) {
            return null;
        }

        if (result.? == .return_obj) {
            defer result.?.return_obj.deinit(allocator);
            return result.?.return_obj.value.*;
        } else if (result.? == .error_obj) {
            return result;
        }
    }

    return result;
}

fn eval_stmt(allocator: std.mem.Allocator, stmt: ast.Statement) ?object.Object {
    switch (stmt) {
        .expr_stmt => |expr_stmt| return eval_expr(allocator, expr_stmt.expression),
        .let_stmt => |_| return null,
        .return_stmt => |return_stmt| {
            const val = eval_expr(allocator, return_stmt.return_value);
            if (val == null) {
                return null;
            }
            const obj = allocator.create(object.Object) catch return null;
            obj.* = val.?;
            return object.Object{ .return_obj = .{ .value = obj } };
        },
    }
}

fn native_bool_to_boolean_object(input: bool) object.Object {
    return if (input) TRUE else FALSE;
}

fn eval_bang_operator_expression(right: object.Object) object.Object {
    switch (right) {
        .boolean_obj => |bool_obj| return native_bool_to_boolean_object(!bool_obj.value),
        .integer_obj, .error_obj, .null_obj, .return_obj => return FALSE,
    }
}

fn eval_minus_operator_expression(allocator: std.mem.Allocator, right: object.Object) object.Object {
    switch (right) {
        .integer_obj => |int_obj| return object.Object{
            .integer_obj = .{
                .value = -int_obj.value,
            },
        },
        .boolean_obj, .error_obj, .null_obj, .return_obj => return object.Object.new_error(allocator, "unknown operator: -{s}", .{@tagName(right.object_type())}),
    }
}

fn eval_prefix_expression(allocator: std.mem.Allocator, operator: []const u8, right: object.Object) ?object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return eval_bang_operator_expression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return eval_minus_operator_expression(allocator, right);
    }

    return object.Object.new_error(allocator, "unknown operator: {s}{any}", .{ operator, @tagName(right.object_type()) });
}

fn eval_block_statement_expr(allocator: std.mem.Allocator, block_expr: ast.BlockStatement) ?object.Object {
    return eval_statements(allocator, block_expr.statements);
}

fn eval_if_expression(allocator: std.mem.Allocator, if_expr: ast.IfExpression) ?object.Object {
    const condition = eval_expr(allocator, if_expr.condition.*);
    if (is_truthy(condition.?)) {
        return eval_block_statement_expr(allocator, if_expr.consequence.*);
    } else if (if_expr.alternative != null) {
        return eval_block_statement_expr(allocator, if_expr.alternative.?.*);
    }

    return NULL;
}

fn is_truthy(obj: object.Object) bool {
    return switch (obj) {
        .boolean_obj => |boolean| return boolean.value,
        .integer_obj => return true,
        .null_obj, .error_obj, .return_obj => return false,
    };
}

fn eval_infix_expression(allocator: std.mem.Allocator, operator: []const u8, left: object.Object, right: object.Object) object.Object {
    // left int, right int
    if (left.object_type() == object.ObjectType.integer_obj and right.object_type() == object.ObjectType.integer_obj) {
        return eval_integer_infix_expression(allocator, operator, left, right);
    }

    // left bool, right bool
    if (left.object_type() == object.ObjectType.boolean_obj and right.object_type() == object.ObjectType.boolean_obj) {
        return eval_boolean_infix_expression(allocator, operator, left, right);
    }

    if (left.object_type() != right.object_type()) {
        return object.Object.new_error(allocator, "type mismatch: {s} {s} {s}", .{ @tagName(left.object_type()), operator, @tagName(right.object_type()) });
    }

    return object.Object.new_error(allocator, "unknown operator: {s}{s}", .{ operator, @tagName(right.object_type()) });
}

fn eval_boolean_infix_expression(allocator: std.mem.Allocator, operator: []const u8, left: object.Object, right: object.Object) object.Object {
    const left_val = left.boolean_obj.value;
    const right_val = right.boolean_obj.value;

    if (std.mem.eql(u8, operator, "==")) {
        return object.Object{ .boolean_obj = .{ .value = left_val == right_val } };
    } else if (std.mem.eql(u8, operator, "!=")) {
        return object.Object{ .boolean_obj = .{ .value = left_val != right_val } };
    }

    return object.Object.new_error(allocator, "unknown operator: {s} {s} {s}", .{ @tagName(left.object_type()), operator, @tagName(right.object_type()) });
}

fn eval_integer_infix_expression(allocator: std.mem.Allocator, operator: []const u8, left: object.Object, right: object.Object) object.Object {
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

    return object.Object.new_error(allocator, "unknown operator: {s} {s} {s}", .{ @tagName(left.object_type()), operator, @tagName(right.object_type()) });
}

fn eval_expr(allocator: std.mem.Allocator, expr: ast.Expression) ?object.Object {
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
        .if_expr => |if_expr| return eval_if_expression(allocator, if_expr),
        .infix_expr => |infix_expr| {
            const left = eval_expr(allocator, infix_expr.left.*);
            if (is_error(left.?)) {
                return left;
            }
            const right = eval_expr(allocator, infix_expr.right.*);
            if (is_error(right.?)) {
                return right;
            }
            return eval_infix_expression(allocator, infix_expr.operator, left.?, right.?);
        },
        .prefix_expr => |prefix_expr| {
            const right = eval_expr(allocator, prefix_expr.right.*);
            if (is_error(right.?)) {
                return right;
            }
            return eval_prefix_expression(allocator, prefix_expr.operator, right.?);
        },
    }
}

fn test_eval(input: []const u8) !?object.Object {
    var l = lexer.Lexer.init(input);
    var p = try parser.Parser.init(std.testing.allocator, &l);
    defer p.deinit();

    var program = try p.parse_program();
    defer program.deinit();

    return eval_program(&program, std.testing.allocator);
}

fn test_integer_object(obj: object.Object, expected: i64) bool {
    return obj.integer_obj.value == expected;
}

fn test_bool_object(obj: object.Object, expected: bool) bool {
    return obj.boolean_obj.value == expected;
}

fn test_null_object(obj: object.Object) bool {
    return obj == .null_obj;
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

test "test if else expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: object.Object,
    }{
        .{ .input = "if (true) { 10 }", .expected = object.Object{ .integer_obj = .{ .value = 10 } } },
        .{ .input = "if (false) { 10 }", .expected = object.Object{ .null_obj = .{} } },
        .{ .input = "if (1) { 10 }", .expected = object.Object{ .integer_obj = .{ .value = 10 } } },
        .{ .input = "if (1 < 2) { 10 }", .expected = object.Object{ .integer_obj = .{ .value = 10 } } },
        .{ .input = "if (1 > 2) { 10 }", .expected = object.Object{ .null_obj = .{} } },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = object.Object{ .integer_obj = .{ .value = 20 } } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = object.Object{ .integer_obj = .{ .value = 10 } } },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        if (evaluated != null) {
            switch (evaluated.?) {
                .integer_obj => try std.testing.expect(test_integer_object(evaluated.?, t.expected.integer_obj.value)),
                .boolean_obj => unreachable,
                .return_obj => unreachable,
                .error_obj => unreachable,
                .null_obj => try std.testing.expect(test_null_object(evaluated.?)),
            }
        } else {
            unreachable;
        }
    }
}

test "test return statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "9; return 2 * 5; 69;", .expected = 10 },
        .{ .input = "if (10 > 1) { if (10 > 1) { return 10; } return 1; }", .expected = 10 },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        std.debug.print("testing {d} and got {any}\n", .{ t.expected, evaluated });
        try std.testing.expect(test_integer_object(evaluated.?, t.expected));
    }
}

test "test error handling" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "5 + true;", .expected = "type mismatch: integer_obj + boolean_obj" },
        .{ .input = "5 + true; 5;", .expected = "type mismatch: integer_obj + boolean_obj" },
        .{ .input = "-true;", .expected = "unknown operator: -boolean_obj" },
        .{ .input = "true + false;", .expected = "unknown operator: boolean_obj + boolean_obj" },
        .{ .input = "5; true + false; 5;", .expected = "unknown operator: boolean_obj + boolean_obj" },
        .{ .input = "if (10 > 1) { true + false; }", .expected = "unknown operator: boolean_obj + boolean_obj" },
        .{ .input = "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }", .expected = "unknown operator: boolean_obj + boolean_obj" },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        defer std.testing.allocator.free(evaluated.?.error_obj.message);

        std.debug.print("testing input: {s} expected: {s} and got {any}\n", .{ t.input, t.expected, evaluated });
        try std.testing.expectEqualStrings(t.expected, evaluated.?.error_obj.message);
    }
}
