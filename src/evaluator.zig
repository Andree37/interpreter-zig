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

pub fn eval_program(program: *ast.Program, allocator: std.mem.Allocator, env: *object.Environment) ?object.Object {
    return eval_statements(env, allocator, program.statements);
}

fn eval_statements(env: *object.Environment, allocator: std.mem.Allocator, stmts: std.ArrayList(ast.Statement)) ?object.Object {
    var result: ?object.Object = null;

    for (stmts.items) |stmt| {
        result = eval_stmt(env, allocator, stmt);
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

fn eval_stmt(env: *object.Environment, allocator: std.mem.Allocator, stmt: ast.Statement) ?object.Object {
    switch (stmt) {
        .expr_stmt => |expr_stmt| return eval_expr(env, allocator, expr_stmt.expression),
        .let_stmt => |let_stmt| {
            const obj = eval_expr(env, allocator, let_stmt.value);
            if (obj != null) {
                if (is_error(obj.?)) {
                    return obj;
                }
                const key = allocator.dupe(u8, let_stmt.name.value) catch return null; //dupe as we are freeing elsewhere
                env.set(key, obj.?) catch return null;
            }
            return obj;
        },
        .return_stmt => |return_stmt| {
            const val = eval_expr(env, allocator, return_stmt.return_value);
            if (val == null) {
                return null;
            }
            const obj = allocator.create(object.Object) catch return null;
            obj.* = val.?;
            return object.Object{ .return_obj = .{ .value = obj } };
        },
    }
}

fn eval_identifier(env: *object.Environment, allocator: std.mem.Allocator, ident: ast.Identifier) ?object.Object {
    const key = allocator.dupe(u8, ident.value) catch return null;
    defer allocator.free(key);

    const val = env.get(key);
    if (val == null) {
        return object.Object.new_error(allocator, "identifier not found: {s}", .{ident.value});
    }

    return val;
}

fn native_bool_to_boolean_object(input: bool) object.Object {
    return if (input) TRUE else FALSE;
}

fn eval_bang_operator_expression(right: object.Object) object.Object {
    switch (right) {
        .boolean_obj => |bool_obj| return native_bool_to_boolean_object(!bool_obj.value),
        .integer_obj, .error_obj, .null_obj, .return_obj, .function_obj => return FALSE,
    }
}

fn eval_minus_operator_expression(allocator: std.mem.Allocator, right: object.Object) object.Object {
    switch (right) {
        .integer_obj => |int_obj| return object.Object{
            .integer_obj = .{
                .value = -int_obj.value,
            },
        },
        .boolean_obj, .error_obj, .null_obj, .return_obj, .function_obj => return object.Object.new_error(allocator, "unknown operator: -{s}", .{@tagName(right.object_type())}),
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

fn eval_block_statement_expr(env: *object.Environment, allocator: std.mem.Allocator, block_expr: ast.BlockStatement) ?object.Object {
    var result: ?object.Object = null;

    for (block_expr.statements.items) |stmt| {
        result = eval_stmt(env, allocator, stmt);
        if (result == null) {
            return null;
        }

        if (result.? == .return_obj) {
            return result;
        } else if (result.? == .error_obj) {
            return result;
        }
    }

    return result;
}

fn eval_if_expression(env: *object.Environment, allocator: std.mem.Allocator, if_expr: ast.IfExpression) ?object.Object {
    const condition = eval_expr(env, allocator, if_expr.condition.*);
    if (is_truthy(condition.?)) {
        return eval_block_statement_expr(env, allocator, if_expr.consequence.*);
    } else if (if_expr.alternative != null) {
        return eval_block_statement_expr(env, allocator, if_expr.alternative.?.*);
    }

    return NULL;
}

fn is_truthy(obj: object.Object) bool {
    return switch (obj) {
        .boolean_obj => |boolean| return boolean.value,
        .integer_obj => return true,
        .null_obj, .error_obj, .return_obj, .function_obj => return false,
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

fn eval_expressions(env: *object.Environment, allocator: std.mem.Allocator, exprs: std.ArrayList(ast.Expression)) !std.ArrayList(object.Object) {
    var results = std.ArrayList(object.Object).init(allocator);

    for (exprs.items) |exp| {
        const evaluated = eval_expr(env, allocator, exp);
        if (is_error(evaluated.?)) {
            results.clearAndFree();
            try results.append(evaluated.?);
            return results;
        }
        try results.append(evaluated.?);
    }
    return results;
}

fn apply_function(allocator: std.mem.Allocator, fun: object.Object, args: std.ArrayList(object.Object)) !object.Object {
    const function = fun.function_obj;

    const extendedEnv = try extend_function_env(allocator, function, args);
    defer extendedEnv.deinit_enclosed();

    const evaluated = eval_block_statement_expr(extendedEnv, allocator, function.body.*);

    if (evaluated == null) {
        return NULL;
    }

    if (evaluated.? == .return_obj) {
        defer evaluated.?.return_obj.deinit(allocator);
        return evaluated.?.return_obj.value.*;
    }

    return evaluated.?;
}

fn extend_function_env(allocator: std.mem.Allocator, fun: object.Function, args: std.ArrayList(object.Object)) !*object.Environment {
    const env = try object.Environment.init_enclosed(allocator, fun.env);

    for (fun.parameters.items, 0..) |param, i| {
        const key = try allocator.dupe(u8, param.value);
        try env.set(key, args.items[i]);
    }

    return env;
}

fn eval_expr(env: *object.Environment, allocator: std.mem.Allocator, expr: ast.Expression) ?object.Object {
    switch (expr) {
        .integer_literal => |int| return object.Object{
            .integer_obj = object.Integer{
                .value = int.value,
            },
        },
        .bool_expr => |boolean| return if (boolean.value) TRUE else FALSE,
        .call_expr => |call_expr| {
            const fun = eval_expr(env, allocator, call_expr.function.*);
            if (is_error(fun.?)) {
                return fun;
            }

            const args = eval_expressions(env, allocator, call_expr.arguments) catch return object.Object.new_error(allocator, "woop something went wrong...", .{});
            defer args.deinit();
            if (args.items.len == 1 and is_error(args.items[0])) {
                return args.items[0];
            }
            return apply_function(allocator, fun.?, args) catch return object.Object.new_error(allocator, "woop something went wrong...", .{});
        },
        .func_literal => |func_literal| {
            const params = func_literal.parameters;
            const body = func_literal.body;

            return object.Object{ .function_obj = .{ .body = body, .parameters = params, .env = env } };
        },
        .identifier => |ident| return eval_identifier(env, allocator, ident),
        .if_expr => |if_expr| return eval_if_expression(env, allocator, if_expr),
        .infix_expr => |infix_expr| {
            const left = eval_expr(env, allocator, infix_expr.left.*);
            if (is_error(left.?)) {
                return left;
            }
            const right = eval_expr(env, allocator, infix_expr.right.*);
            if (is_error(right.?)) {
                return right;
            }
            return eval_infix_expression(allocator, infix_expr.operator, left.?, right.?);
        },
        .prefix_expr => |prefix_expr| {
            const right = eval_expr(env, allocator, prefix_expr.right.*);
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

    var env = try object.Environment.init(std.testing.allocator);
    defer env.deinit();

    return eval_program(&program, std.testing.allocator, env);
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
                .function_obj => unreachable,
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
        .{ .input = "foobar", .expected = "identifier not found: foobar" },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        defer std.testing.allocator.free(evaluated.?.error_obj.message);

        std.debug.print("testing input: {s} expected: {s} and got {any}\n", .{ t.input, t.expected, evaluated });
        try std.testing.expectEqualStrings(t.expected, evaluated.?.error_obj.message);
    }
}

test "test let statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; b;", .expected = 5 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);

        std.debug.print("\nTest: {s}\n", .{t.input});
        std.debug.print("Expected integer: {d}\n", .{t.expected});
        std.debug.print("Got: {any}\n", .{evaluated.?});

        try std.testing.expect(test_integer_object(evaluated.?, t.expected));
    }
}

test "test function object" {
    const input = "fn(x) { x + 2; };";

    var l = lexer.Lexer.init(input);
    var p = try parser.Parser.init(std.testing.allocator, &l);
    defer p.deinit();

    var program = try p.parse_program();
    defer program.deinit();

    var env = try object.Environment.init(std.testing.allocator);
    defer env.deinit();

    const evaluated = eval_program(&program, std.testing.allocator, env);

    const fun = evaluated.?.function_obj;

    try std.testing.expect(fun.parameters.items.len == 1);
    try std.testing.expectEqualStrings("x", fun.parameters.items[0].value);

    var body_str = std.ArrayList(u8).init(std.testing.allocator);
    defer body_str.deinit();
    try fun.body.string(body_str.writer());

    try std.testing.expectEqualStrings("(x + 2)", body_str.items);
}

test "test function application" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = 5 },
        .{ .input = "let double = fn(x) { return x * 2; }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add (5, 5));", .expected = 20 },
        .{ .input = "fn(x) { x; }(5)", .expected = 5 },
    };

    for (tests) |t| {
        const evaluated = try test_eval(t.input);
        try std.testing.expect(test_integer_object(evaluated.?, t.expected));
    }
}
