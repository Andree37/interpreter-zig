const std = @import("std");

const ast = @import("ast.zig");

var global_environments: ?std.ArrayList(*Environment) = null;
pub var global_allocator: ?std.mem.Allocator = null;

pub fn init_environment_tracking(allocator: std.mem.Allocator) void {
    global_allocator = allocator;
    global_environments = std.ArrayList(*Environment).init(allocator);
}

pub fn deinit_environment_tracking() void {
    if (global_environments) |*envs| {
        for (envs.items) |env| {
            env.deinit_enclosed();
        }
        envs.deinit();
        global_environments = null;
        global_allocator = null;
    }
}

fn track_environment(env: *Environment) void {
    if (global_environments) |*envs| {
        envs.append(env) catch return;
    }
}

pub const ObjectType = enum {
    integer_obj,
    boolean_obj,
    null_obj,
    return_obj,
    error_obj,
    function_obj,
};

pub const Object = union(enum) {
    integer_obj: Integer,
    boolean_obj: Boolean,
    null_obj: Null,
    return_obj: Return,
    error_obj: Error,
    function_obj: Function,

    pub fn object_type(self: *const Object) ObjectType {
        return switch (self.*) {
            .integer_obj => ObjectType.integer_obj,
            .boolean_obj => ObjectType.boolean_obj,
            .null_obj => ObjectType.null_obj,
            .return_obj => ObjectType.return_obj,
            .error_obj => ObjectType.error_obj,
            .function_obj => ObjectType.function_obj,
        };
    }

    pub fn inspect(self: *const Object, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self.*) {
            .integer_obj => |obj| try obj.inspect(allocator),
            .boolean_obj => |obj| try obj.inspect(allocator),
            .null_obj => |obj| try obj.inspect(),
            .return_obj => |obj| obj.inspect(allocator),
            .error_obj => |obj| try obj.inspect(allocator),
            .function_obj => |obj| try obj.inspect(allocator),
        };
    }

    pub fn new_error(comptime format: []const u8, args: anytype) Object {
        const msg = std.fmt.allocPrint(global_allocator.?, format, args) catch return Object{ .null_obj = .{} };
        const err = Error{ .message = msg };
        return Object{ .error_obj = err };
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: *const Integer, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{d}", .{self.value}) catch return "";
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: *const Boolean, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{}", .{self.value}) catch return "";
    }
};

pub const Null = struct {
    pub fn inspect(_: *const Null) ![]const u8 {
        return "null";
    }
};

pub const Return = struct {
    value: *const Object,

    pub fn inspect(self: *const Return, allocator: std.mem.Allocator) []const u8 {
        return self.value.inspect(allocator) catch return "";
    }

    pub fn deinit(self: *const Return, allocator: std.mem.Allocator) void {
        allocator.destroy(self.value);
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn inspect(self: *const Error, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "ERROR: {s}", .{self.message}) catch return "";
    }

    pub fn deinit(self: *const Error, allocator: std.mem.Allocator) void {
        allocator.free(self.message);
    }
};

pub const Function = struct {
    parameters: std.ArrayList(ast.Identifier),
    body: *ast.BlockStatement,
    env: *Environment,

    pub fn inspect(self: *const Function, allocator: std.mem.Allocator) ![]const u8 {
        var params = std.ArrayList(u8).init(allocator);
        defer params.deinit();

        var body = std.ArrayList(u8).init(allocator);
        defer body.deinit();

        const params_writer = params.writer();
        for (self.parameters.items, 0..) |param, i| {
            if (i > 0) params_writer.writeAll(", ") catch return "";
            params_writer.writeAll(param.value) catch return "";
        }

        try self.body.string(body.writer());

        var result = std.ArrayList(u8).init(allocator);
        const writer = result.writer();

        std.fmt.format(writer, "fn({s}) {{\n{s}\n}}", .{ params.items, body.items }) catch return "";

        return result.toOwnedSlice() catch return "";
    }
};

// NOT OBJECTS

pub const Environment = struct {
    outer: ?*Environment,
    store: std.StringHashMap(Object),
    allocator: std.mem.Allocator,
    is_tracked: bool,

    pub fn init_enclosed(allocator: std.mem.Allocator, outer: *Environment) !*Environment {
        const map = std.StringHashMap(Object).init(allocator);
        const env = try allocator.create(Environment);

        env.* = .{ .store = map, .allocator = allocator, .outer = outer, .is_tracked = false };
        return env;
    }

    pub fn init(allocator: std.mem.Allocator) !*Environment {
        const map = std.StringHashMap(Object).init(allocator);
        const env = try allocator.create(Environment);
        env.* = .{ .store = map, .allocator = allocator, .outer = null, .is_tracked = false };
        return env;
    }

    pub fn mark_for_tracking(self: *Environment) void {
        if (!self.is_tracked) {
            self.is_tracked = true;
            track_environment(self);
        }
    }

    pub fn deinit(self: *Environment) void {
        self.deinit_recursive();
    }

    fn deinit_recursive(self: *Environment) void {
        var it = self.store.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        if (self.outer) |outer| {
            outer.deinit_recursive();
        }
        self.store.deinit();
        self.allocator.destroy(self);
    }

    pub fn deinit_enclosed(self: *Environment) void {
        var it = self.store.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.store.deinit();
        self.allocator.destroy(self);
    }

    pub fn get(self: *Environment, key: []const u8) ?Object {
        const obj = self.store.get(key);
        if (obj == null and self.outer != null) {
            return self.outer.?.get(key);
        }

        return obj;
    }

    pub fn set(self: *Environment, key: []const u8, val: Object) !void {
        try self.store.put(key, val);
    }
};
