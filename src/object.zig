const std = @import("std");

pub const ObjectType = enum {
    integer_obj,
    boolean_obj,
    null_obj,
    return_obj,
    error_obj,
};

pub const Object = union(enum) {
    integer_obj: Integer,
    boolean_obj: Boolean,
    null_obj: Null,
    return_obj: Return,
    error_obj: Error,

    pub fn object_type(self: *const Object) ObjectType {
        return switch (self.*) {
            .integer_obj => ObjectType.integer_obj,
            .boolean_obj => ObjectType.boolean_obj,
            .null_obj => ObjectType.null_obj,
            .return_obj => ObjectType.return_obj,
            .error_obj => ObjectType.error_obj,
        };
    }

    pub fn inspect(self: *const Object, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self.*) {
            .integer_obj => |obj| try obj.inspect(allocator),
            .boolean_obj => |obj| try obj.inspect(allocator),
            .null_obj => |obj| try obj.inspect(),
            .return_obj => |obj| obj.inspect(allocator),
            .error_obj => |obj| try obj.inspect(allocator),
        };
    }

    pub fn new_error(allocator: std.mem.Allocator, comptime format: []const u8, args: anytype) Object {
        const msg = std.fmt.allocPrint(allocator, format, args) catch return Object{ .null_obj = .{} };
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

// NOT OBJECTS

pub const Environment = struct {
    store: std.StringHashMap(Object),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Environment {
        const map = std.StringHashMap(Object).init(allocator);
        return .{ .store = map, .allocator = allocator };
    }

    pub fn deinit(self: *Environment) void {
        var it = self.store.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.store.deinit();
    }
};
