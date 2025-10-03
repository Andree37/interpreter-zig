const std = @import("std");

pub const ObjectType = enum {
    integer_obj,
    boolean_obj,
    null_obj,
};

pub const Object = union(enum) {
    integer_obj: Integer,
    boolean_obj: Boolean,
    null_obj: Null,

    pub fn object_type(self: *const Object) ObjectType {
        return switch (self.*) {
            .integer_obj => ObjectType.integer_obj,
            .boolean_obj => ObjectType.boolean_obj,
            .null_obj => ObjectType.null_obj,
        };
    }

    pub fn inspect(self: *const Object, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self.*) {
            .integer_obj => |obj| try obj.inspect(allocator),
            .boolean_obj => |obj| try obj.inspect(allocator),
            .null_obj => |obj| try obj.inspect(),
        };
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
