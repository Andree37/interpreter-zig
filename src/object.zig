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
            .integer_obj => |obj| obj.object_type,
            .boolean_obj => |obj| obj.object_type,
            .null_obj => |obj| obj.object_type,
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
    const object_type: ObjectType = ObjectType.integer_obj;
    value: i64,

    pub fn inspect(self: *const Integer, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{d}", .{self.value}) catch return "";
    }
};

pub const Boolean = struct {
    const object_type: ObjectType = ObjectType.boolean_obj;
    value: bool,

    pub fn inspect(self: *const Boolean, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{}", .{self.value}) catch return "";
    }
};

pub const Null = struct {
    const object_type: ObjectType = ObjectType.null_obj;

    pub fn inspect(_: *const Null) ![]const u8 {
        return "null";
    }
};
