const builtin = @import("builtin");
const std = @import("std");

const logger = std.log.scoped(.tracked_value);

pub fn TrackedValue(T: type) type {
    return struct {
        const This = @This();
        const InnerType = T;

        val: ?InnerType = null,

        pub fn get(self: This) InnerType {
            if (self.val) |v| {
                return v;
            } else {
                if (builtin.mode == .Debug) {
                    @panic("read of uninitialized register value!");
                } else {
                    logger.err("read of uninitialized register value, this is likely a bug!");
                }
                return undefined;
            }
        }

        pub fn set(self: *This, val: InnerType) void {
            self.val = val;
        }

        pub inline fn maybe(self: This) ?InnerType {
            return self.val;
        }
    };
}

fn MaybeAllType(T: type) type {
    const info = @typeInfo(T);

    if (info != .@"struct") {
        @compileError("input must be a tuple of TrackedValue");
    }

    const strct = info.@"struct";

    if (!strct.is_tuple) {
        @compileError("input must be a tuple of TrackedValue");
    }

    var types: [strct.fields.len]type = undefined;
    inline for (strct.fields, 0..) |field, i| {
        const field_info = @typeInfo(field.type);

        if (field_info != .@"struct") {
            @compileError("input must be a tuple of TrackedValue");
        }

        if (!@hasDecl(field.type, "maybe")) {
            @compileError("input must be a tuple of TrackedValue");
        }

        if (!@hasDecl(field.type, "InnerType")) {
            @compileError("input must be a tuple of TrackedValue");
        }

        const inner_type = @field(field.type, "InnerType");

        if (@TypeOf(inner_type) != type) {
            @compileError("input must be a tuple of TrackedValue");
        }

        types[i] = inner_type;
    }

    return @Tuple(&types);
}

pub fn maybeAll(values: anytype) ?MaybeAllType(@TypeOf(values)) {
    var ret: MaybeAllType(@TypeOf(values)) = undefined;

    inline for (values, 0..) |value, i| {
        ret[i] = value.maybe() orelse return null;
    }

    return ret;
}
