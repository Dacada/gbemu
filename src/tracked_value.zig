const builtin = @import("builtin");
const std = @import("std");

const logger = std.log.scoped(.tracked_value);

pub fn TrackedValue(Inner: type) type {
    return struct {
        const This = @This();

        val: ?Inner = null,

        pub fn get(self: This) Inner {
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

        pub fn set(self: *This, val: Inner) void {
            self.val = val;
        }

        pub inline fn is_init(self: This) bool {
            return self.val != null;
        }
    };
}
