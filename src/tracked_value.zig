const MemoryFlag = @import("memory_flag.zig").MemoryFlag;

pub fn TrackedValue(T: type) type {
    return struct {
        const This = @This();

        value: ?T = null,
        should_panic: bool = false,
        default: T,

        pub inline fn init(default: T) This {
            return .{
                .default = default,
            };
        }

        pub inline fn resolve(self: This, flag: ?*MemoryFlag) T {
            if (self.value) |val| {
                return val;
            }
            if (self.should_panic) {
                @panic("attempt to resolve undefined value");
            }
            if (flag) |flg| {
                flg.uninitialized = true;
            }
            return self.default;
        }

        pub inline fn set(self: *This, value: T) void {
            self.value = value;
        }
    };
}
