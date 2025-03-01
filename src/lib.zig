pub const cpu = @import("cpu.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
