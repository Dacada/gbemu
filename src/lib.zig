pub const cpu = @import("cpu.zig");
pub const mmu = @import("mmu.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
