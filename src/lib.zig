pub const cpu = @import("cpu.zig");
pub const mmu = @import("mmu.zig");
pub const alu = @import("alu.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
