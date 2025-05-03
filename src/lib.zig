pub const cpu = @import("cpu.zig");
pub const mmu = @import("mmu.zig");
pub const alu = @import("alu.zig");
pub const assembler = @import("assembler.zig");
pub const indirection = @import("indirection.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
