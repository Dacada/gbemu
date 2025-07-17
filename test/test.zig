pub const cpu_extensive = @import("test_cpu_extensive.zig");
pub const cpu_program = @import("test_cpu_programs.zig");
pub const serial = @import("test_serial.zig");
pub const interrupt = @import("test_interrupt.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
