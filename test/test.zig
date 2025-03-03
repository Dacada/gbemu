pub const cpu_extensive = @import("test_cpu_extensive.zig");
pub const cpu_program = @import("test_cpu_programs.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
