const std = @import("std");
const lib = @import("lib");

const Container = lib.dependency_container.Container(.{
    .apu = .mock,
    .audio_backend = .mock_nil,
    .boot_rom = .dummy,
    .cartridge = .dummy,
    .cpu = .mock,
    .interrupt = .mock,
    .joypad = .dummy,
    .lcd = .dummy,
    .ppu = .dummy,
    .timer = .dummy,
    .video_backend = .mock_nil,
    .debugger = .mock,

    // almost everything gets mocked, so making the real components explicit too
    .mmio = .real,
    .mmu = .real,
    .scheduler = .real,
    .serial = .real,
});

const Emulator = Container.Emulator;

fn spin(emu: *Emulator, ticks: usize) !void {
    for (0..ticks) |_| {
        const res = try emu.tick();
        try std.testing.expect(!res);
    }
}

test "serial transfer" {
    var container = Container.init(.{});
    const emu = try container.get_emulator();
    var mmu = try container.get_mmu();
    const serial = try container.get_serial();
    const sched = try container.get_scheduler();

    // Wait a few cycles
    try spin(emu, 100);

    // Start transfer
    mmu.write(0xFF01, 0xAA);
    mmu.write(0xFF02, 0b1000_0001);

    // No changes yet
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(8, sched.heap.count);
    try std.testing.expectEqual(0b10101010, serial.data);

    // Run the cycle where the transfer supposedly started
    try spin(emu, 1);

    // Changes should happen after 512 cycles, run 511 of them
    try spin(emu, 511);

    // Still no changes
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(8, sched.heap.count);
    try std.testing.expectEqual(0b10101010, serial.data);

    // One more cycle
    try spin(emu, 1);

    // First bit shifted
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(7, sched.heap.count);
    try std.testing.expectEqual(0b01010101, serial.data);

    // Run one cycle
    try spin(emu, 1);

    // No changes
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(7, sched.heap.count);
    try std.testing.expectEqual(0b01010101, serial.data);

    // Run the other 511 cycles
    try spin(emu, 511);

    // Another bit shift
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(6, sched.heap.count);
    try std.testing.expectEqual(0b10101011, serial.data);

    // Do 512*6-1 more cycles to be one shift away from being done
    try spin(emu, 512 * 6 - 1);

    // One shift left
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(1, sched.heap.count);
    try std.testing.expectEqual(0b01111111, serial.data);

    // Last cycle
    try spin(emu, 1);

    // We're done
    try std.testing.expect(!serial.running);
    try std.testing.expectEqual(0, sched.heap.count);
    try std.testing.expectEqual(0b11111111, serial.data);
}
