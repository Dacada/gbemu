const std = @import("std");
const lib = @import("lib");

var array = [_]u8{0x00} ** 0x10000;
var program = [_]u8{0x00} ** 0x100;

const FakeDebugger = struct {
    pub fn enter_debugger_if_needed(_: *const FakeDebugger) !?lib.debugger.DebuggerResult {
        return null;
    }
};

fn spin(emu: *lib.emulator.Emulator(FakeDebugger), ticks: usize) !void {
    for (0..ticks) |_| {
        const res = try emu.tick();
        try std.testing.expect(!res);
    }
}

test "serial transfer" {
    const code = "loop: jp loop";
    const prog = try lib.assembler.translate(code, std.testing.allocator);
    std.mem.copyBackwards(u8, &program, prog);
    std.testing.allocator.free(prog);

    var sched = lib.scheduler.Scheduler{};
    var serial = lib.serial.Serial{ .sched = &sched };
    var mmio = lib.mmio.Mmio{
        .joypad = lib.memory.SimpleMemory(false, &array, null).memory(),
        .serial = serial.memory(),
        .timer = lib.memory.SimpleMemory(false, &array, null).memory(),
        .interrupts = lib.memory.SimpleMemory(false, &array, null).memory(),
        .audio = lib.memory.SimpleMemory(false, &array, null).memory(),
        .wave = lib.memory.SimpleMemory(false, &array, null).memory(),
        .lcd = lib.memory.SimpleMemory(false, &array, null).memory(),
        .boot_rom = lib.memory.SimpleMemory(false, &array, null).memory(),
    };
    var mmu = lib.mmu.Mmu{
        .cartRom = lib.memory.SimpleMemory(false, &program, null).memory(),
        .cartRam = lib.memory.SimpleMemory(false, &array, null).memory(),
        .vram = lib.memory.SimpleMemory(false, &array, null).memory(),
        .oam = lib.memory.SimpleMemory(false, &array, null).memory(),
        .forbidden = lib.memory.SimpleMemory(false, &array, null).memory(),
        .mmio = mmio.memory(),
    };
    lib.emulator.initialize_memory(mmu.memory());
    var ppu = lib.ppu.Ppu.init();
    var mem = mmu.memory();
    var cpu = lib.cpu.Cpu.init(&mem, null);
    var dbg = FakeDebugger{};
    var emu = lib.emulator.Emulator(FakeDebugger){
        .cpu = &cpu,
        .ppu = &ppu,
        .sched = &sched,
        .debugger = &dbg,
    };

    // Wait a few cycles
    try spin(&emu, 100);

    // Start transfer
    mem.write(0xFF01, 0xAA);
    mem.write(0xFF02, 0b1000_0001);

    // No changes yet
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(8, sched.heap.count);
    try std.testing.expectEqual(0b10101010, serial.data);

    // Run the cycle where the transfer supposedly started
    try spin(&emu, 1);

    // Changes should happen after 512 cycles, run 511 of them
    try spin(&emu, 511);

    // Still no changes
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(8, sched.heap.count);
    try std.testing.expectEqual(0b10101010, serial.data);

    // One more cycle
    try spin(&emu, 1);

    // First bit shifted
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(7, sched.heap.count);
    try std.testing.expectEqual(0b01010101, serial.data);

    // Run one cycle
    try spin(&emu, 1);

    // No changes
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(7, sched.heap.count);
    try std.testing.expectEqual(0b01010101, serial.data);

    // Run the other 511 cycles
    try spin(&emu, 511);

    // Another bit shift
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(6, sched.heap.count);
    try std.testing.expectEqual(0b10101011, serial.data);

    // Do 512*6-1 more cycles to be one shift away from being done
    try spin(&emu, 512 * 6 - 1);

    // One shift left
    try std.testing.expect(serial.running);
    try std.testing.expectEqual(1, sched.heap.count);
    try std.testing.expectEqual(0b01111111, serial.data);

    // Last cycle
    try spin(&emu, 1);

    // We're done
    try std.testing.expect(!serial.running);
    try std.testing.expectEqual(0, sched.heap.count);
    try std.testing.expectEqual(0b11111111, serial.data);
}
