const std = @import("std");
const lib = @import("lib");

const MockMemory = lib.mmu.MockMemory;
const InterruptKind = lib.interruptKind.InterruptKind;

pub const FakeScheduler = struct {};

pub const FakeCartridge = struct {
    lastWrite: u8 = 0x00,

    pub const Rom = MockMemory;
    pub const Ram = MockMemory;
};

pub const FakePpu = struct {
    lastWrite: u8 = 0x00,

    pub const Vram = MockMemory;
    pub const Oam = MockMemory;
    pub const Forbidden = MockMemory;

    pub fn tick(_: *FakePpu) void {}
};

pub const FakeTimer = struct {
    pub fn tick(_: *FakeTimer) void {}
};

pub const FakeDebugger = struct {
    pub fn enter_debugger_if_needed(_: *const FakeDebugger) !?lib.debugger.DebuggerResult {
        return null;
    }
};

pub const FakeInterrupt = struct {
    pub fn request(_: *FakeInterrupt, _: InterruptKind) void {}
};

pub const FakeCpu = struct {
    pub fn tick(_: FakeCpu) void {}
};

const Scheduler = lib.scheduler.Scheduler;
const Serial = lib.serial.Serial(Scheduler, FakeInterrupt);
const Dummy = lib.mmio.Dummy;
const Mmio = lib.mmio.Mmio(Dummy, Serial, Dummy, Dummy, Dummy, Dummy, Dummy, Dummy);
const Mmu = lib.mmu.Mmu(FakeCartridge, FakePpu, Mmio);
const Cpu = FakeCpu;
const Emulator = lib.emulator.Emulator(Cpu, FakePpu, FakeTimer, Scheduler, FakeDebugger);

fn spin(emu: *Emulator, ticks: usize) !void {
    for (0..ticks) |_| {
        const res = try emu.tick();
        try std.testing.expect(!res);
    }
}

test "serial transfer" {
    var cart = FakeCartridge{};

    var sched = Scheduler.init();

    var dummy = Dummy{};
    var intr = FakeInterrupt{};
    var serial = Serial.init(&sched, &intr);
    var ppu = FakePpu{};

    var mmio = Mmio.init(
        &dummy,
        &serial,
        &dummy,
        &dummy,
        &dummy,
        &dummy,
        &dummy,
        &dummy,
    );

    var mmu = Mmu.init(&cart, &ppu, &mmio);
    lib.emulator.initialize_memory(Mmu, &mmu);

    var cpu = FakeCpu{};
    var timer = FakeTimer{};

    var dbg = FakeDebugger{};
    var emu = Emulator.init(&cpu, &ppu, &timer, &sched, &dbg);

    // Wait a few cycles
    try spin(&emu, 100);

    // Start transfer
    mmu.write(0xFF01, 0xAA);
    mmu.write(0xFF02, 0b1000_0001);

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
