const std = @import("std");
const lib = @import("lib");

const FakeMmu = lib.mmu.MockMmu;
const InterruptKind = lib.interruptKind.InterruptKind;
const Interrupt = lib.interrupt.Interrupt;
const Cpu = lib.cpu.Cpu(FakeMmu, Interrupt);

fn runInstructions(cpu: *Cpu, num: usize) !void {
    for (0..num) |_| {
        cpu.tick();
        try std.testing.expect(!cpu.flags.breakpoint);
        while (!cpu.isInstructionBoundary()) {
            cpu.tick();
            try std.testing.expect(!cpu.flags.breakpoint);
        }
    }
    try std.testing.expect(cpu.isInstructionBoundary());
}

fn test_interrupt(target: InterruptKind, targetAddr: u16) !void {
    {
        const program = "loop: jp loop";

        const assembled1 = try lib.assembler.translate(program, std.testing.allocator, 0x100);
        defer std.testing.allocator.free(assembled1);

        @memset(&FakeMmu.backingArray, 0xAA);
        @memcpy(FakeMmu.backingArray[0x100 .. 0x100 + assembled1.len], assembled1);

        const assembled2 = try lib.assembler.translate(program, std.testing.allocator, targetAddr);
        defer std.testing.allocator.free(assembled2);

        @memcpy(FakeMmu.backingArray[targetAddr .. targetAddr + assembled2.len], assembled2);
    }

    var intr = Interrupt.init();
    var mmu = FakeMmu{};
    var cpu = Cpu.init(&mmu, &intr, 0xAA);

    cpu.reg.PC = 0x100;
    cpu.reg.SP.setAll(0xFFFE);
    intr.@"if" = target.asMask();

    // When IME is disabled and IE is disabled, interrupt is not serviced
    cpu.reg.IME = 0;
    intr.ie = ~target.asMask();
    try runInstructions(&cpu, 100);
    try std.testing.expectEqual(0x101, cpu.reg.PC);

    // When IME is enabled and IE is disabled, interrupt is not serviced
    cpu.reg.IME = 1;
    intr.ie = ~target.asMask();
    try runInstructions(&cpu, 100);
    try std.testing.expectEqual(0x101, cpu.reg.PC);

    // When IME is disabled and IE is enabled, interrupt is not serviced
    cpu.reg.IME = 0;
    intr.ie = target.asMask();
    try runInstructions(&cpu, 100);
    try std.testing.expectEqual(0x101, cpu.reg.PC);

    // When IME is enabled and IE is enabled, interrupt is serviced

    cpu.reg.IME = 1;
    intr.ie = target.asMask();

    // M0 - Decrease PC
    cpu.tick();
    try std.testing.expectEqual(0x100, cpu.reg.PC);

    // M1 - Decrease SP
    cpu.tick();
    try std.testing.expectEqual(0xFFFD, cpu.reg.SP.all());

    // M2 - Load PC hi, decrease SP
    cpu.tick();
    try std.testing.expectEqual(0xFFFC, cpu.reg.SP.all());
    try std.testing.expectEqual(0x01, mmu.read(0xFFFD));

    // M3 - Load PC lo, set PC to IRQ address
    cpu.tick();
    try std.testing.expectEqual(0x00, mmu.read(0xFFFC));
    try std.testing.expectEqual(targetAddr, cpu.reg.PC);

    // M4 - Generic fetch
    cpu.tick();
    try std.testing.expectEqual(targetAddr + 1, cpu.reg.PC);
    try std.testing.expectEqual(mmu.read(targetAddr), cpu.reg.IR);

    // And we're still here after a while
    try runInstructions(&cpu, 100);
    try std.testing.expectEqual(targetAddr + 1, cpu.reg.PC);
}

test "interrupt vblank" {
    try test_interrupt(.vblank, 0x40);
}

test "interrupt lcd" {
    try test_interrupt(.lcd, 0x48);
}

test "interrupt timer" {
    try test_interrupt(.timer, 0x50);
}

test "interrupt serial" {
    try test_interrupt(.serial, 0x58);
}

test "interrupt joypad" {
    try test_interrupt(.joypad, 0x60);
}
