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

test "halt normally: ime=1 and no interrupt pending" {
    const program =
        \\ei
        \\nop
        \\halt
    ;

    {
        const assembled = try lib.assembler.translate(program, std.testing.allocator, 0);
        defer std.testing.allocator.free(assembled);
        @memset(&FakeMmu.backingArray, 0xAA);
        @memcpy(FakeMmu.backingArray[0..assembled.len], assembled);
        FakeMmu.backingArray[0x40] = 0;
    }

    var intr = Interrupt.init();
    var mmu = FakeMmu{};
    var cpu = Cpu.init(&mmu, &intr, 0xAA);

    cpu.reg.PC = 0;
    cpu.reg.SP.setAll(0xFFFE);
    cpu.reg.IME = 0;
    intr.ie = InterruptKind.vblank.asMask();

    // execute ei
    cpu.tick(); // fetch ei
    cpu.tick(); // execute ei, fetch nop
    try std.testing.expect(!cpu.flags.breakpoint);

    // execute nop
    cpu.tick(); // execute nop, fetch halt
    try std.testing.expectEqual(1, cpu.reg.IME);
    try std.testing.expect(!cpu.flags.breakpoint);

    // execute halt
    cpu.tick(); // execute halt, fetch 0xAA

    // we are halted, cpu isn't advancing
    for (0..100) |_| {
        try std.testing.expectEqual(4, cpu.reg.PC);
        try std.testing.expect(!cpu.flags.breakpoint);
        cpu.tick();
    }

    // trigger interrupt
    intr.@"if" = InterruptKind.vblank.asMask();

    // we are no longer halted, servicing interrupt
    cpu.tick(); // adjust pc
    cpu.tick(); // adjust sp
    cpu.tick(); // load pc hi
    cpu.tick(); // load pc lo
    cpu.tick(); // fetch 0x00
    try std.testing.expect(!cpu.flags.breakpoint);
    try std.testing.expectEqual(0, cpu.reg.IR);

    // we stored the correct PC
    try std.testing.expectEqual(0, FakeMmu.backingArray[0xFFFD]);
    try std.testing.expectEqual(3, FakeMmu.backingArray[0xFFFC]);
}

test "halt normally: ime=0 and no interrupt pending" {
    const program =
        \\nop
        \\halt
        \\nop
    ;

    {
        const assembled = try lib.assembler.translate(program, std.testing.allocator, 0);
        defer std.testing.allocator.free(assembled);
        @memset(&FakeMmu.backingArray, 0xAA);
        @memcpy(FakeMmu.backingArray[0..assembled.len], assembled);
        FakeMmu.backingArray[0x40] = 0;
    }

    var intr = Interrupt.init();
    var mmu = FakeMmu{};
    var cpu = Cpu.init(&mmu, &intr, 0xAA);

    cpu.reg.PC = 0;
    cpu.reg.SP.setAll(0xFFFE);
    cpu.reg.IME = 0;
    intr.ie = InterruptKind.vblank.asMask();

    // execute nop
    cpu.tick(); // fetch nop
    cpu.tick(); // execute nop, fetch halt
    try std.testing.expect(!cpu.flags.breakpoint);

    // execute halt
    cpu.tick(); // execute halt, fetch 0xAA

    // we are halted, cpu isn't advancing
    for (0..100) |_| {
        try std.testing.expectEqual(3, cpu.reg.PC);
        try std.testing.expect(!cpu.flags.breakpoint);
        cpu.tick();
    }

    // trigger interrupt
    intr.@"if" = InterruptKind.vblank.asMask();

    // we are no longer halted, execute nop (since interrupts are disabled)
    cpu.tick(); // execute nop, fetch 0xAA
    try std.testing.expectEqual(4, cpu.reg.PC);
}

// halt bug is tested through specific cases

test "halt bug: ei, halt" {
    const program =
        \\ei
        \\halt
    ;

    {
        const assembled = try lib.assembler.translate(program, std.testing.allocator, 0);
        defer std.testing.allocator.free(assembled);
        @memset(&FakeMmu.backingArray, 0xAA);
        @memcpy(FakeMmu.backingArray[0..assembled.len], assembled);
        FakeMmu.backingArray[0x40] = 0xD9; // reti
    }

    var intr = Interrupt.init();
    var mmu = FakeMmu{};
    var cpu = Cpu.init(&mmu, &intr, 0xAA);

    cpu.reg.PC = 0;
    cpu.reg.SP.setAll(0xFFFE);
    cpu.reg.IME = 0;
    intr.ie = InterruptKind.vblank.asMask();
    intr.@"if" = InterruptKind.vblank.asMask();

    // execute ei
    cpu.tick(); // fetch ei
    cpu.tick(); // execute ei, fetch halt
    try std.testing.expect(!cpu.flags.breakpoint);

    // execute halt
    cpu.tick(); // execute halt, fetch 0xAA
    try std.testing.expect(!cpu.flags.breakpoint);

    // interrupt is serviced
    cpu.tick(); // adjust pc
    cpu.tick(); // adjust sp
    cpu.tick(); // load pc hi
    cpu.tick(); // load pc lo
    cpu.tick(); // fetch reti
    try std.testing.expect(!cpu.flags.breakpoint);
    try std.testing.expectEqual(0xD9, cpu.reg.IR);
    try std.testing.expect(intr.pending() == null);

    // we return from the interrupt
    cpu.tick(); // fetch pc lo
    cpu.tick(); // fetch pc hi
    cpu.tick(); // set ime, update pc
    cpu.tick(); // fetch halt
    try std.testing.expect(!cpu.flags.breakpoint);

    // we execute halt again
    cpu.tick(); // execute halt, fetch 0xAA

    // and now we're halted for good
    for (0..100) |_| {
        try std.testing.expectEqual(3, cpu.reg.PC);
        try std.testing.expect(!cpu.flags.breakpoint);
        cpu.tick();
    }
}

test "halt bug: halt, inc b " {
    const program =
        \\halt
        \\inc b
    ;

    {
        const assembled = try lib.assembler.translate(program, std.testing.allocator, 0);
        defer std.testing.allocator.free(assembled);
        @memset(&FakeMmu.backingArray, 0xAA);
        @memcpy(FakeMmu.backingArray[0..assembled.len], assembled);
    }

    var intr = Interrupt.init();
    var mmu = FakeMmu{};
    var cpu = Cpu.init(&mmu, &intr, 0xAA);

    cpu.reg.PC = 0;
    cpu.reg.SP.setAll(0xFFFE);
    cpu.reg.IME = 0;
    cpu.reg.BC.Hi = 0;
    intr.ie = InterruptKind.vblank.asMask();
    intr.@"if" = InterruptKind.vblank.asMask();

    // execute halt
    cpu.tick(); // fetch halt
    cpu.tick(); // execute halt, fetch inc b
    try std.testing.expect(!cpu.flags.breakpoint);

    // first inc b
    cpu.tick(); // exit halt immediately, execute inc b and fetch it again
    try std.testing.expect(!cpu.flags.breakpoint);

    // second inc b
    cpu.tick(); // execute inc b and fetch 0xAA
    try std.testing.expect(!cpu.flags.breakpoint);
    try std.testing.expectEqual(0x02, cpu.reg.BC.Hi);
}

test "halt bug: halt, ld B 4" {
    const program =
        \\halt
        \\ld B 4
    ;

    {
        const assembled = try lib.assembler.translate(program, std.testing.allocator, 0);
        defer std.testing.allocator.free(assembled);
        @memset(&FakeMmu.backingArray, 0xAA);
        @memcpy(FakeMmu.backingArray[0..assembled.len], assembled);
    }

    var intr = Interrupt.init();
    var mmu = FakeMmu{};
    var cpu = Cpu.init(&mmu, &intr, 0xAA);

    cpu.reg.PC = 0;
    cpu.reg.SP.setAll(0xFFFE);
    cpu.reg.IME = 0;
    cpu.reg.BC.setAll(0);
    intr.ie = InterruptKind.vblank.asMask();
    intr.@"if" = InterruptKind.vblank.asMask();

    // execute halt
    cpu.tick(); // fetch halt
    cpu.tick(); // execute halt, fetch 0x06
    try std.testing.expect(!cpu.flags.breakpoint);

    // execute ld 0x06
    cpu.tick(); // exit halt immediately, read opcode 0x06 again as immediate
    cpu.tick(); // store immediate, fetch 0x04
    try std.testing.expect(!cpu.flags.breakpoint);
    try std.testing.expectEqual(0x06, cpu.reg.BC.Hi);

    // execute inc b
    cpu.tick(); // execute inc b and fetch 0xAA
    try std.testing.expect(!cpu.flags.breakpoint);
    try std.testing.expectEqual(0x07, cpu.reg.BC.Hi);
}

test "halt bug: halt, rst 1" {
    const program =
        \\halt
        \\rst 1
    ;

    {
        const assembled = try lib.assembler.translate(program, std.testing.allocator, 0);
        defer std.testing.allocator.free(assembled);
        @memset(&FakeMmu.backingArray, 0xAA);
        @memcpy(FakeMmu.backingArray[0..assembled.len], assembled);
    }

    var intr = Interrupt.init();
    var mmu = FakeMmu{};
    var cpu = Cpu.init(&mmu, &intr, 0xAA);

    cpu.reg.PC = 0;
    cpu.reg.SP.setAll(0xFFFE);
    cpu.reg.IME = 0;
    cpu.reg.BC.setAll(0);
    intr.ie = InterruptKind.vblank.asMask();
    intr.@"if" = InterruptKind.vblank.asMask();

    // execute halt
    cpu.tick(); // fetch halt
    cpu.tick(); // execute halt, fetch 0x06
    try std.testing.expect(!cpu.flags.breakpoint);

    // execute rst 0x08
    cpu.tick(); // exit halt immediately, adjust sp
    cpu.tick(); // store pc hi
    cpu.tick(); // store pc lo, store target addr to pc
    cpu.tick(); // fetch 0xAA
    try std.testing.expect(!cpu.flags.breakpoint);

    // we would return back to rst
    try std.testing.expectEqual(0x00, FakeMmu.backingArray[0xFFFD]);
    try std.testing.expectEqual(0x01, FakeMmu.backingArray[0xFFFC]);
}

test "halt bug: halt, halt" {
    const program =
        \\halt
        \\halt
    ;

    {
        const assembled = try lib.assembler.translate(program, std.testing.allocator, 0);
        defer std.testing.allocator.free(assembled);
        @memset(&FakeMmu.backingArray, 0xAA);
        @memcpy(FakeMmu.backingArray[0..assembled.len], assembled);
    }

    var intr = Interrupt.init();
    var mmu = FakeMmu{};
    var cpu = Cpu.init(&mmu, &intr, 0xAA);

    cpu.reg.PC = 0;
    cpu.reg.SP.setAll(0xFFFE);
    cpu.reg.IME = 0;
    cpu.reg.BC.setAll(0);
    intr.ie = InterruptKind.vblank.asMask();
    intr.@"if" = InterruptKind.vblank.asMask();

    // execute halt
    cpu.tick(); // fetch halt
    cpu.tick(); // execute halt, fetch second halt
    try std.testing.expect(!cpu.flags.breakpoint);

    // execute second halt
    cpu.tick(); // exit halt immediately, execute second halt, fetch second halt again
    try std.testing.expect(!cpu.flags.breakpoint);

    // we're stuck now
    for (0..100) |_| {
        try std.testing.expectEqual(1, cpu.reg.PC);
        try std.testing.expect(!cpu.flags.breakpoint);
        try std.testing.expect(cpu.flags.double_halt);
        cpu.tick();
    }
}
