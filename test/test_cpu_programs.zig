const std = @import("std");
const testutil = @import("testutil.zig");
const run_program = testutil.run_program;
const destroy_cpu = testutil.destroy_cpu;
const TestCpuState = testutil.TestCpuState;

test "test program 1" {
    // Only LD instructions, register or memory, no immediate

    // This program swaps the values of B and the value from dereferencing RAM on the address you get from using B on high and low. In pseudocode:

    //   B = 0xD0
    //   RAM[0xD0D0] = 0xFF
    //   ---
    //   H = B
    //   L = B
    //   B = RAM[HL]
    //   RAM[HL] = H
    //   ---
    //   ASSERT B == 0xFF
    //   ASSERT HL == 0xD0D0
    //   ASSERT RAM[0xD0D0] = 0xD0

    const cpu = try run_program(
        "LD reg/memory",
        &[_]u8{
            0x68, // LD L, B
            0x65, // LD H, L
            0x46, // LD B, (HL)
            0x74, // LD (HL), H
            0x00, // NOP
            0xFD, // (illegal)
        },
        TestCpuState.init()
            .rB(0xD0)
            .ram(0xD0D0, 0xFF),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Hi);
    try std.testing.expectEqual(0xD0D0, cpu.register_bank.HL.all());
    try std.testing.expectEqual(0xD0, try cpu.mmu.read(0xD0D0));
}

test "test program 2" {
    // Only LD instructions, register, memory, immediate

    // This program simply loads the value on RAM address 0xD00D into registers B and C. Then puts something else in there.

    //   RAM[0xD00D] = 0xFF
    //   ---
    //   H = 0xD0
    //   L = 0x0D
    //   B = RAM[HL]
    //   C = B
    //   RAM[HL] = 0x00
    //   ---
    //   ASSERT B == 0xFF
    //   ASSERT C == 0xFF
    //   ASSERT RAM[0xD00D] == 0x00

    const cpu = try run_program(
        "LD reg/memory/immediate",
        &[_]u8{
            0x26, // LD H, 0xD0
            0xD0,
            0x2E, // LD L, 0x0D
            0x0D,
            0x46, // LD B, (HL)
            0x48, // LD C, B
            0x36, // LD (HL), 0x00
            0x00,
            0x00, // NOP
            0xFD, // (illegal)
        },
        TestCpuState.init()
            .ram(0xD00D, 0xFF),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Hi);
    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Lo);
    try std.testing.expectEqual(0x00, cpu.mmu.read(0xD00D));
}

test "test program 3" {
    // Only LD instructions indirect from and to A

    // This program loads the value from (BC) into (DE) using A as an intermediary.

    // RAM[0xD00D] = 0xFF
    // BC = 0xD00D
    // DE = 0xDDDD
    // ---
    // A = RAM[BC]
    // RAM[DE] = A
    // ---
    // ASSERT A == 0xFF
    // ASSERT RAM[0xDDDD] == 0xFF

    const cpu = try run_program(
        "LD indirect accumulator",
        &[_]u8{
            0x0A, // LD A, (BC)
            0x12, // LD (DE), A
            0x00, // NOP
            0xFD, // (illegal)
        },
        TestCpuState.init()
            .ram(0xD00D, 0xFF)
            .rBC(0xD00D)
            .rDE(0xDDDD),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.AF.Hi);
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xDDDD));
}

test "test program 4" {
    // Only LD instructions direct from and to A

    // This program loads the value from 0xD00D into 0xDDDD using A as an intermediary.

    // RAM[0xD00D] = 0xFF
    // ---
    // A = RAM[0xD00D]
    // RAM[0xDDDD] = A
    // ---
    // ASSERT A == 0xFF
    // ASSERT RAM[0xDDDD] == 0xFF

    const cpu = try run_program(
        "LD direct accumulator",
        &[_]u8{
            0xFA, // LD A, 0xD00D
            0x0D,
            0xD0,
            0xEA, // LD 0xDDDD, A
            0xDD,
            0xDD,
            0x00, // NOP
            0xFD, // (illegal)
        },
        TestCpuState.init()
            .ram(0xD00D, 0xFF),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.AF.Hi);
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xDDDD));
}

test "test program 5" {
    // Mostly LDH instructions indirect from and to A

    // This program loads the value from 0xFFAA into 0xFFBB using A as the intermediary and C as the indirect.

    // RAM[0xFFAA] = 0xFF
    // C = 0xAA
    // ---
    // A = RAM[0xFF C]
    // C = 0xBB
    // RAM[0xFF C] = A
    // ---
    // ASSERT A == 0xFF
    // ASSERT RAM[0xFFBB] = 0xFF

    const cpu = try run_program(
        "LDH indirect accumulator",
        &[_]u8{
            0xF2, // LDH A, (C)
            0x0E, // LD C, 0xBB
            0xBB,
            0xE2, // LDH (C), A
        },
        TestCpuState.init()
            .ram(0xFFAA, 0xFF)
            .rC(0xAA),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.AF.Hi);
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xFFBB));
}

test "test program 6" {
    // Only LDH instructions direct from and to A

    // This program loads the value from 0xFFAA into 0xFFBB using A as the intermediary

    // RAM[0xFFAA] = 0xFF
    // ---
    // A = RAM[0xFFAA]
    // RAM[0xFFBB] = A
    // ---
    // ASSERT A == 0xFF
    // ASSERT RAM[0xFFBB] = 0xFF

    const cpu = try run_program(
        "LDH direct accumulator",
        &[_]u8{
            0xF0, // LDH A, 0xFFAA
            0xAA,
            0xE0, // LDH 0xFFBB, A
            0xBB,
        },
        TestCpuState.init()
            .ram(0xFFAA, 0xFF),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.AF.Hi);
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xFFBB));
}

test "test program 7" {
    // Only LD instructions indirect from and to A with inc/dec HL

    // This program loads three consecutive values starting from 0xFFA0 and does nothing with them

    // HL = 0xD0A0
    // RAM[0xD0A0] = 0x11
    // RAM(0xD0A1] = 0x22
    // RAM[0xD0A2] = 0x33
    // ---
    // A = RAM[HL+]
    // A = RAM[HL+]
    // A = RAM[HL+]
    // ---
    // ASSERT A == 0x33
    // ASSERT HL == 0xD0A3

    const cpu = try run_program(
        "LD indirect accumulator with inc/dec HL",
        &[_]u8{
            0x2A, // LD A, (HL+)
            0x2A, // LD A, (HL+)
            0x2A, // LD A, (HL+)
        },
        TestCpuState.init()
            .ram(0xD0A0, 0x11)
            .ram(0xD0A1, 0x22)
            .ram(0xD0A2, 0x33)
            .rHL(0xD0A0),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0x33, cpu.register_bank.AF.Hi);
    try std.testing.expectEqual(0xD0A3, cpu.register_bank.HL.all());
}
