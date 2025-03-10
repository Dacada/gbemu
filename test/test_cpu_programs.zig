const std = @import("std");
const testutil = @import("testutil.zig");
const run_program = testutil.run_program;
const destroy_cpu = testutil.destroy_cpu;
const TestCpuState = testutil.TestCpuState;

test "LD only integ test" {
    // Seed values into memory and move them around.

    const program = [_]u8{
        //////////////////// Seed some initial values into Work RAM
        0x3E, 0xAA, //////// LD A, 0xAA
        0xEA, 0x00, 0xC0, // LD (0xC000), A
        0x3E, 0xBB, //////// LD A, 0xBB
        0xEA, 0x01, 0xC0, // LD (0xC001), A
        0x3E, 0xCC, //////// LD A, 0xCC
        0xEA, 0x02, 0xC0, // LD (0xC002), A

        //////////////////// Store values via HL pointer and increment
        0x21, 0x03, 0xC0, // LD HL, 0xC003
        0x3E, 0xDD, //////// LD A, 0xDD
        0x22, ////////////// LD (HL+), A   ; Store at 0xC003, HL increments
        0x3E, 0xEE, //////// LD A, 0xEE
        0x77, ////////////// LD (HL), A   ; Store at 0xC004, HL increments

        //////////////////// Use BC and DE as indirect pointers
        0x01, 0x00, 0xC0, // LD BC, 0xC000
        0x11, 0x05, 0xC0, // LD DE, 0xC005
        0x0A, ////////////// LD A, (BC)    ; Load from 0xC000
        0x12, ////////////// LD (DE), A    ; Store at 0xC005

        //////////////////// More indirect loads using HL and DE
        0x21, 0x01, 0xC0, // LD HL, 0xC001
        0x7E, ////////////// LD A, (HL)
        0x12, ////////////// LD (DE), A    ; Store BB at 0xC005 (overwriting AA)

        //////////////////// Load and store using HL+ and HL-
        0x21, 0x06, 0xC0, // LD HL, 0xC006
        0x3E, 0x11, //////// LD A, 0x11
        0x22, ////////////// LD (HL+), A   ; Store at 0xC006, HL increments
        0x3E, 0x22, //////// LD A, 0x22
        0x32, ////////////// LD (HL-), A   ; Store at 0xC007, HL decrements
        0x3E, 0x33, //////// LD A, 0x33
        0x77, ////////////// LD (HL), A    ; Store at 0xC006, overwrite 0x11

        //////////////////// Copying a value indirectly
        0x21, 0x02, 0xC0, // LD HL, 0xC002
        0x7E, ////////////// LD A, (HL)
        0xEA, 0x08, 0xC0, // LD (0xC008), A  ; Copy CC to 0xC008

        //////////////////// HL-based shuffle
        0x21, 0x05, 0xC0, // LD HL, 0xC005
        0x7E, ////////////// LD A, (HL)
        0xEA, 0x09, 0xC0, // LD (0xC009), A  ; Move value from 0xC005 to 0xC009
        0xFA, 0x08, 0xC0, // LD A, (0xC008)
        0x77, ////////////// LD (HL), A      ; Swap value from 0xC008 back to 0xC005

        //////////////////// LDH operations
        0xFA, 0x00, 0xC0, // LD A, (0xC000)
        0xE0, 0x80, //////// LDH (0xFF80), A ; Copy AA from 0xC000 to 0xFF80
        0xFA, 0x01, 0xC0, // LD A, (0xC001)
        0xE0, 0x81, //////// LDH (0xFF81), A ; Copy BB from 0xC001 to 0xFF81

        //////////////////// Final verification marker
        0x3E, 0x99, //////// LD A, 0x99
        0xEA, 0x0F, 0xC0, // LD (0xC00F), A

        //////////////////// End
        0xFD, ////////////// Illegal instruction
    };

    const cpu = try run_program(
        "LD only integ test",
        &program,
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(program.len, cpu.reg.PC);

    try std.testing.expectEqual(0x99, cpu.reg.AF.Hi);
    try std.testing.expectEqual(0xC0, cpu.reg.BC.Hi);
    try std.testing.expectEqual(0x00, cpu.reg.BC.Lo);
    try std.testing.expectEqual(0xC0, cpu.reg.DE.Hi);
    try std.testing.expectEqual(0x05, cpu.reg.DE.Lo);
    try std.testing.expectEqual(0xC0, cpu.reg.HL.Hi);
    try std.testing.expectEqual(0x05, cpu.reg.HL.Lo);

    try std.testing.expectEqual(0xAA, cpu.mmu.read(0xC000));
    try std.testing.expectEqual(0xBB, cpu.mmu.read(0xC001));
    try std.testing.expectEqual(0xCC, cpu.mmu.read(0xC002));
    try std.testing.expectEqual(0xDD, cpu.mmu.read(0xC003));
    try std.testing.expectEqual(0xEE, cpu.mmu.read(0xC004));
    try std.testing.expectEqual(0xCC, cpu.mmu.read(0xC005));
    try std.testing.expectEqual(0x33, cpu.mmu.read(0xC006));
    try std.testing.expectEqual(0x22, cpu.mmu.read(0xC007));
    try std.testing.expectEqual(0xCC, cpu.mmu.read(0xC008));
    try std.testing.expectEqual(0xBB, cpu.mmu.read(0xC009));
    try std.testing.expectEqual(0x99, cpu.mmu.read(0xC00F));
    try std.testing.expectEqual(0xAA, cpu.mmu.read(0xFF80));
    try std.testing.expectEqual(0xBB, cpu.mmu.read(0xFF81));
}

test "LD only integ test (16-bit)" {
    // Seed values into memory and move them around.

    const program = [_]u8{
        //////////////////// Initialize 16-bit registers
        0x21, 0x00, 0xC0, // LD HL, 0xC000    ; Set HL to start of Work RAM
        0x11, 0x00, 0xC1, // LD DE, 0xC100    ; Set DE to another location in Work RAM
        0x01, 0x34, 0x12, // LD BC, 0x1234    ; Set BC to a test value
        0x31, 0xF0, 0xFF, // LD SP, 0xFFF0    ; Set SP to high RAM (safe for stack operations)

        //////////////////// Store SP into memory for verification
        0x08, 0x10, 0xC0, // LD (0xC010), SP  ; Store initial SP (0xFFF0) at address 0xC010

        //////////////////// Push and Pop values
        0xC5, ////////////// PUSH BC          ; Push BC onto the stack
        0xE5, ////////////// PUSH HL          ; Push HL onto the stack
        0xD1, ////////////// POP DE           ; Pop into DE (DE should now hold HL's value)
        0xC1, ////////////// POP BC           ; Pop into BC (BC should now hold its original value)

        //////////////////// Modify Stack Pointer
        0xF8, 0x08, //////// LD HL, SP+8      ; Load HL with SP + 8 (should be 0xFFF8)

        //////////////////// Store adjusted HL value into memory
        0x7D, ////////////// LD A, L  ; Store HL (adjusted SP)
        0xEA, 0x12, 0xC0, // LD (0xC012), A
        0x7C, ////////////// LD A, H
        0xEA, 0x13, 0xC0, // LD (0xC013), A

        //////////////////// Store SP from HL and verify
        0xF9, ////////////// LD SP, HL        ; Move HL (adjusted) into SP
        0x08, 0x14, 0xC0, // LD (0xC014), SP  ; Store new SP value into memory

        //////////////////// Final verification marker
        0x3E, 0x99, //////// LD A, 0x99
        0xEA, 0x1F, 0xC0, // LD (0xC01F), A

        //////////////////// End
        0xFD, ////////////// Illegal instruction
    };

    const cpu = try run_program(
        "LD only integ test (16-bit)",
        &program,
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(program.len, cpu.reg.PC);

    try std.testing.expectEqual(0x99, cpu.reg.AF.Hi);
    try std.testing.expectEqual(0x1234, cpu.reg.BC.all());
    try std.testing.expectEqual(0xC000, cpu.reg.DE.all());
    try std.testing.expectEqual(0xFFF8, cpu.reg.HL.all());
    try std.testing.expectEqual(0xFFF8, cpu.reg.SP.all());

    try std.testing.expectEqual(0xF0, cpu.mmu.read(0xC010));
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xC011));
    try std.testing.expectEqual(0xF8, cpu.mmu.read(0xC012));
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xC013));
    try std.testing.expectEqual(0xF8, cpu.mmu.read(0xC014));
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xC015));
    try std.testing.expectEqual(0x99, cpu.mmu.read(0xC01F));
}
