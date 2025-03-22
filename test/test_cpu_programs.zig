const std = @import("std");
const lib = @import("lib");
const testutil = @import("testutil.zig");
const run_program = testutil.run_program;
const destroy_cpu = testutil.destroy_cpu;
const TestCpuState = testutil.TestCpuState;

test "LD only integ test" {
    // Seed values into memory and move them around.

    const code =
        \\ ; Seed some initial values into Work RAM
        \\ LD A, 0xAA
        \\ LD (0xC000), A
        \\ LD A, 0xBB
        \\ LD (0xC001), A
        \\ LD A, 0xCC
        \\ LD (0xC002), A
        \\
        \\ ; Store values via HL pointer and increment
        \\ LD HL, 0xC003
        \\ LD A, 0xDD
        \\ LD (HL+), A   ; Store at 0xC003, HL increments
        \\ LD A, 0xEE
        \\ LD (HL), A   ; Store at 0xC004, HL increments
        \\
        \\ ; Use BC and DE as indirect pointers
        \\ LD BC, 0xC000
        \\ LD DE, 0xC005
        \\ LD A, (BC)    ; Load from 0xC000
        \\ LD (DE), A    ; Store at 0xC005
        \\
        \\ ; More indirect loads using HL and DE
        \\ LD HL, 0xC001
        \\ LD A, (HL)
        \\ LD (DE), A    ; Store BB at 0xC005 (overwriting AA)
        \\
        \\ ; Load and store using HL+ and HL-
        \\ LD HL, 0xC006
        \\ LD A, 0x11
        \\ LD (HL+), A   ; Store at 0xC006, HL increments
        \\ LD A, 0x22
        \\ LD (HL-), A   ; Store at 0xC007, HL decrements
        \\ LD A, 0x33
        \\ LD (HL), A    ; Store at 0xC006, overwrite 0x11
        \\
        \\ ; Copying a value indirectly
        \\ LD HL, 0xC002
        \\ LD A, (HL)
        \\ LD (0xC008), A  ; Copy CC to 0xC008
        \\
        \\ ; HL-based shuffle
        \\ LD HL, 0xC005
        \\ LD A, (HL)
        \\ LD (0xC009), A  ; Move value from 0xC005 to 0xC009
        \\ LD A, (0xC008)
        \\ LD (HL), A      ; Swap value from 0xC008 back to 0xC005
        \\
        \\ ; LDH operations
        \\ LD A, (0xC000)
        \\ LDH (0x80), A ; Copy AA from 0xC000 to 0xFF80
        \\ LD A, (0xC001)
        \\ LDH (0x81), A ; Copy BB from 0xC001 to 0xFF81
        \\
        \\ ; Final verification marker
        \\ LD A, 0x99
        \\ LD (0xC00F), A                                         
    ;
    var program = try lib.assembler.translate(code, std.testing.allocator);
    defer std.testing.allocator.free(program);

    program = try std.testing.allocator.realloc(program, program.len + 1);
    program[program.len - 1] = 0xFD; // Illegal instruction signals end of program

    const cpu = try run_program("LD only integ test", program);
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

    const code =
        \\ ; Initialize 16-bit registers                                           
        \\ LD HL, 0xC000    ; Set HL to start of Work RAM                        
        \\ LD DE, 0xC100    ; Set DE to another location in Work RAM             
        \\ LD BC, 0x1234    ; Set BC to a test value                             
        \\ LD SP, 0xFFF0    ; Set SP to high RAM (safe for stack operations)     
        \\                                                                       
        \\ ; Store SP into memory for verification                                 
        \\ LD (0xC010), SP  ; Store initial SP (0xFFF0) at address 0xC010        
        \\                                                                       
        \\ ; Push and Pop values                                                   
        \\ PUSH BC          ; Push BC onto the stack                             
        \\ PUSH HL          ; Push HL onto the stack                             
        \\ POP DE           ; Pop into DE (DE should now hold HL's value)        
        \\ POP BC           ; Pop into BC (BC should now hold its original value)
        \\                                                                       
        \\ ; Modify Stack Pointer                                                  
        \\ LD HL, SP+8      ; Load HL with SP + 8 (should be 0xFFF8)             
        \\                                                                       
        \\ ; Store adjusted HL value into memory                                   
        \\ LD A, L  ; Store HL (adjusted SP)                                     
        \\ LD (0xC012), A                                                        
        \\ LD A, H                                                               
        \\ LD (0xC013), A                                                        
        \\                                                                       
        \\ ; Store SP from HL and verify                                           
        \\ LD SP, HL        ; Move HL (adjusted) into SP                         
        \\ LD (0xC014), SP  ; Store new SP value into memory                     
        \\                                                                       
        \\ ; Final verification marker                                             
        \\ LD A, 0x99                                                            
        \\ LD (0xC01F), A                                                        
    ;
    var program = try lib.assembler.translate(code, std.testing.allocator);
    defer std.testing.allocator.free(program);

    program = try std.testing.allocator.realloc(program, program.len + 1);
    program[program.len - 1] = 0xFD; // Illegal instruction signals end of program

    const cpu = try run_program(
        "LD only integ test (16-bit)",
        program,
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
