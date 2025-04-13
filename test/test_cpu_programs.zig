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

test "Arithmetic (8-bit)" {
    // Seed values into memory and do arithmetic with them. Save flags to memory.

    const code =
        \\ ; Initialize SP
        \\ LD SP, 0xFFFF
        \\
        \\ ; Setup
        \\ LD A, 0xF0         ; A = 0xF0
        \\ LD B, 0x30         ; B = 0x30
        \\ ADD B              ; A = 0xF0 + 0x30 = 0x20 (C=1)
        \\ PUSH AF            ; Save flags (Carry should be set)
        \\
        \\ LD C, 0x20         ; C = 0x20
        \\ SUB C              ; A = 0x20 - 0x20 = 0x00 (Z=1)
        \\ PUSH AF            ; Save flags (Zero should be set)
        \\
        \\ ; Additional standard ops
        \\ LD A, 0x12         ; A = 0x12
        \\ LD B, 0x34         ; B = 0x34
        \\ ADD B              ; A = 0x12 + 0x34 = 0x46
        \\ PUSH AF
        \\
        \\ LD C, 0x08
        \\ SUB C              ; A = 0x46 - 0x08 = 0x3E
        \\ PUSH AF
        \\
        \\ INC A              ; A = 0x3F
        \\ PUSH AF
        \\
        \\ DEC A              ; A = 0x3E
        \\ PUSH AF
        \\
        \\ LD D, 0xFF
        \\ AND D              ; A = A & D = 0x3E
        \\ PUSH AF
        \\
        \\ OR C               ; A = A | C = 0x3E
        \\ PUSH AF
        \\
        \\ XOR B              ; A = 0x0A
        \\ PUSH AF
        \\
        \\ CP C               ; Compare A vs C (0x0A - 0x08)
        \\ PUSH AF
        \\
        \\ ; Store registers
        \\ LD HL, 0xC000
        \\ LD (HL), A
        \\ INC L
        \\ LD (HL), B
        \\ INC L
        \\ LD (HL), C
        \\ INC L
        \\ LD (HL), D
    ;
    var program = try lib.assembler.translate(code, std.testing.allocator);
    defer std.testing.allocator.free(program);

    program = try std.testing.allocator.realloc(program, program.len + 1);
    program[program.len - 1] = 0xFD; // Illegal instruction signals end of program

    const cpu = try run_program(
        "Arithmetic integ test (8-bit)",
        program,
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(program.len, cpu.reg.PC);

    try std.testing.expectEqual(0x0A, cpu.reg.AF.Hi);
    try std.testing.expectEqual(0x34, cpu.reg.BC.Hi);
    try std.testing.expectEqual(0x08, cpu.reg.BC.Lo);
    try std.testing.expectEqual(0xFF, cpu.reg.DE.Hi);
    try std.testing.expectEqual(0xC003, cpu.reg.HL.all());
    try std.testing.expectEqual(0xFFEB, cpu.reg.SP.all());

    try std.testing.expectEqual(0x0A, cpu.mmu.read(0xC000));
    try std.testing.expectEqual(0x34, cpu.mmu.read(0xC001));
    try std.testing.expectEqual(0x08, cpu.mmu.read(0xC002));
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xC003));

    try std.testing.expectEqual(0x20, cpu.mmu.read(0xFFFE));
    try std.testing.expectEqual(0b00010000, cpu.mmu.read(0xFFFD));
    try std.testing.expectEqual(0x00, cpu.mmu.read(0xFFFC));
    try std.testing.expectEqual(0b11000000, cpu.mmu.read(0xFFFB));
    try std.testing.expectEqual(0x46, cpu.mmu.read(0xFFFA));
    try std.testing.expectEqual(0b00000000, cpu.mmu.read(0xFFF9));
    try std.testing.expectEqual(0x3E, cpu.mmu.read(0xFFF8));
    try std.testing.expectEqual(0b01100000, cpu.mmu.read(0xFFF7));
    try std.testing.expectEqual(0x3F, cpu.mmu.read(0xFFF6));
    try std.testing.expectEqual(0b00000000, cpu.mmu.read(0xFFF5));
    try std.testing.expectEqual(0x3E, cpu.mmu.read(0xFFF4));
    try std.testing.expectEqual(0b01000000, cpu.mmu.read(0xFFF3));
    try std.testing.expectEqual(0x3E, cpu.mmu.read(0xFFF2));
    try std.testing.expectEqual(0b00100000, cpu.mmu.read(0xFFF1));
    try std.testing.expectEqual(0x3E, cpu.mmu.read(0xFFF0));
    try std.testing.expectEqual(0b00000000, cpu.mmu.read(0xFFEF));
    try std.testing.expectEqual(0x0A, cpu.mmu.read(0xFFEE));
    try std.testing.expectEqual(0b00000000, cpu.mmu.read(0xFFED));
    try std.testing.expectEqual(0x0A, cpu.mmu.read(0xFFEC));
    try std.testing.expectEqual(0b01000000, cpu.mmu.read(0xFFEB));
}

test "Arithmetic (16-bit)" {
    // Seed values into memory and do arithmetic with them. Save flags to memory.

    const code =
        \\ ; Initial setup
        \\ LD BC, 0x1234      ; BC = 0x1234
        \\ LD DE, 0x1111      ; DE = 0x1111
        \\ LD HL, 0x0000      ; HL = 0x0000
        \\ LD SP, 0xFFF0      ; SP = 0xFFF0
        \\ 
        \\ ; 16-bit arithmetic
        \\ INC BC             ; BC = 0x1235
        \\ DEC DE             ; DE = 0x1110
        \\ ADD HL, BC         ; HL = 0x1235
        \\ ADD HL, DE         ; HL = 0x2345
        \\ ADD SP, -4         ; SP = 0xFFEC
    ;
    var program = try lib.assembler.translate(code, std.testing.allocator);
    defer std.testing.allocator.free(program);

    program = try std.testing.allocator.realloc(program, program.len + 1);
    program[program.len - 1] = 0xFD; // Illegal instruction signals end of program

    const cpu = try run_program(
        "Arithmetic integ test (16-bit)",
        program,
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(program.len, cpu.reg.PC);

    try std.testing.expectEqual(0x1235, cpu.reg.BC.all());
    try std.testing.expectEqual(0x1110, cpu.reg.DE.all());
    try std.testing.expectEqual(0x2345, cpu.reg.HL.all());
    try std.testing.expectEqual(0xFFEC, cpu.reg.SP.all());
}
