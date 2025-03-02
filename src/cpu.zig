const std = @import("std");
const mmu = @import("mmu.zig");

pub const CpuError = error{
    IllegalInstruction,
};

const RegisterFlags = packed struct {
    Z: u1,
    N: u1,
    H: u1,
    C: u1,
    rest: u4,

    fn all(reg: RegisterFlags) u8 {
        return (@as(u8, reg.Z) << 7) | (@as(u8, reg.N) << 6) | (@as(u8, reg.H) << 5) | (@as(u8, reg.C) << 4) | reg.rest;
    }

    fn setAll(reg: *RegisterFlags, val: u8) void {
        reg.Z = @intCast((val & 0b1000_0000) >> 7);
        reg.N = @intCast((val & 0b0100_0000) >> 6);
        reg.H = @intCast((val & 0b0010_0000) >> 5);
        reg.C = @intCast((val & 0b0001_0000) >> 4);
        reg.rest = @intCast(val & 0b0000_1111);
    }
};

test "register flags" {
    var reg: RegisterFlags = undefined;

    reg.setAll(0xAA);
    try std.testing.expectEqual(1, reg.Z);
    try std.testing.expectEqual(0, reg.N);
    try std.testing.expectEqual(1, reg.H);
    try std.testing.expectEqual(0, reg.C);
    try std.testing.expectEqual(0xA, reg.rest);

    reg.Z = 0;
    reg.N = 1;
    reg.H = 0;
    reg.C = 1;
    reg.rest = 0x5;
    try std.testing.expectEqual(0x55, reg.all());
}

const RegisterWithFlags = packed struct {
    Hi: u8,
    Lo: RegisterFlags,

    fn all(reg: RegisterWithFlags) u16 {
        return (@as(u16, reg.Hi) << 8) | reg.Lo.all();
    }

    fn setAll(reg: *RegisterWithFlags, val: u16) void {
        reg.Hi = @intCast((val & 0xFF00) >> 8);
        reg.Lo.setAll(@intCast(val & 0x00FF));
    }
};

test "register with flags" {
    var reg: RegisterWithFlags = undefined;

    reg.setAll(0xABCD);
    try std.testing.expectEqual(0xAB, reg.Hi);
    try std.testing.expectEqual(0xCD, reg.Lo.all());

    reg.Hi = 0x12;
    reg.Lo.setAll(0x34);
    try std.testing.expectEqual(0x1234, reg.all());
}

const RegisterWithHalves = packed struct {
    Hi: u8,
    Lo: u8,

    fn all(reg: RegisterWithHalves) u16 {
        return (@as(u16, reg.Hi) << 8) | reg.Lo;
    }

    fn setAll(reg: *RegisterWithHalves, val: u16) void {
        reg.Hi = @intCast((val & 0xFF00) >> 8);
        reg.Lo = @intCast(val & 0x00FF);
    }
};

test "register with halves" {
    var reg: RegisterWithHalves = undefined;

    reg.setAll(0xABCD);
    try std.testing.expectEqual(0xAB, reg.Hi);
    try std.testing.expectEqual(0xCD, reg.Lo);

    reg.Hi = 0x12;
    reg.Lo = 0x34;
    try std.testing.expectEqual(0x1234, reg.all());
}

const RegisterBank = struct {
    AF: RegisterWithFlags,
    BC: RegisterWithHalves,
    DE: RegisterWithHalves,
    HL: RegisterWithHalves,
    SP: u16,
    PC: u16,
};

const CpuState = enum {
    fetch_opcode_only,
    start_current_opcode,
};

pub const Cpu = struct {
    mmu: mmu.Mmu,
    register_bank: RegisterBank,

    state: CpuState,
    current_opcode: u8,

    pub fn init(mmu_: mmu.Mmu) Cpu {
        return Cpu{
            .mmu = mmu_,
            .register_bank = RegisterBank{
                .AF = RegisterWithFlags{
                    .Hi = 0,
                    .Lo = RegisterFlags{
                        .Z = 0,
                        .N = 0,
                        .H = 0,
                        .C = 0,
                        .rest = 0,
                    },
                },
                .BC = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .DE = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .HL = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .SP = 0,
                .PC = 0,
            },

            .state = CpuState.fetch_opcode_only,
            .current_opcode = undefined,
        };
    }

    pub fn zeroize_regs(self: *Cpu) void {
        self.register_bank.AF.setAll(0x0000);
        self.register_bank.BC.setAll(0x0000);
        self.register_bank.DE.setAll(0x0000);
        self.register_bank.HL.setAll(0x0000);
        self.register_bank.SP = 0x0000;
        self.register_bank.PC = 0x0000;
    }

    pub fn tick(self: *Cpu) (mmu.MmuMemoryError || CpuError)!void {
        switch (self.state) {
            CpuState.fetch_opcode_only => try self.fetchNextOpcode(),
            CpuState.start_current_opcode => try self.decodeAndSetup(),
        }
    }

    fn fetchNextOpcode(self: *Cpu) mmu.MmuMemoryError!void {
        self.current_opcode = try self.mmu.read(self.register_bank.PC);
        self.register_bank.PC += 1;
        self.state = CpuState.start_current_opcode;
    }

    fn ptrOpcodeRegOrMmu(self: *Cpu, idx: u3, comptime ro: bool) if (ro) mmu.MmuMemoryError!*const u8 else mmu.MmuMemoryError!*u8 {
        return switch (idx) {
            0b000 => &self.register_bank.BC.Hi,
            0b001 => &self.register_bank.BC.Lo,
            0b010 => &self.register_bank.DE.Hi,
            0b011 => &self.register_bank.DE.Lo,
            0b100 => &self.register_bank.HL.Hi,
            0b101 => &self.register_bank.HL.Lo,
            0b110 => {
                if (ro) {
                    return try self.mmu.referenceRO(self.register_bank.HL.all());
                } else {
                    return try self.mmu.referenceRW(self.register_bank.HL.all());
                }
            },
            0b111 => &self.register_bank.AF.Hi,
        };
    }

    fn decodeAndSetup(self: *Cpu) (mmu.MmuMemoryError || CpuError)!void {
        if (self.current_opcode == 0xFD) {
            return CpuError.IllegalInstruction;
        }

        // NOP
        else if (self.current_opcode == 0) {
            try self.fetchNextOpcode();
        }

        // HALT
        else if (self.current_opcode == 0x76) {
            // TODO: Figure out how HALT works
            return CpuError.IllegalInstruction;
        }

        // LD register or indirect (HL)
        else if ((self.current_opcode & 0b11_000_000) >> 6 == 0b01) {
            const to: u3 = @intCast((self.current_opcode & 0b00_111_000) >> 3);
            const from: u3 = @intCast(self.current_opcode & 0b00_000_111);
            const from_ptr = try self.ptrOpcodeRegOrMmu(from, true);
            const to_ptr = try self.ptrOpcodeRegOrMmu(to, false);
            to_ptr.* = from_ptr.*;

            if (to == 0b110 or from == 0b110) {
                // we already used the bus to write to ram, so fetch has to wait until the next tick
                self.state = CpuState.fetch_opcode_only;
            } else {
                try self.fetchNextOpcode();
            }
        }

    }
};

test "fetch" {
    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);
    rom[0] = 0xAA;
    var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
    defer mmu_.deinit();
    var cpu = Cpu.init(mmu_);

    try cpu.tick();

    try std.testing.expectEqual(0xAA, cpu.current_opcode);
    try std.testing.expectEqual(CpuState.start_current_opcode, cpu.state);
}

test "nop" {
    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);
    rom[0] = 0x00;
    rom[1] = 0xFD;
    var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
    defer mmu_.deinit();
    var cpu = Cpu.init(mmu_);

    try cpu.tick(); // fetch nop
    try std.testing.expectEqual(1, cpu.register_bank.PC); // pc advances during fetch
    try cpu.tick(); // execute nop and fetch the illegal instruction
    try std.testing.expectEqual(2, cpu.register_bank.PC); // pc has advanced
    try std.testing.expectError(CpuError.IllegalInstruction, cpu.tick()); // execute the next instruction (and fetch the third one)
}

fn helper_set_cpu_register(cpu: *Cpu, reg: u3, val: u8) !void {
    switch (reg) {
        0b000 => cpu.register_bank.BC.Hi = val,
        0b001 => cpu.register_bank.BC.Lo = val,
        0b010 => cpu.register_bank.DE.Hi = val,
        0b011 => cpu.register_bank.DE.Lo = val,
        0b100 => cpu.register_bank.HL.Hi = val,
        0b101 => cpu.register_bank.HL.Lo = val,
        0b111 => cpu.register_bank.AF.Hi = val,
        else => undefined,
    }
}

fn helper_expect_cpu_equal(actual: *const Cpu, expected: *const Cpu) !void {
    try std.testing.expectEqualSlices(u8, expected.mmu.rom, actual.mmu.rom);
    try std.testing.expectEqualSlices(u8, expected.mmu.wram, actual.mmu.wram);
    try std.testing.expectEqual(expected.register_bank.AF.all(), actual.register_bank.AF.all());
    try std.testing.expectEqual(expected.register_bank.BC.all(), actual.register_bank.BC.all());
    try std.testing.expectEqual(expected.register_bank.DE.all(), actual.register_bank.DE.all());
    try std.testing.expectEqual(expected.register_bank.HL.all(), actual.register_bank.HL.all());
    try std.testing.expectEqual(expected.register_bank.SP, actual.register_bank.SP);
    try std.testing.expectEqual(expected.register_bank.PC, actual.register_bank.PC);
}

test "ld register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |from| {
        if (from == 0b110) {
            continue;
        }
        for (0..(0b111 + 1)) |to| {
            if (to == 0b110) {
                continue;
            }

            // Constants
            const instr: u8 = @intCast((0b01 << 6) | (to << 3) | from);
            const test_value: u8 = 0x0D;

            // setup CPU state
            var rom = try std.testing.allocator.alloc(u8, 0x8000);
            defer std.testing.allocator.free(rom);
            @memset(rom, 0x00);
            rom[0] = 0x00;
            rom[1] = instr;
            rom[2] = 0xFD;

            var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
            defer mmu_.deinit();
            mmu_.zeroize();

            var cpu = Cpu.init(mmu_);
            cpu.zeroize_regs();
            try helper_set_cpu_register(&cpu, @intCast(from), test_value);

            // Setup expected state after first tick
            var mmu_1 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
            defer mmu_1.deinit();
            mmu_1.zeroize();

            var cpu_1 = Cpu.init(mmu_1);
            cpu_1.zeroize_regs();
            try helper_set_cpu_register(&cpu_1, @intCast(from), test_value);
            cpu_1.register_bank.PC = 0x0001;

            // Setup expected state after second tick
            var mmu_2 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
            defer mmu_2.deinit();
            mmu_2.zeroize();

            var cpu_2 = Cpu.init(mmu_1);
            cpu_2.zeroize_regs();
            try helper_set_cpu_register(&cpu_2, @intCast(from), test_value);
            cpu_2.register_bank.PC = 0x0002;

            // Setup expected state after third tick
            var mmu_3 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
            defer mmu_3.deinit();
            mmu_3.zeroize();

            var cpu_3 = Cpu.init(mmu_1);
            cpu_3.zeroize_regs();
            try helper_set_cpu_register(&cpu_3, @intCast(from), test_value);
            try helper_set_cpu_register(&cpu_3, @intCast(to), test_value);
            cpu_3.register_bank.PC = 0x0003;

            // Execute
            try cpu.tick(); // load nop
            try helper_expect_cpu_equal(&cpu, &cpu_1);
            try cpu.tick(); // execute nop and load instruction under test
            try helper_expect_cpu_equal(&cpu, &cpu_2);
            try cpu.tick(); // execute instruction under test and load illegal instruction
            try helper_expect_cpu_equal(&cpu, &cpu_3);
        }
    }
}

test "ld register indirect from every register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |from| {
        if (from == 0b110) {
            continue;
        }
        const instr: u8 = @intCast((0b01 << 6) | (0b110 << 3) | from);
        const test_addr: u16 = 0xD0D0;
        const test_value: u8 = 0xD0; // this makes the reading from H or from L test case easy

        var rom = try std.testing.allocator.alloc(u8, 0x8000);
        defer std.testing.allocator.free(rom);
        rom[0] = 0x00;
        rom[1] = instr;
        rom[2] = 0xFD;

        var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_.deinit();
        mmu_.zeroize();

        var cpu = Cpu.init(mmu_);
        cpu.zeroize_regs();
        try helper_set_cpu_register(&cpu, @intCast(from), test_value);
        cpu.register_bank.HL.setAll(test_addr);

        // Setup expected state after first tick
        var mmu_1 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_1.deinit();
        mmu_1.zeroize();

        var cpu_1 = Cpu.init(mmu_1);
        cpu_1.zeroize_regs();
        try helper_set_cpu_register(&cpu_1, @intCast(from), test_value);
        cpu_1.register_bank.HL.setAll(test_addr);
        cpu_1.register_bank.PC = 0x0001;

        // Setup expected state after second tick
        var mmu_2 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_2.deinit();
        mmu_2.zeroize();

        var cpu_2 = Cpu.init(mmu_2);
        cpu_2.zeroize_regs();
        try helper_set_cpu_register(&cpu_2, @intCast(from), test_value);
        cpu_2.register_bank.HL.setAll(test_addr);
        cpu_2.register_bank.PC = 0x0002;

        // Setup expected state after third tick
        var mmu_3 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_3.deinit();
        mmu_3.zeroize();
        try mmu_3.write(test_addr, test_value);

        var cpu_3 = Cpu.init(mmu_3);
        cpu_3.zeroize_regs();
        try helper_set_cpu_register(&cpu_3, @intCast(from), test_value);
        cpu_3.register_bank.HL.setAll(test_addr);
        cpu_3.register_bank.PC = 0x0002;

        // Setup expected state after fourth tick
        var mmu_4 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_4.deinit();
        mmu_4.zeroize();
        try mmu_4.write(test_addr, test_value);

        var cpu_4 = Cpu.init(mmu_4);
        cpu_4.zeroize_regs();
        try helper_set_cpu_register(&cpu_4, @intCast(from), test_value);
        cpu_4.register_bank.HL.setAll(test_addr);
        cpu_4.register_bank.PC = 0x0003;

        try cpu.tick(); // load nop
        try helper_expect_cpu_equal(&cpu, &cpu_1);
        try cpu.tick(); // execute nop and load instruction under test
        try helper_expect_cpu_equal(&cpu, &cpu_2);
        try cpu.tick(); // execute instruction under test (write to ram)
        try helper_expect_cpu_equal(&cpu, &cpu_3);
        try cpu.tick(); // load illegal instruction
        try helper_expect_cpu_equal(&cpu, &cpu_4);
    }
}

test "ld register indirect to every register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }
        const instr: u8 = @intCast((0b01 << 6) | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD0D0;

        var rom = try std.testing.allocator.alloc(u8, 0x8000);
        defer std.testing.allocator.free(rom);
        rom[0] = 0x00;
        rom[1] = instr;
        rom[2] = 0xFD;

        var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_.deinit();
        mmu_.zeroize();
        try mmu_.write(test_addr, test_value);

        var cpu = Cpu.init(mmu_);
        cpu.zeroize_regs();
        cpu.register_bank.HL.setAll(test_addr);

        // Setup expected state after first tick
        var mmu_1 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_1.deinit();
        mmu_1.zeroize();
        try mmu_1.write(test_addr, test_value);

        var cpu_1 = Cpu.init(mmu_1);
        cpu_1.zeroize_regs();
        cpu_1.register_bank.HL.setAll(test_addr);
        cpu_1.register_bank.PC = 0x0001;

        // Setup expected state after second tick
        var mmu_2 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_2.deinit();
        mmu_2.zeroize();
        try mmu_2.write(test_addr, test_value);

        var cpu_2 = Cpu.init(mmu_2);
        cpu_2.zeroize_regs();
        cpu_2.register_bank.HL.setAll(test_addr);
        cpu_2.register_bank.PC = 0x0002;

        // Setup expected state after second tick
        var mmu_3 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_3.deinit();
        mmu_3.zeroize();
        try mmu_3.write(test_addr, test_value);

        var cpu_3 = Cpu.init(mmu_3);
        cpu_3.zeroize_regs();
        cpu_3.register_bank.HL.setAll(test_addr);
        cpu_3.register_bank.PC = 0x0002;
        try helper_set_cpu_register(&cpu_3, @intCast(to), test_value);

        // Setup expected state after second tick
        var mmu_4 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_4.deinit();
        mmu_4.zeroize();
        try mmu_4.write(test_addr, test_value);

        var cpu_4 = Cpu.init(mmu_4);
        cpu_4.zeroize_regs();
        cpu_4.register_bank.HL.setAll(test_addr);
        cpu_4.register_bank.PC = 0x0003;
        try helper_set_cpu_register(&cpu_4, @intCast(to), test_value);

        try cpu.tick(); // load nop
        try helper_expect_cpu_equal(&cpu, &cpu_1);
        try cpu.tick(); // execute nop and load instruction under test
        try helper_expect_cpu_equal(&cpu, &cpu_2);
        try cpu.tick(); // execute instruction under test (read from ram)
        try helper_expect_cpu_equal(&cpu, &cpu_3);
        try cpu.tick(); // load illegal instruction
        try helper_expect_cpu_equal(&cpu, &cpu_4);
    }
}

test "ld immediate" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }

        // Constants
        const instr: u8 = @intCast(0b00 << 6 | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;

        // setup CPU state
        var rom = try std.testing.allocator.alloc(u8, 0x8000);
        defer std.testing.allocator.free(rom);
        @memset(rom, 0x00);
        rom[0] = 0x00;
        rom[1] = instr;
        rom[2] = test_value;
        rom[3] = 0xFD;

        var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_.deinit();
        mmu_.zeroize();

        var cpu = Cpu.init(mmu_);
        cpu.zeroize_regs();

        // Setup expected state after first tick
        var mmu_1 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_1.deinit();
        mmu_1.zeroize();

        var cpu_1 = Cpu.init(mmu_1);
        cpu_1.zeroize_regs();
        cpu_1.register_bank.PC = 0x0001;

        // Setup expected state after second tick
        var mmu_2 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_2.deinit();
        mmu_2.zeroize();

        var cpu_2 = Cpu.init(mmu_2);
        cpu_2.zeroize_regs();
        cpu_2.register_bank.PC = 0x0002;

        // Setup expected state after third tick
        var mmu_3 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_3.deinit();
        mmu_3.zeroize();

        var cpu_3 = Cpu.init(mmu_3);
        cpu_3.zeroize_regs();
        cpu_3.register_bank.PC = 0x0003;
        try helper_set_cpu_register(&cpu_3, @intCast(to), test_value);

        // Setup expected state after fourth tick
        var mmu_4 = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_4.deinit();
        mmu_4.zeroize();

        var cpu_4 = Cpu.init(mmu_4);
        cpu_4.zeroize_regs();
        cpu_4.register_bank.PC = 0x0004;
        try helper_set_cpu_register(&cpu_4, @intCast(to), test_value);

        // Execute
        try cpu.tick(); // load nop
        try helper_expect_cpu_equal(&cpu, &cpu_1);
        try cpu.tick(); // execute nop and load instruction under test
        try helper_expect_cpu_equal(&cpu, &cpu_2);
        try cpu.tick(); // execute instruction under test: load immediate into register
        try helper_expect_cpu_equal(&cpu, &cpu_3);
        try cpu.tick(); // load illegal instruction
        try helper_expect_cpu_equal(&cpu, &cpu_4);
    }
}
