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

    pub fn all(reg: RegisterFlags) u8 {
        return (@as(u8, reg.Z) << 7) | (@as(u8, reg.N) << 6) | (@as(u8, reg.H) << 5) | (@as(u8, reg.C) << 4) | reg.rest;
    }

    pub fn setAll(reg: *RegisterFlags, val: u8) void {
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

    pub fn all(reg: RegisterWithFlags) u16 {
        return (@as(u16, reg.Hi) << 8) | reg.Lo.all();
    }

    pub fn setAll(reg: *RegisterWithFlags, val: u16) void {
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

    pub fn all(reg: RegisterWithHalves) u16 {
        return (@as(u16, reg.Hi) << 8) | reg.Lo;
    }

    pub fn setAll(reg: *RegisterWithHalves, val: u16) void {
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
    SP: RegisterWithHalves, // We don't use the 8bit addressing but uniformity makes some opcodes simpler
    PC: u16,
};

const CpuState = enum {
    start_execution_current_opcode,
    fetch_opcode,
    store_value_8_bit,
    fetch_address_msb_from_program,
    store_or_load_accumulator,
    fetch_value_msb_from_program,
    store_to_16bit_register,
};

pub const Cpu = struct {
    mmu: mmu.Mmu,
    register_bank: RegisterBank,

    state: CpuState,
    current_opcode: u8,

    intermediate_8bit: u8,
    intermediate_16bit: u16,

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
                .SP = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .PC = 0,
            },

            .state = CpuState.fetch_opcode,
            .current_opcode = undefined,
            .intermediate_8bit = undefined,
            .intermediate_16bit = undefined,
        };
    }

    pub fn zeroize_regs(self: *Cpu) void {
        self.register_bank.AF.setAll(0x0000);
        self.register_bank.BC.setAll(0x0000);
        self.register_bank.DE.setAll(0x0000);
        self.register_bank.HL.setAll(0x0000);
        self.register_bank.SP.setAll(0x0000);
        self.register_bank.PC = 0x0000;
    }

    pub fn tick(self: *Cpu) (mmu.MmuMemoryError || CpuError)!void {
        self.state = switch (self.state) {
            CpuState.start_execution_current_opcode => try self.startExecutionCurrentOpcode(),
            CpuState.fetch_opcode => try self.fetchOpcode(),
            CpuState.store_value_8_bit => try self.storeValue8Bit(),
            CpuState.fetch_address_msb_from_program => try self.fetchAddressMsbFromProgram(),
            CpuState.store_or_load_accumulator => try self.storeOrLoadAccumulator(),
            CpuState.fetch_value_msb_from_program => try self.fetchValueMsbFromProgram(),
            CpuState.store_to_16bit_register => try self.storeTo16BitRegister(),
        };
    }

    fn ptrRegOrMmu8Bit(self: *Cpu, idx: u3, comptime ro: bool) if (ro) mmu.MmuMemoryError!*const u8 else mmu.MmuMemoryError!*u8 {
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

    fn ptrReg16Bit(self: *Cpu, idx: u2) *RegisterWithHalves {
        return switch (idx) {
            0b00 => &self.register_bank.BC,
            0b01 => &self.register_bank.DE,
            0b10 => &self.register_bank.HL,
            0b11 => &self.register_bank.SP,
        };
    }

    fn fetchValueFromProgram(self: *Cpu) mmu.MmuMemoryError!u8 {
        const val = try self.mmu.read(self.register_bank.PC);
        self.register_bank.PC += 1;
        return val;
    }

    fn fetchValueFromProgramThenStoreAsMsb(self: *Cpu) mmu.MmuMemoryError!void {
        var val: u16 = try self.fetchValueFromProgram();
        val <<= 8;
        self.intermediate_16bit |= val;
    }

    fn startExecutionCurrentOpcode(self: *Cpu) (mmu.MmuMemoryError || CpuError)!CpuState {
        // NOP
        if (self.current_opcode == 0) {
            return try self.fetchOpcode();
        }

        // HALT
        if (self.current_opcode == 0x76) {
            // TODO: Figure out how HALT works
            return CpuError.IllegalInstruction;
        }

        // LD register or indirect (HL)
        if ((self.current_opcode & 0b11_000_000) >> 6 == 0b01) {
            const to: u3 = @intCast((self.current_opcode & 0b00_111_000) >> 3);
            const from: u3 = @intCast(self.current_opcode & 0b00_000_111);
            const from_ptr = try self.ptrRegOrMmu8Bit(from, true);
            const to_ptr = try self.ptrRegOrMmu8Bit(to, false);
            to_ptr.* = from_ptr.*;

            if (to == 0b110 or from == 0b110) {
                return CpuState.fetch_opcode;
            } else {
                return try self.fetchOpcode();
            }
        }

        // LD register or indirect (HL) immediate
        if (self.current_opcode & 0b11_000_111 == 0b00_000_110) {
            const to: u3 = @intCast((self.current_opcode & 0b00_111_000) >> 3);
            const val = try self.fetchValueFromProgram();

            self.intermediate_8bit = val;
            if (to == 0b110) {
                return CpuState.store_value_8_bit;
            } else {
                return self.storeValue8Bit();
            }
        }

        // LD accumulator indirect
        if (self.current_opcode & 0b111_00_111 == 0b000_00_010) {
            const reg: u1 = @intCast((self.current_opcode & 0b000_10_000) >> 4);
            const addr = switch (reg) {
                0 => self.register_bank.BC.all(),
                1 => self.register_bank.DE.all(),
            };
            const rw = (self.current_opcode & 0b000_01_000) >> 3;

            if (rw == 1) {
                self.register_bank.AF.Hi = try self.mmu.read(addr);
            } else {
                try self.mmu.write(addr, self.register_bank.AF.Hi);
            }
            return CpuState.fetch_opcode;
        }

        // LD accumulator direct
        if (self.current_opcode & 0b111_0_1111 == 0b111_0_1010) {
            self.intermediate_16bit = try self.fetchValueFromProgram();
            return CpuState.fetch_address_msb_from_program;
        }

        // LDH accumulator indirect
        if (self.current_opcode & 0b111_0_1111 == 0b111_0_0010) {
            const addr: u16 = 0xFF00 | @as(u16, self.register_bank.BC.Lo);
            const rw = (self.current_opcode & 0b000_1_0000) >> 4;
            if (rw == 1) {
                self.register_bank.AF.Hi = try self.mmu.read(addr);
            } else {
                try self.mmu.write(addr, self.register_bank.AF.Hi);
            }
            return CpuState.fetch_opcode;
        }

        // LDH accumulator direct
        if (self.current_opcode & 0b111_0_1111 == 0b111_0_0000) {
            const immediate: u16 = try self.fetchValueFromProgram();
            self.intermediate_16bit = 0xFF00 | immediate;
            return CpuState.store_or_load_accumulator;
        }

        // LD accumulator indirect HL inc/dec
        if (self.current_opcode & 0b111_00_111 == 0b001_00_010) {
            const addr = self.register_bank.HL.all();
            const decinc = (self.current_opcode & 0b000_10_000) >> 4;
            const rw = (self.current_opcode & 0b000_01_000) >> 3;

            if (rw == 1) {
                self.register_bank.AF.Hi = try self.mmu.read(addr);
            } else {
                try self.mmu.write(addr, self.register_bank.AF.Hi);
            }

            if (decinc == 1) {
                self.register_bank.HL.setAll(addr - 1);
            } else {
                self.register_bank.HL.setAll(addr + 1);
            }

            return CpuState.fetch_opcode;
        }

        // LD immediate 16bit
        if (self.current_opcode & 0b11_00_1111 == 0b00_00_0001) {
            self.intermediate_16bit = try self.fetchValueFromProgram();
            return CpuState.fetch_value_msb_from_program;
        }

        // Catch anything else
        return CpuError.IllegalInstruction;
    }

    fn fetchOpcode(self: *Cpu) mmu.MmuMemoryError!CpuState {
        self.current_opcode = try self.fetchValueFromProgram();
        return CpuState.start_execution_current_opcode;
    }

    fn storeValue8Bit(self: *Cpu) mmu.MmuMemoryError!CpuState {
        const to: u3 = @intCast((self.current_opcode & 0b00_111_000) >> 3);
        const to_ptr = try self.ptrRegOrMmu8Bit(to, false);
        to_ptr.* = self.intermediate_8bit;
        self.intermediate_8bit = undefined;
        return CpuState.fetch_opcode;
    }

    fn fetchAddressMsbFromProgram(self: *Cpu) mmu.MmuMemoryError!CpuState {
        try self.fetchValueFromProgramThenStoreAsMsb();
        return CpuState.store_or_load_accumulator;
    }

    fn fetchValueMsbFromProgram(self: *Cpu) mmu.MmuMemoryError!CpuState {
        try self.fetchValueFromProgramThenStoreAsMsb();
        return CpuState.store_to_16bit_register;
    }

    fn storeOrLoadAccumulator(self: *Cpu) mmu.MmuMemoryError!CpuState {
        const rw = (self.current_opcode & 0b000_1_0000) >> 4;
        if (rw == 1) {
            self.register_bank.AF.Hi = try self.mmu.read(self.intermediate_16bit);
        } else {
            try self.mmu.write(self.intermediate_16bit, self.register_bank.AF.Hi);
        }
        self.intermediate_16bit = undefined;
        return CpuState.fetch_opcode;
    }

    fn storeTo16BitRegister(self: *Cpu) mmu.MmuMemoryError!CpuState {
        const which: u2 = @intCast((self.current_opcode & 0b00_11_0000) >> 4);
        const reg = self.ptrReg16Bit(which);
        reg.setAll(self.intermediate_16bit);
        return try self.fetchOpcode();
    }
};

// Very basic tests

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
    try std.testing.expectEqual(CpuState.start_execution_current_opcode, cpu.state);
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
