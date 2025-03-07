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
};

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
    A: u8,
    BC: RegisterWithHalves,
    DE: RegisterWithHalves,
    F: RegisterFlags,

    HL: RegisterWithHalves,

    IR: u8,
    SP: RegisterWithHalves,
    PC: u16,

    WZ: RegisterWithHalves,
};

const SelfRefCpuMethod = struct {
    func: *const fn (*Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod,

    fn init(func: fn (*Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod) SelfRefCpuMethod {
        return SelfRefCpuMethod{
            .func = func,
        };
    }
};

pub const Cpu = struct {
    mmu: mmu.Mmu,
    reg: RegisterBank,

    next_tick: SelfRefCpuMethod,

    pub fn init(mmu_: mmu.Mmu) Cpu {
        return Cpu{
            .mmu = mmu_,
            .reg = RegisterBank{
                .A = 0,
                .BC = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .DE = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .F = RegisterFlags{
                    .C = 0,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                },
                .HL = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .IR = 0,
                .SP = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .PC = 0,
                .WZ = RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
            },
            .next_tick = SelfRefCpuMethod.init(Cpu.fetchOpcode),
        };
    }

    pub fn zeroize_regs(self: *Cpu) void {
        self.reg.A = 0x00;
        self.reg.BC.setAll(0x0000);
        self.reg.DE.setAll(0x0000);
        self.reg.HL.setAll(0x0000);
        self.reg.SP.setAll(0x0000);
        self.reg.PC = 0x0000;
    }

    pub fn tick(self: *Cpu) (mmu.MmuMemoryError || CpuError)!void {
        self.next_tick = try self.next_tick.func(self);
    }

    fn fetchPC(self: *Cpu) mmu.MmuMemoryError!u8 {
        const val = try self.mmu.read(self.reg.PC);
        self.reg.PC += 1;
        return val;
    }

    fn fetchOpcode(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.IR = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.decodeOpcode);
    }

    fn decodeOpcode(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        // NOP
        if (self.reg.IR == 0) {
            return self.fetchOpcode();
        }

        // Load register (register)
        if (self.reg.IR & 0b11_000_111 == 0b01_000_110) { // from indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.loadRegisterIndirectHL2);
        }
        if (self.reg.IR & 0b11_111_000 == 0b01_110_000) { // to indirect HL
            const from: u3 = @intCast(self.reg.IR & 0b00_000_111);
            const from_ptr = self.ptrReg8Bit(from);
            try self.mmu.write(self.reg.HL.all(), from_ptr.*);
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }
        if (self.reg.IR & 0b11_000_000 == 0b01_000_000) {
            const to: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
            const from: u3 = @intCast(self.reg.IR & 0b00_000_111);
            const to_ptr = self.ptrReg8Bit(to);
            const from_ptr = self.ptrReg8Bit(from);
            to_ptr.* = from_ptr.*;
            return self.fetchOpcode();
        }

        // Load register (immediate)
        if (self.reg.IR & 0b11_111_111 == 0b00_110_110) { // to indirect HL
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadRegisterImmediateIndirectHL2);
        }
        if (self.reg.IR & 0b11_000_111 == 0b00_000_110) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadRegisterImmediate2);
        }

        // Load accumulator (indirect)
        if (self.reg.IR & 0b111_0_1111 == 0b000_0_1010) {
            const reg: u2 = @intCast((self.reg.IR & 0b000_10_000) >> 4);
            const reg_ptr = self.ptrReg16Bit(reg);
            self.reg.WZ.Lo = try self.mmu.read(reg_ptr.all());
            return SelfRefCpuMethod.init(Cpu.loadAccumulatorIndirect2);
        }

        // Load from accumulator (indirect)
        if (self.reg.IR & 0b111_0_1111 == 0b000_0_0010) {
            const reg: u2 = @intCast((self.reg.IR & 0b000_10_000) >> 4);
            const reg_ptr = self.ptrReg16Bit(reg);
            try self.mmu.write(reg_ptr.all(), self.reg.A);
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }

        // Load accumulator (direct)
        if (self.reg.IR & 0b11111111 == 0b11111010) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadAccumulatorDirect2);
        }

        // Load from accumulator (direct)
        if (self.reg.IR & 0b11111111 == 0b11101010) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadFromAccumulatorDirect2);
        }

        // Load accumulator (indirect 0xFF00+C)
        if (self.reg.IR & 0b11111111 == 0b11110010) {
            const addr = 0xFF00 | @as(u16, self.reg.BC.Lo);
            self.reg.WZ.Lo = try self.mmu.read(addr);
            return SelfRefCpuMethod.init(Cpu.loadAccumulatorIndirectHigh2);
        }

        // Load from accumulator (indirect 0xFF00+C)
        if (self.reg.IR & 0b11111111 == 0b11100010) {
            const addr = 0xFF00 | @as(u16, self.reg.BC.Lo);
            try self.mmu.write(addr, self.reg.A);
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }

        // Load accumulator (direct 0xFF00+immediate)
        if (self.reg.IR & 0b11111111 == 0b11110000) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadAccumulatorDirectHigh2);
        }

        // Load from accumulator (direct 0xFF00+immediate)
        if (self.reg.IR & 0b11111111 == 0b11100000) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadFromAccumulatorDirectHigh2);
        }

        // Load accumulator (indirect HL)
        if (self.reg.IR & 0b111_0_1111 == 0b001_0_1010) {
            const addr = self.reg.HL.all();
            self.reg.WZ.Lo = try self.mmu.read(addr);
            const incdec: u1 = @intCast((self.reg.IR & 0b000_1_0000) >> 4);
            const newval = switch (incdec) {
                0 => addr + 1,
                1 => addr - 1,
            };
            self.reg.HL.setAll(newval);
            return SelfRefCpuMethod.init(Cpu.loadAccumulatorIndirectHL2);
        }

        // Load from accumulator (indirect HL)
        if (self.reg.IR & 0b111_0_1111 == 0b001_0_0010) {
            const addr = self.reg.HL.all();
            try self.mmu.write(addr, self.reg.A);
            const incdec: u1 = @intCast((self.reg.IR & 0b000_1_0000) >> 4);
            const newval = switch (incdec) {
                0 => addr + 1,
                1 => addr - 1,
            };
            self.reg.HL.setAll(newval);
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }

        // Load 16-bit register
        if (self.reg.IR & 0b11_00_1111 == 0b00_00_0001) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.load16bitRegister2);
        }

        return CpuError.IllegalInstruction;
    }

    fn loadRegisterIndirectHL2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const to: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const to_ptr = self.ptrReg8Bit(to);
        to_ptr.* = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadRegisterImmediate2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const to: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const to_ptr = self.ptrReg8Bit(to);
        to_ptr.* = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadRegisterImmediateIndirectHL2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.HL.all(), self.reg.WZ.Lo);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn loadAccumulatorIndirect2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.A = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadAccumulatorDirect2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadAccumulatorDirect3);
    }

    fn loadAccumulatorDirect3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.mmu.read(self.reg.WZ.all());
        return SelfRefCpuMethod.init(Cpu.loadAccumulatorDirect4);
    }

    fn loadAccumulatorDirect4(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.A = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadFromAccumulatorDirect2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadFromAccumulatorDirect3);
    }

    fn loadFromAccumulatorDirect3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.WZ.all(), self.reg.A);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn loadAccumulatorIndirectHigh2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.A = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadAccumulatorDirectHigh2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const addr = 0xFF00 | @as(u16, self.reg.WZ.Lo);
        self.reg.WZ.Lo = try self.mmu.read(addr);
        return SelfRefCpuMethod.init(Cpu.loadAccumulatorDirectHigh3);
    }

    fn loadAccumulatorDirectHigh3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.A = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadFromAccumulatorDirectHigh2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const addr = 0xFF00 | @as(u16, self.reg.WZ.Lo);
        try self.mmu.write(addr, self.reg.A);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn loadAccumulatorIndirectHL2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.A = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn load16bitRegister2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.load16bitRegister3);
    }

    fn load16bitRegister3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const reg: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg);
        reg_ptr.setAll(self.reg.WZ.all());
        return self.fetchOpcode();
    }

    fn ptrReg8Bit(self: *Cpu, idx: u3) *u8 {
        return switch (idx) {
            0b000 => &self.reg.BC.Hi,
            0b001 => &self.reg.BC.Lo,
            0b010 => &self.reg.DE.Hi,
            0b011 => &self.reg.DE.Lo,
            0b100 => &self.reg.HL.Hi,
            0b101 => &self.reg.HL.Lo,
            0b110 => unreachable,
            0b111 => &self.reg.A,
        };
    }

    fn ptrReg16Bit(self: *Cpu, idx: u2) *RegisterWithHalves {
        return switch (idx) {
            0b00 => &self.reg.BC,
            0b01 => &self.reg.DE,
            0b10 => &self.reg.HL,
            0b11 => &self.reg.SP,
        };
    }
};
