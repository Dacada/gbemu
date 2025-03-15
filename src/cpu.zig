const std = @import("std");
const mmu = @import("mmu.zig");
const alu = @import("alu.zig");

pub const CpuError = error{
    IllegalInstruction,
};

const Register = union(enum) {
    AluRegister: *alu.AluRegister,
    RegisterWithHalves: *alu.RegisterWithHalves,

    pub fn all(reg: Register) u16 {
        return switch (reg) {
            .AluRegister => |r| r.all(),
            .RegisterWithHalves => |r| r.all(),
        };
    }

    pub fn setAll(reg: Register, val: u16) void {
        return switch (reg) {
            .AluRegister => |r| r.setAll(val),
            .RegisterWithHalves => |r| r.setAll(val),
        };
    }

    pub fn hi(reg: Register) u8 {
        return switch (reg) {
            .AluRegister => |r| r.Hi,
            .RegisterWithHalves => |r| r.Hi,
        };
    }

    pub fn lo(reg: Register) u8 {
        return switch (reg) {
            .AluRegister => |r| r.Lo.all(),
            .RegisterWithHalves => |r| r.Lo,
        };
    }
};

const RegisterBank = struct {
    AF: alu.AluRegister,
    BC: alu.RegisterWithHalves,
    DE: alu.RegisterWithHalves,

    HL: alu.RegisterWithHalves,

    SP: alu.RegisterWithHalves,
    PC: u16,

    IR: u8,
    WZ: alu.RegisterWithHalves,
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
                .AF = alu.AluRegister{
                    .Hi = 0,
                    .Lo = alu.RegisterFlags{
                        .Z = 0,
                        .N = 0,
                        .H = 0,
                        .C = 0,
                        .rest = 0,
                    },
                },
                .BC = alu.RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .DE = alu.RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .HL = alu.RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .SP = alu.RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
                .PC = 0,
                .IR = 0,
                .WZ = alu.RegisterWithHalves{
                    .Hi = 0,
                    .Lo = 0,
                },
            },
            .next_tick = SelfRefCpuMethod.init(Cpu.fetchOpcode),
        };
    }

    pub fn zeroize_regs(self: *Cpu) void {
        self.reg.AF.setAll(0x0000);
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
        // LOAD 8-BIT

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
            try self.mmu.write(reg_ptr.all(), self.reg.AF.Hi);
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
            try self.mmu.write(addr, self.reg.AF.Hi);
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
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            const incdec: u1 = @intCast((self.reg.IR & 0b000_1_0000) >> 4);
            switch (incdec) {
                0 => self.reg.HL.inc(),
                1 => self.reg.HL.dec(),
            }
            return SelfRefCpuMethod.init(Cpu.loadAccumulatorIndirectHL2);
        }

        // Load from accumulator (indirect HL)
        if (self.reg.IR & 0b111_0_1111 == 0b001_0_0010) {
            try self.mmu.write(self.reg.HL.all(), self.reg.AF.Hi);
            const incdec: u1 = @intCast((self.reg.IR & 0b000_1_0000) >> 4);
            switch (incdec) {
                0 => self.reg.HL.inc(),
                1 => self.reg.HL.dec(),
            }
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }

        // LOAD 16-BIT

        // Load 16-bit register
        if (self.reg.IR & 0b11_00_1111 == 0b00_00_0001) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.load16bitRegister2);
        }

        // Load from stack pointer (direct)
        if (self.reg.IR & 0b11111111 == 0b00001000) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadFrmStackPointerDirect2);
        }

        // Load stack pointer from HL
        if (self.reg.IR & 0b11111111 == 0b11111001) {
            self.reg.SP.setAll(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }

        // Push register
        if (self.reg.IR & 0b11_00_1111 == 0b11_00_0101) {
            self.reg.SP.dec();
            return SelfRefCpuMethod.init(Cpu.pushRegister2);
        }

        // Pop register
        if (self.reg.IR & 0b11_00_1111 == 0b11_00_0001) {
            self.reg.WZ.Lo = try self.mmu.read(self.reg.SP.all());
            self.reg.SP.inc();
            return SelfRefCpuMethod.init(Cpu.popRegister2);
        }

        // Load HL from adjusted SP
        if (self.reg.IR & 0b11111111 == 0b11111000) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadHLfromAdjustedSP2);
        }

        // ARITHMETIC 8-BIT

        // Add register
        if (self.reg.IR & 0b1111_0_111 == 0b1000_0_110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doAdd);
        }
        if (self.reg.IR & 0b1111_0_000 == 0b1000_0_000) {
            const reg: u3 = @intCast(self.reg.IR & 0b0000_0_111);
            const reg_ptr = self.ptrReg8Bit(reg);
            self.reg.WZ.Lo = reg_ptr.*;
            return self.doAdd();
        }

        // Add immediate
        if (self.reg.IR & 0b1111_0_111 == 0b1100_0_110) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.doAdd);
        }

        // Subtract register
        if (self.reg.IR & 0b1111_0_111 == 0b1001_0_110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doSub);
        }
        if (self.reg.IR & 0b1111_0_000 == 0b1001_0_000) {
            const reg: u3 = @intCast(self.reg.IR & 0b0000_0_111);
            const reg_ptr = self.ptrReg8Bit(reg);
            self.reg.WZ.Lo = reg_ptr.*;
            return self.doSub();
        }

        // Subtract immediate
        if (self.reg.IR & 0b1111_0_111 == 0b1101_0_110) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.doSub);
        }

        // Compare register
        if (self.reg.IR & 0b11111111 == 0b10111110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doCmp);
        }
        if (self.reg.IR & 0b11111000 == 0b10111000) {
            const reg: u3 = @intCast(self.reg.IR & 0b0000_0_111);
            const reg_ptr = self.ptrReg8Bit(reg);
            self.reg.WZ.Lo = reg_ptr.*;
            return self.doCmp();
        }

        // Compare immediate
        if (self.reg.IR & 0b11111111 == 0b11111110) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.doCmp);
        }

        // Increment register
        if (self.reg.IR & 0b11111111 == 0b00110100) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.incrementRegisterHL2);
        }
        if (self.reg.IR & 0b11_000_111 == 0b00_000_100) {
            const reg: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
            const reg_ptr = self.ptrReg8Bit(reg);
            reg_ptr.* = self.reg.AF.inc(reg_ptr.*);
            return self.fetchOpcode();
        }

        // Decrement register
        if (self.reg.IR & 0b11111111 == 0b00110101) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.decrementRegisterHL2);
        }
        if (self.reg.IR & 0b11_000_111 == 0b00_000_101) {
            const reg: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
            const reg_ptr = self.ptrReg8Bit(reg);
            reg_ptr.* = self.reg.AF.dec(reg_ptr.*);
            return self.fetchOpcode();
        }

        // AND register
        if (self.reg.IR & 0b11111111 == 0b10100110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doAnd);
        }
        if (self.reg.IR & 0b11111_000 == 0b10100_000) {
            const reg: u3 = @intCast(self.reg.IR & 0b00000_111);
            const reg_ptr = self.ptrReg8Bit(reg);
            self.reg.WZ.Lo = reg_ptr.*;
            return self.doAnd();
        }

        // AND immediate
        if (self.reg.IR & 0b11111111 == 0b11100110) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.doAnd);
        }

        // OR register
        if (self.reg.IR & 0b11111111 == 0b10110110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doOr);
        }
        if (self.reg.IR & 0b11111_000 == 0b10110_000) {
            const reg: u3 = @intCast(self.reg.IR & 0b00000_111);
            const reg_ptr = self.ptrReg8Bit(reg);
            self.reg.WZ.Lo = reg_ptr.*;
            return self.doOr();
        }

        // OR immediate
        if (self.reg.IR & 0b11111111 == 0b11110110) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.doOr);
        }

        // XOR register
        if (self.reg.IR & 0b11111111 == 0b10101110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doXor);
        }
        if (self.reg.IR & 0b11111_000 == 0b10101_000) {
            const reg: u3 = @intCast(self.reg.IR & 0b00000_111);
            const reg_ptr = self.ptrReg8Bit(reg);
            self.reg.WZ.Lo = reg_ptr.*;
            return self.doXor();
        }

        // XOR immediate
        if (self.reg.IR & 0b11111111 == 0b11101110) {
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.doXor);
        }

        // Complement carry flag
        if (self.reg.IR & 0b11111111 == 0b00111111) {
            self.reg.AF.ccf();
            return self.fetchOpcode();
        }

        // Set carry flag
        if (self.reg.IR & 0b11111111 == 0b00110111) {
            self.reg.AF.scf();
            return self.fetchOpcode();
        }

        // Decimal adjust accumulator
        if (self.reg.IR & 0b11111111 == 0b00100111) {
            self.reg.AF.daa();
            return self.fetchOpcode();
        }

        // Complement accumulator
        if (self.reg.IR & 0b11111111 == 0b00101111) {
            self.reg.AF.cpl();
            return self.fetchOpcode();
        }

        // NOP
        if (self.reg.IR & 0b11111111 == 0b00000000) {
            return self.fetchOpcode();
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
        self.reg.AF.Hi = self.reg.WZ.Lo;
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
        self.reg.AF.Hi = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadFromAccumulatorDirect2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadFromAccumulatorDirect3);
    }

    fn loadFromAccumulatorDirect3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.WZ.all(), self.reg.AF.Hi);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn loadAccumulatorIndirectHigh2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadAccumulatorDirectHigh2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const addr = 0xFF00 | @as(u16, self.reg.WZ.Lo);
        self.reg.WZ.Lo = try self.mmu.read(addr);
        return SelfRefCpuMethod.init(Cpu.loadAccumulatorDirectHigh3);
    }

    fn loadAccumulatorDirectHigh3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.WZ.Lo;
        return self.fetchOpcode();
    }

    fn loadFromAccumulatorDirectHigh2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const addr = 0xFF00 | @as(u16, self.reg.WZ.Lo);
        try self.mmu.write(addr, self.reg.AF.Hi);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn loadAccumulatorIndirectHL2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.WZ.Lo;
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

    fn loadFrmStackPointerDirect2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadFromStackPointerDirect3);
    }

    fn loadFromStackPointerDirect3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.WZ.all(), self.reg.SP.Lo);
        self.reg.WZ.inc();
        return SelfRefCpuMethod.init(Cpu.loadFromStackPointerDirect4);
    }

    fn loadFromStackPointerDirect4(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.WZ.all(), self.reg.SP.Hi);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn pushRegister2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const reg: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrRegGeneric(reg);
        try self.mmu.write(self.reg.SP.all(), reg_ptr.hi());
        self.reg.SP.dec();
        return SelfRefCpuMethod.init(Cpu.pushRegister3);
    }

    fn pushRegister3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const reg: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrRegGeneric(reg);
        try self.mmu.write(self.reg.SP.all(), reg_ptr.lo());
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn popRegister2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.mmu.read(self.reg.SP.all());
        self.reg.SP.inc();
        return SelfRefCpuMethod.init(Cpu.popRegister3);
    }

    fn popRegister3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const reg: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrRegGeneric(reg);
        reg_ptr.setAll(self.reg.WZ.all());
        return self.fetchOpcode();
    }

    fn loadHLfromAdjustedSP2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const tmp = self.reg.AF.Hi;
        self.reg.AF.Hi = self.reg.SP.Lo;
        self.reg.AF.add(self.reg.WZ.Lo, 0);
        self.reg.HL.Lo = self.reg.AF.Hi;
        self.reg.AF.Hi = tmp;
        self.reg.AF.Lo.Z = 0;
        return SelfRefCpuMethod.init(Cpu.loadHLfromAdjustedSP3);
    }

    fn loadHLfromAdjustedSP3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const adj: u8 = if ((self.reg.WZ.Lo & 0b1000_0000) >> 7 == 1) 0xFF else 0x00;
        const tmp, _ = @addWithOverflow(self.reg.SP.Hi, adj);
        self.reg.HL.Hi, _ = @addWithOverflow(tmp, self.reg.AF.Lo.C);
        return self.fetchOpcode();
    }

    fn incrementRegisterHL2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const res = self.reg.AF.inc(self.reg.WZ.Lo);
        try self.mmu.write(self.reg.HL.all(), res);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn decrementRegisterHL2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const res = self.reg.AF.dec(self.reg.WZ.Lo);
        try self.mmu.write(self.reg.HL.all(), res);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn doAdd(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const with_carry: u1 = @intCast((self.reg.IR & 0b0000_1_000) >> 3);
        self.reg.AF.add(self.reg.WZ.Lo, with_carry);
        return self.fetchOpcode();
    }

    fn doSub(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const with_carry: u1 = @intCast((self.reg.IR & 0b0000_1_000) >> 3);
        self.reg.AF.sub(self.reg.WZ.Lo, with_carry);
        return self.fetchOpcode();
    }

    fn doCmp(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const a = self.reg.AF.Hi;
        self.reg.AF.sub(self.reg.WZ.Lo, 0);
        self.reg.AF.Hi = a;
        return self.fetchOpcode();
    }

    fn doAnd(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.AF.and_(self.reg.WZ.Lo);
        return self.fetchOpcode();
    }

    fn doOr(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.AF.or_(self.reg.WZ.Lo);
        return self.fetchOpcode();
    }

    fn doXor(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.AF.xor(self.reg.WZ.Lo);
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
            0b111 => &self.reg.AF.Hi,
        };
    }

    fn ptrReg16Bit(self: *Cpu, idx: u2) *alu.RegisterWithHalves {
        return switch (idx) {
            0b00 => &self.reg.BC,
            0b01 => &self.reg.DE,
            0b10 => &self.reg.HL,
            0b11 => &self.reg.SP,
        };
    }

    fn ptrRegGeneric(self: *Cpu, idx: u2) Register {
        return switch (idx) {
            0b00 => Register{ .RegisterWithHalves = &self.reg.BC },
            0b01 => Register{ .RegisterWithHalves = &self.reg.DE },
            0b10 => Register{ .RegisterWithHalves = &self.reg.HL },
            0b11 => Register{ .AluRegister = &self.reg.AF },
        };
    }
};
