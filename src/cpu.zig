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

    IME: u1,
};

const SelfRefCpuMethod = struct {
    func: *const fn (*Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod,

    fn init(func: fn (*Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod) SelfRefCpuMethod {
        return SelfRefCpuMethod{
            .func = func,
        };
    }
};

const CpuOp1Union = union {
    tmp: u8,
    next_alu_op: *const fn (*alu.AluRegister, u8) u8,
    next_alu_op_bit: *const fn (u8, u3) u8,
    next_alu_op_bit_test: *const fn (*alu.AluRegister, u8, u3) void,
};

const CpuOp2Union = union {
    bitIdx: u3,
};

pub const Cpu = struct {
    mmu: mmu.Mmu,
    reg: RegisterBank,

    next_tick: SelfRefCpuMethod,
    next_op_1: CpuOp1Union = undefined,
    next_op_2: CpuOp2Union = undefined,

    enable_interrupt_next_instruction: bool = false,
    enable_interrupt_current_instruction: bool = false,

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
                .IME = 0,
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

    pub fn onInstructionStart(self: *Cpu) void {
        if (self.enable_interrupt_next_instruction) {
            self.enable_interrupt_next_instruction = false;
            self.enable_interrupt_current_instruction = true;
        }
    }

    pub fn onInstructionEnd(self: *Cpu) void {
        if (self.enable_interrupt_current_instruction) {
            self.reg.IME = 1;
            self.enable_interrupt_current_instruction = false;
        }
    }

    pub fn tick(self: *Cpu) (mmu.MmuMemoryError || CpuError)!void {
        if (self.next_tick.func == Cpu.decodeOpcode) {
            self.onInstructionStart();
        }
        self.next_tick = try self.next_tick.func(self);
        if (self.next_tick.func == Cpu.decodeOpcode) {
            self.onInstructionEnd();
        }
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
        // Decoding logic from pan docs.
        const block: u2 = @intCast((self.reg.IR & 0b11_000000) >> 6);
        return switch (block) {
            0b00 => self.decodeBlock0(),
            0b01 => self.decodeBlock1(),
            0b10 => self.decodeBlock2(),
            0b11 => self.decodeBlock3(),
        };
    }

    fn decodeBlock0(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const suffix: u3 = @intCast(self.reg.IR & 0b00_000_111);

        switch (suffix) {
            0b000 => {
                const bit5: u1 = @intCast((self.reg.IR & 0b00_100_000) >> 5);
                const bit43: u2 = @intCast((self.reg.IR & 0b00_011_000) >> 3);

                if (bit5 == 1) {
                    return self.executeJumpRelativeConditional();
                }

                switch (bit43) {
                    0b00 => {
                        return self.executeNop();
                    },
                    0b01 => {
                        return self.executeLoadImmediateIndirectStackPointer();
                    },
                    0b10 => {
                        return self.executeStop();
                    },
                    0b11 => {
                        return self.executeJumpRelative();
                    },
                }
            },
            0b001 => {
                const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);

                if (bit3 == 1) {
                    return self.executeAddRegister16BitToRegisterHL();
                } else {
                    return self.executeLoadImmediateToRegister16Bit();
                }
            },
            0b010 => {
                const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);

                if (bit3 == 0) {
                    return self.executeLoadIndirectRegister16BitFromAccumulator();
                } else {
                    return self.executeLoadIndirectRegister16BitToAccumulator();
                }
            },
            0b011 => {
                const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);

                if (bit3 == 0) {
                    return self.executeIncrementRegister16Bit();
                } else {
                    return self.executeDecrementRegister16Bit();
                }
            },
            0b100 => {
                return self.executeIncrementRegister8Bit();
            },
            0b101 => {
                return self.executeDecrementRegister8Bit();
            },
            0b110 => {
                return self.executeLoadImmediateToRegister8Bit();
            },
            0b111 => {
                const bit543: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
                switch (bit543) {
                    0b000 => return self.executeRotateLeftThroughCarryAccumulator(),
                    0b001 => return self.executeRotateRightThroughCarryAccumulator(),
                    0b010 => return self.executeRotateLeftAccumulator(),
                    0b011 => return self.executeRotateRightAccumulator(),
                    0b100 => return self.executeDecimalAdjustAccumulator(),
                    0b101 => return self.executeComplementAccumulator(),
                    0b110 => return self.executeSetCarryFlag(),
                    0b111 => return self.executeComplementCarryFlag(),
                }
            },
        }
    }

    fn decodeBlock1(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR == 0b01_110_110) {
            return self.executeHalt();
        }
        return self.executeRegisterToRegisterLoad8Bit();
    }

    fn decodeBlock2(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        return switch (infix) {
            0b000 => self.executeAddRegister8Bit(),
            0b001 => self.executeAddRegister8Bit(), // with carry
            0b010 => self.executeSubRegister8Bit(),
            0b011 => self.executeSubRegister8Bit(), // with carry
            0b100 => self.executeAndRegister8Bit(),
            0b101 => self.executeXorRegister8Bit(),
            0b110 => self.executeOrRegister8Bit(),
            0b111 => self.executeCpRegister8Bit(),
        };
    }

    fn decodeBlock3(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const suffix: u3 = @intCast(self.reg.IR & 0b00_000_111);
        switch (suffix) {
            0b000 => {
                const bit5: u1 = @intCast((self.reg.IR & 0b00_100_000) >> 5);
                if (bit5 == 0) {
                    return self.executeReturnConditional();
                }

                const bit43: u2 = @intCast((self.reg.IR & 0b00_011_000) >> 3);
                return switch (bit43) {
                    0b00 => self.executeLoadIndirectImmediateStackFromAccumulator(),
                    0b01 => self.executeAddRelativeImmediateToStackPointer(),
                    0b10 => self.executeLoadIndirectImmediateStackToAccumulator(),
                    0b11 => self.executeLoadToHLFromIndirectAdjustedStackPointer(),
                };
            },
            0b001 => {
                const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);
                if (bit3 == 0) {
                    return self.executePop();
                }

                const bit54: u2 = @intCast((self.reg.IR & 0b00_110_000) >> 4);
                return switch (bit54) {
                    0b00 => self.executeReturn(),
                    0b01 => self.executeReturnFromInterrupt(),
                    0b10 => self.executeJumpToHL(),
                    0b11 => self.executeLoadStackPointerFromHL(),
                };
            },
            0b010 => {
                const bit5: u1 = @intCast((self.reg.IR & 0b00_100_000) >> 5);
                if (bit5 == 0) {
                    return self.executeJumpConditional();
                }

                const bit43: u2 = @intCast((self.reg.IR & 0b00_011_000) >> 3);
                return switch (bit43) {
                    0b00 => self.executeLoadIndirectRegisterCStackFromAccumulator(),
                    0b01 => self.executeLoadIndirectImmediate16BitFromAccumulator(),
                    0b10 => self.executeLoadIndirectRegisterCStackToAccumulator(),
                    0b11 => self.executeLoadIndirectImmediate16BitToAccumulator(),
                };
            },
            0b011 => {
                const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
                return switch (infix) {
                    0b000 => self.executeJump(),
                    0b001 => self.executePrefix(),
                    0b110 => self.executeDisableInterrupt(),
                    0b111 => self.executeEnableInterrupt(),
                    else => CpuError.IllegalInstruction,
                };
            },
            0b100 => {
                const bit5: u1 = @intCast((self.reg.IR & 0b00_100_000) >> 5);
                if (bit5 == 1) {
                    return CpuError.IllegalInstruction;
                }
                return self.executeCallConditional();
            },
            0b101 => {
                const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);
                if (bit3 == 0) {
                    return self.executePush();
                }

                if ((self.reg.IR & 0b00_110_000) != 0) {
                    return CpuError.IllegalInstruction;
                }

                return self.executeCall();
            },
            0b110 => {
                const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
                return switch (infix) {
                    0b000 => self.executeAddImmediate(),
                    0b001 => self.executeAddImmediate(), // with carry
                    0b010 => self.executeSubImmediate(),
                    0b011 => self.executeSubImmediate(), // with carry
                    0b100 => self.executeAndImmediate(),
                    0b101 => self.executeXorImmediate(),
                    0b110 => self.executeOrImmediate(),
                    0b111 => self.executeCpImmediate(),
                };
            },
            0b111 => {
                return self.executeCallReset();
            },
        }
    }

    fn executePrefix(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.IR = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.decodeOpcodePrefixed);
    }

    fn executeNop(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        return self.fetchOpcode();
    }

    fn executeHalt(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        // TODO
        return self.fetchOpcode();
    }

    fn executeStop(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        // TODO
        return self.fetchOpcode();
    }

    fn executeDisableInterrupt(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.enable_interrupt_next_instruction = false;
        self.enable_interrupt_current_instruction = false;
        self.reg.IME = 0;
        return self.fetchOpcode();
    }

    fn executeEnableInterrupt(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.enable_interrupt_next_instruction = true;
        return self.fetchOpcode();
    }

    fn executeRegisterToRegisterLoad8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00_000_111 == 0b00_000_110) { // from indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.loadRegisterIndirectHL2);
        }
        if (self.reg.IR & 0b00_111_000 == 0b00_110_000) { // to indirect HL
            const from: u3 = @intCast(self.reg.IR & 0b00_000_111);
            const from_ptr = self.ptrReg8Bit(from);
            try self.mmu.write(self.reg.HL.all(), from_ptr.*);
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }

        const to: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const from: u3 = @intCast(self.reg.IR & 0b00_000_111);
        const to_ptr = self.ptrReg8Bit(to);
        const from_ptr = self.ptrReg8Bit(from);
        to_ptr.* = from_ptr.*;
        return self.fetchOpcode();
    }

    fn executeLoadImmediateToRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        // Load register (immediate)
        if (self.reg.IR & 0b00_111_000 == 0b00_110_000) { // to indirect HL
            self.reg.WZ.Lo = try self.fetchPC();
            return SelfRefCpuMethod.init(Cpu.loadRegisterImmediateIndirectHL2);
        }

        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadRegisterImmediate2);
    }

    fn executeLoadIndirectRegister16BitToAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        // Indirect HL
        if (self.reg.IR & 0b00_1_00000 == 0b00_1_00000) {
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            const incdec: u1 = @intCast((self.reg.IR & 0b000_1_0000) >> 4);
            switch (incdec) {
                0 => self.reg.HL.inc(),
                1 => self.reg.HL.dec(),
            }
            return SelfRefCpuMethod.init(Cpu.loadAccumulatorIndirectHL2);
        }

        const reg: u2 = @intCast((self.reg.IR & 0b000_10_000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg);
        self.reg.WZ.Lo = try self.mmu.read(reg_ptr.all());
        return SelfRefCpuMethod.init(Cpu.loadAccumulatorIndirect2);
    }

    fn executeLoadIndirectRegister16BitFromAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        // Indirect HL
        if (self.reg.IR & 0b00_1_00000 == 0b00_1_00000) {
            try self.mmu.write(self.reg.HL.all(), self.reg.AF.Hi);
            const incdec: u1 = @intCast((self.reg.IR & 0b000_1_0000) >> 4);
            switch (incdec) {
                0 => self.reg.HL.inc(),
                1 => self.reg.HL.dec(),
            }
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }

        // if (self.reg.IR & 0b11_1_0_1_111 == 0b00_0_0_0_010) {
        const reg: u2 = @intCast((self.reg.IR & 0b000_10_000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg);
        try self.mmu.write(reg_ptr.all(), self.reg.AF.Hi);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn executeLoadIndirectImmediate16BitToAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadAccumulatorDirect2);
    }

    fn executeLoadIndirectImmediate16BitFromAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadFromAccumulatorDirect2);
    }

    fn executeLoadIndirectRegisterCStackToAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const addr = 0xFF00 | @as(u16, self.reg.BC.Lo);
        self.reg.WZ.Lo = try self.mmu.read(addr);
        return SelfRefCpuMethod.init(Cpu.loadAccumulatorIndirectHigh2);
    }

    fn executeLoadIndirectRegisterCStackFromAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const addr = 0xFF00 | @as(u16, self.reg.BC.Lo);
        try self.mmu.write(addr, self.reg.AF.Hi);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn executeLoadIndirectImmediateStackToAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadAccumulatorDirectHigh2);
    }

    fn executeLoadIndirectImmediateStackFromAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadFromAccumulatorDirectHigh2);
    }

    fn executeLoadImmediateToRegister16Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.load16bitRegister2);
    }

    fn executeLoadImmediateIndirectStackPointer(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadFrmStackPointerDirect2);
    }

    fn executeLoadStackPointerFromHL(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.SP.setAll(self.reg.HL.all());
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn executePush(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.SP.dec();
        return SelfRefCpuMethod.init(Cpu.pushRegister2);
    }

    fn executePop(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.mmu.read(self.reg.SP.all());
        self.reg.SP.inc();
        return SelfRefCpuMethod.init(Cpu.popRegister2);
    }

    fn executeLoadToHLFromIndirectAdjustedStackPointer(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.loadHLfromAdjustedSP2);
    }

    fn executeAddRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00000_111 == 0b00000_110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doAdd);
        }

        const reg: u3 = @intCast(self.reg.IR & 0b0000_0_111);
        const reg_ptr = self.ptrReg8Bit(reg);
        self.reg.WZ.Lo = reg_ptr.*;
        return self.doAdd();
    }

    fn executeAddImmediate(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.doAdd);
    }

    fn executeSubRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00000_111 == 0b00000_110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doSub);
        }

        const reg: u3 = @intCast(self.reg.IR & 0b0000_0_111);
        const reg_ptr = self.ptrReg8Bit(reg);
        self.reg.WZ.Lo = reg_ptr.*;
        return self.doSub();
    }

    fn executeSubImmediate(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.doSub);
    }

    fn executeCpRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00000_111 == 0b00000_110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doCmp);
        }

        const reg: u3 = @intCast(self.reg.IR & 0b0000_0_111);
        const reg_ptr = self.ptrReg8Bit(reg);
        self.reg.WZ.Lo = reg_ptr.*;
        return self.doCmp();
    }

    fn executeCpImmediate(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.doCmp);
    }

    fn executeIncrementRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00_111_000 == 0b00_110_000) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.incrementRegisterHL2);
        }

        const reg: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const reg_ptr = self.ptrReg8Bit(reg);
        reg_ptr.* = self.reg.AF.inc(reg_ptr.*);
        return self.fetchOpcode();
    }

    fn executeDecrementRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00_111_000 == 0b00_110_000) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.decrementRegisterHL2);
        }

        const reg: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const reg_ptr = self.ptrReg8Bit(reg);
        reg_ptr.* = self.reg.AF.dec(reg_ptr.*);
        return self.fetchOpcode();
    }

    fn executeAndRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00000_111 == 0b00000_110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doAnd);
        }

        const reg: u3 = @intCast(self.reg.IR & 0b00000_111);
        const reg_ptr = self.ptrReg8Bit(reg);
        self.reg.WZ.Lo = reg_ptr.*;
        return self.doAnd();
    }

    fn executeAndImmediate(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.doAnd);
    }

    fn executeOrRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00000_111 == 0b00000_110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doOr);
        }

        const reg: u3 = @intCast(self.reg.IR & 0b00000_111);
        const reg_ptr = self.ptrReg8Bit(reg);
        self.reg.WZ.Lo = reg_ptr.*;
        return self.doOr();
    }

    fn executeOrImmediate(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.doOr);
    }

    fn executeXorRegister8Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        if (self.reg.IR & 0b00000_111 == 0b00000_110) { // indirect HL
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
            return SelfRefCpuMethod.init(Cpu.doXor);
        }

        const reg: u3 = @intCast(self.reg.IR & 0b00000_111);
        const reg_ptr = self.ptrReg8Bit(reg);
        self.reg.WZ.Lo = reg_ptr.*;
        return self.doXor();
    }

    fn executeXorImmediate(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.doXor);
    }

    fn executeComplementCarryFlag(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.AF.ccf();
        return self.fetchOpcode();
    }

    fn executeSetCarryFlag(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.AF.scf();
        return self.fetchOpcode();
    }

    fn executeDecimalAdjustAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.AF.daa();
        return self.fetchOpcode();
    }

    fn executeComplementAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.AF.cpl();
        return self.fetchOpcode();
    }

    fn executeIncrementRegister16Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const reg_code: u2 = @intCast((self.reg.IR & 0b00_11_0_000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg_code);
        reg_ptr.inc();
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn executeDecrementRegister16Bit(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const reg_code: u2 = @intCast((self.reg.IR & 0b00_11_0_000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg_code);
        reg_ptr.dec();
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn executeAddRegister16BitToRegisterHL(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const reg_code: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg_code);
        self.reg.HL.Lo = self.reg.AF.add_return(self.reg.HL.Lo, reg_ptr.Lo, 0, self.reg.AF.Lo.Z);
        return SelfRefCpuMethod.init(Cpu.addRegister162);
    }

    fn executeAddRelativeImmediateToStackPointer(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.addToSPRelative2);
    }

    fn executeRotateLeftThroughCarryAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.AF.rlc(self.reg.AF.Hi);
        self.reg.AF.Lo.Z = 0;
        return self.fetchOpcode();
    }

    fn executeRotateRightThroughCarryAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.AF.rrc(self.reg.AF.Hi);
        self.reg.AF.Lo.Z = 0;
        return self.fetchOpcode();
    }

    fn executeRotateLeftAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.AF.rl(self.reg.AF.Hi);
        self.reg.AF.Lo.Z = 0;
        return self.fetchOpcode();
    }

    fn executeRotateRightAccumulator(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.AF.rr(self.reg.AF.Hi);
        self.reg.AF.Lo.Z = 0;
        return self.fetchOpcode();
    }

    fn executeJump(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.jumpToImmediate2);
    }

    fn executeJumpToHL(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.PC = self.reg.HL.all();
        return self.fetchOpcode();
    }

    fn executeJumpConditional(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.jumpToImmediateConditional2);
    }

    fn executeJumpRelative(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.jumpToRelative2);
    }

    fn executeJumpRelativeConditional(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        const cond: u2 = @intCast((self.reg.IR & 0b00_11_000) >> 3);
        if (self.reg.AF.cond(cond)) {
            return SelfRefCpuMethod.init(Cpu.jumpToRelativeConditional2);
        } else {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }
    }

    fn executeCall(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.call2);
    }

    fn executeCallConditional(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.callConditional2);
    }

    fn executeReturn(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.mmu.read(self.reg.SP.all());
        self.reg.SP.inc();
        return SelfRefCpuMethod.init(Cpu.return2);
    }

    fn executeReturnConditional(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
        if (self.reg.AF.cond(cond)) {
            return SelfRefCpuMethod.init(Cpu.returnConditional2);
        } else {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }
    }

    fn executeReturnFromInterrupt(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.mmu.read(self.reg.SP.all());
        self.reg.SP.inc();
        return SelfRefCpuMethod.init(Cpu.returnInterrupt2);
    }

    fn executeCallReset(self: *Cpu) (mmu.MmuMemoryError || CpuError)!SelfRefCpuMethod {
        self.reg.SP.dec();
        return SelfRefCpuMethod.init(Cpu.restart2);
    }

    fn decodeOpcodePrefixed(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const op1: u2 = @intCast((self.reg.IR & 0b11_000_000) >> 6);
        const op2: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const regIdx: u3 = @intCast(self.reg.IR & 0b00_000_111);
        const isMemory = regIdx == 0b110;

        if (isMemory) {
            self.reg.WZ.Lo = try self.mmu.read(self.reg.HL.all());
        }
        const reg = self.ptrReg8Bit(regIdx);

        switch (op1) {
            0b00 => { // shift/rotate/swap
                self.next_op_1 = CpuOp1Union{ .next_alu_op = switch (op2) {
                    0b000 => alu.AluRegister.rlc,
                    0b001 => alu.AluRegister.rrc,
                    0b010 => alu.AluRegister.rl,
                    0b011 => alu.AluRegister.rr,
                    0b100 => alu.AluRegister.sla,
                    0b101 => alu.AluRegister.sra,
                    0b110 => alu.AluRegister.swap,
                    0b111 => alu.AluRegister.srl,
                } };
                if (isMemory) {
                    return SelfRefCpuMethod.init(Cpu.aluOpOnMemory);
                }
                reg.* = self.next_op_1.next_alu_op(&self.reg.AF, reg.*);
                return self.fetchOpcode();
            },
            0b01 => { // bit test
                self.next_op_1 = CpuOp1Union{ .next_alu_op_bit_test = alu.AluRegister.bit };
                if (isMemory) {
                    self.next_op_2.bitIdx = op2;
                    return SelfRefCpuMethod.init(Cpu.aluOpOnMemoryBitsNoWriteBack);
                }
                self.next_op_1.next_alu_op_bit_test(&self.reg.AF, reg.*, op2);
                return self.fetchOpcode();
            },
            else => { // bit set/reset
                self.next_op_1 = CpuOp1Union{ .next_alu_op_bit = switch (op1) {
                    0b10 => alu.AluRegister.res,
                    0b11 => alu.AluRegister.set,
                    else => unreachable,
                } };
                if (isMemory) {
                    self.next_op_2.bitIdx = op2;
                    return SelfRefCpuMethod.init(Cpu.aluOpOnMemoryBits);
                }
                reg.* = self.next_op_1.next_alu_op_bit(reg.*, op2);
                return self.fetchOpcode();
            },
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
        self.reg.HL.Lo = self.reg.AF.add_return(self.reg.SP.Lo, self.reg.WZ.Lo, 0, 0);
        return SelfRefCpuMethod.init(Cpu.loadHLfromAdjustedSP3);
    }

    fn loadHLfromAdjustedSP3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.HL.Hi = self.reg.AF.add_adj(self.reg.SP.Hi, self.reg.WZ.Lo);
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

    fn addRegister162(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const reg_code: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg_code);
        self.reg.HL.Hi = self.reg.AF.add_return(self.reg.HL.Hi, reg_ptr.Hi, 1, self.reg.AF.Lo.Z);
        return self.fetchOpcode();
    }

    fn addToSPRelative2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.next_op_1 = CpuOp1Union{ .tmp = self.reg.WZ.Lo };
        self.reg.WZ.Lo = self.reg.AF.add_return(self.reg.SP.Lo, self.next_op_1.tmp, 0, 0);
        return SelfRefCpuMethod.init(Cpu.addToSPRelative3);
    }

    fn addToSPRelative3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = self.reg.AF.add_adj(self.reg.SP.Hi, self.next_op_1.tmp);
        return SelfRefCpuMethod.init(Cpu.addToSPRelative4);
    }

    fn addToSPRelative4(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.SP.setAll(self.reg.WZ.all());
        return self.fetchOpcode();
    }

    fn jumpToImmediate2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.jumpToImmediate3);
    }

    fn jumpToImmediate3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.PC = self.reg.WZ.all();
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn jumpToImmediateConditional2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
        if (self.reg.AF.cond(cond)) {
            return SelfRefCpuMethod.init(Cpu.jumpToImmediateConditional3);
        } else {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }
    }

    fn jumpToImmediateConditional3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.PC = self.reg.WZ.all();
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn jumpToRelative2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const res = alu.AluRegister.add_interpret_signed_no_flags(self.reg.PC, self.reg.WZ.Lo);
        self.reg.WZ.setAll(res);
        return SelfRefCpuMethod.init(Cpu.jumpToRelative3);
    }

    fn jumpToRelative3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.PC = self.reg.WZ.all();
        return self.fetchOpcode();
    }

    fn jumpToRelativeConditional2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const res = alu.AluRegister.add_interpret_signed_no_flags(self.reg.PC, self.reg.WZ.Lo);
        self.reg.WZ.setAll(res);
        return SelfRefCpuMethod.init(Cpu.jumpToRelativeConditional3);
    }

    fn jumpToRelativeConditional3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.PC = self.reg.WZ.all();
        return self.fetchOpcode();
    }

    fn call2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.call3);
    }

    fn call3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.SP.dec();
        return SelfRefCpuMethod.init(Cpu.call4);
    }

    fn call4(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.SP.all(), @intCast((self.reg.PC & 0xFF00) >> 8));
        self.reg.SP.dec();
        return SelfRefCpuMethod.init(Cpu.call5);
    }

    fn call5(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.SP.all(), @intCast(self.reg.PC & 0x00FF));
        self.reg.PC = self.reg.WZ.all();
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn callConditional2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.fetchPC();
        const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
        if (self.reg.AF.cond(cond)) {
            return SelfRefCpuMethod.init(Cpu.call3);
        } else {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }
    }

    fn return2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.mmu.read(self.reg.SP.all());
        self.reg.SP.inc();
        return SelfRefCpuMethod.init(Cpu.return3);
    }

    fn return3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.PC = self.reg.WZ.all();
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn returnConditional2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Lo = try self.mmu.read(self.reg.SP.all());
        self.reg.SP.inc();
        return SelfRefCpuMethod.init(Cpu.return2);
    }

    fn returnInterrupt2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.WZ.Hi = try self.mmu.read(self.reg.SP.all());
        self.reg.SP.inc();
        return SelfRefCpuMethod.init(Cpu.returnInterrupt3);
    }

    fn returnInterrupt3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.reg.PC = self.reg.WZ.all();
        self.reg.IME = 1;
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn restart2(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.SP.all(), @intCast((self.reg.PC & 0xFF00) >> 8));
        self.reg.SP.dec();
        return SelfRefCpuMethod.init(Cpu.restart3);
    }

    fn restart3(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        try self.mmu.write(self.reg.SP.all(), @intCast(self.reg.PC & 0x00FF));
        self.reg.PC = 0x0000 | ((self.reg.IR & 0b00_111_000) >> 3);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn aluOpOnMemory(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const res = self.next_op_1.next_alu_op(&self.reg.AF, self.reg.WZ.Lo);
        try self.mmu.write(self.reg.HL.all(), res);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn aluOpOnMemoryBits(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        const res = self.next_op_1.next_alu_op_bit(self.reg.WZ.Lo, self.next_op_2.bitIdx);
        try self.mmu.write(self.reg.HL.all(), res);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn aluOpOnMemoryBitsNoWriteBack(self: *Cpu) mmu.MmuMemoryError!SelfRefCpuMethod {
        self.next_op_1.next_alu_op_bit_test(&self.reg.AF, self.reg.WZ.Lo, self.next_op_2.bitIdx);
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
            0b110 => undefined,
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
