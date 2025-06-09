const std = @import("std");
const mmu = @import("mmu.zig");
const alu = @import("alu.zig");
const MemoryReference = @import("reference.zig").MemoryReference;

const logger = std.log.scoped(.cpu);

// The reason for this union is that we implement BC, DE, HL as RegisterWithHalves but AF as AluRegister so this union
// allows us to use them indistinctively.
const StackRegister = union(enum) {
    AluRegister: *alu.AluRegister,
    RegisterWithHalves: *alu.RegisterWithHalves,

    pub fn all(reg: StackRegister) u16 {
        return switch (reg) {
            .AluRegister => |r| r.all(),
            .RegisterWithHalves => |r| r.all(),
        };
    }

    pub fn setAll(reg: StackRegister, val: u16) void {
        return switch (reg) {
            .AluRegister => |r| r.setAll(val),
            .RegisterWithHalves => |r| r.setAll(val),
        };
    }

    pub fn hi(reg: StackRegister) u8 {
        return switch (reg) {
            .AluRegister => |r| r.Hi,
            .RegisterWithHalves => |r| r.Hi,
        };
    }

    pub fn lo(reg: StackRegister) u8 {
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
    func: *const fn (*Cpu) SelfRefCpuMethod,

    fn init(func: fn (*Cpu) SelfRefCpuMethod) SelfRefCpuMethod {
        return SelfRefCpuMethod{
            .func = func,
        };
    }
};

const CpuOp1Union = union {
    auxIntermediate: u8,
    next_alu_op: *const fn (*alu.AluRegister, u8) u8,
    next_alu_op_bit: *const fn (u8, u3) u8,
    next_alu_op_bit_test: *const fn (*alu.AluRegister, u8, u3) void,
    to_8bit: MemoryReference,
    ptr_reg_stack: StackRegister,
    ptr_reg_16bit: *alu.RegisterWithHalves,
};

const CpuOp2Union = union {
    bitIdx: u3,
    from_8bit: MemoryReference,
    jump_sets_ime: bool,
};

const CpuOp3Union = union {
    extendedAluOpTarget: MemoryReference,
};

pub const Cpu = struct {
    mmu: *mmu.Mmu,
    reg: RegisterBank,

    next_tick: SelfRefCpuMethod,
    next_op_1: CpuOp1Union = undefined,
    next_op_2: CpuOp2Union = undefined,
    next_op_3: CpuOp3Union = undefined,

    enable_interrupt_next_instruction: bool = false,
    enable_interrupt_current_instruction: bool = false,

    _illegalInstructionExecuted: bool = false,

    breakpoint_instruction: ?u8,
    _breakpoint_happened: bool = false,

    pub fn illegalInstructionExecuted(self: *const Cpu) bool {
        return self._illegalInstructionExecuted;
    }

    pub fn breakpointHappened(self: *const Cpu) bool {
        return self._breakpoint_happened;
    }

    pub fn clearBreakpoint(self: *Cpu) void {
        self._breakpoint_happened = false;
    }

    pub fn instructionBoundary(self: *const Cpu) bool {
        return self.next_tick.func == Cpu.decodeOpcode;
    }

    pub fn init(mmu_: *mmu.Mmu, breakpoint_instruction: ?u8) Cpu {
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
            .breakpoint_instruction = breakpoint_instruction,
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

    pub fn tick(self: *Cpu) void {
        if (self.instructionBoundary()) {
            self.onInstructionStart();
        }
        self.next_tick = self.next_tick.func(self);
        if (self.instructionBoundary()) {
            self.onInstructionEnd();
        }
    }

    // DECODE //

    pub fn decodeOpcode(self: *Cpu) SelfRefCpuMethod {
        self._illegalInstructionExecuted = false;
        if (self.breakpoint_instruction != null and self.reg.IR == self.breakpoint_instruction) {
            self._breakpoint_happened = true;
        }

        // Decoding logic from pan docs.
        const block: u2 = @intCast((self.reg.IR & 0b11_000000) >> 6);
        return switch (block) {
            0b00 => self.decodeBlock0(),
            0b01 => self.decodeBlock1(),
            0b10 => self.decodeBlock2(),
            0b11 => self.decodeBlock3(),
        };
    }

    fn decodeBlock0(self: *Cpu) SelfRefCpuMethod {
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
                        return self.fetchImmediateAndContinue(Cpu.executeLoadStackPointerToImmediateIndirect);
                    },
                    0b10 => {
                        return self.executeStop();
                    },
                    0b11 => {
                        return self.fetchImmediateAndContinue(Cpu.executeJumpToRelative);
                    },
                }
            },
            0b001 => {
                const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);

                if (bit3 == 1) {
                    return self.executeAddRegister16BitToRegisterHL();
                } else {
                    return self.fetchImmediateAndContinue(Cpu.executeLoad16BitRegisterFetchHighByte);
                }
            },
            0b010 => {
                return self.executeLoadIndirectRegister16BitAccumulator();
            },
            0b011 => {
                return self.executeIncDecRegister16Bit();
            },
            0b100 => {
                return self.executeIncDecRegister8Bit();
            },
            0b101 => {
                return self.executeIncDecRegister8Bit();
            },
            0b110 => {
                return self.fetchImmediateAndContinue(Cpu.executeLoadImmediateToRegister8Bit);
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

    fn decodeBlock1(self: *Cpu) SelfRefCpuMethod {
        if (self.reg.IR == 0b01_110_110) {
            return self.executeHalt();
        }
        return self.executeRegisterToRegisterLoad8Bit();
    }

    fn decodeBlock2(self: *Cpu) SelfRefCpuMethod {
        const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        return switch (infix) {
            0b000 => self.runAluOperationOnRegister8Bit(Cpu.doAdd),
            0b001 => self.runAluOperationOnRegister8Bit(Cpu.doAdd), // with carry
            0b010 => self.runAluOperationOnRegister8Bit(Cpu.doSub),
            0b011 => self.runAluOperationOnRegister8Bit(Cpu.doSub), // with carry
            0b100 => self.runAluOperationOnRegister8Bit(Cpu.doAnd),
            0b101 => self.runAluOperationOnRegister8Bit(Cpu.doXor),
            0b110 => self.runAluOperationOnRegister8Bit(Cpu.doOr),
            0b111 => self.runAluOperationOnRegister8Bit(Cpu.doCmp),
        };
    }

    fn decodeBlock3(self: *Cpu) SelfRefCpuMethod {
        const suffix: u3 = @intCast(self.reg.IR & 0b00_000_111);
        switch (suffix) {
            0b000 => {
                const bit5: u1 = @intCast((self.reg.IR & 0b00_100_000) >> 5);
                if (bit5 == 0) {
                    return self.executeReturnConditional();
                }

                const bit43: u2 = @intCast((self.reg.IR & 0b00_011_000) >> 3);
                return switch (bit43) {
                    0b00 => self.fetchImmediateAndContinue(Cpu.executeLoadFromAccumulatorImmediateIndirectToHighRam),
                    0b01 => self.fetchImmediateAndContinue(Cpu.executeAddToStackPointerRelative),
                    0b10 => self.fetchImmediateAndContinue(Cpu.executeLoadAccumulatorImmediateIndirectFromHighRam),
                    0b11 => self.fetchImmediateAndContinue(Cpu.executeLoadHLFromAdjustedStackPointer),
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
                    0b11 => self.executeLoadSPRegisterFromHLRegister(),
                };
            },
            0b010 => {
                const bit5: u1 = @intCast((self.reg.IR & 0b00_100_000) >> 5);
                if (bit5 == 0) {
                    return self.fetchImmediateAndContinue(Cpu.executeJumpToImmediateConditional);
                }

                const bit43: u2 = @intCast((self.reg.IR & 0b00_011_000) >> 3);
                return switch (bit43) {
                    0b00 => self.executeLoadIndirectRegisterCStackAccumulator(),
                    0b10 => self.executeLoadIndirectRegisterCStackAccumulator(),
                    0b01 => self.fetchImmediateAndContinue(Cpu.executeLoadImmediateIndirectFromAccumulator),
                    0b11 => self.fetchImmediateAndContinue(Cpu.executeLoadAccumulatorImmediateIndirect),
                };
            },
            0b011 => {
                const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
                return switch (infix) {
                    0b000 => self.fetchImmediateAndContinue(Cpu.executeJumpToImmediate),
                    0b001 => self.executePrefix(),
                    0b110 => self.executeDisableInterrupt(),
                    0b111 => self.executeEnableInterrupt(),
                    else => self.executeIllegalInstruction(),
                };
            },
            0b100 => {
                const bit5: u1 = @intCast((self.reg.IR & 0b00_100_000) >> 5);
                if (bit5 == 1) {
                    return self.executeIllegalInstruction();
                }
                return self.fetchImmediateAndContinue(Cpu.executeCallConditional);
            },
            0b101 => {
                const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);
                if (bit3 == 0) {
                    return self.executePush();
                }

                if ((self.reg.IR & 0b00_110_000) != 0) {
                    return self.executeIllegalInstruction();
                }

                return self.fetchImmediateAndContinue(Cpu.executeCall);
            },
            0b110 => {
                const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
                return switch (infix) {
                    0b000 => self.fetchImmediateAndContinue(Cpu.doAdd),
                    0b001 => self.fetchImmediateAndContinue(Cpu.doAdd), // with carry
                    0b010 => self.fetchImmediateAndContinue(Cpu.doSub),
                    0b011 => self.fetchImmediateAndContinue(Cpu.doSub), // with carry
                    0b100 => self.fetchImmediateAndContinue(Cpu.doAnd),
                    0b101 => self.fetchImmediateAndContinue(Cpu.doXor),
                    0b110 => self.fetchImmediateAndContinue(Cpu.doOr),
                    0b111 => self.fetchImmediateAndContinue(Cpu.doCmp),
                };
            },
            0b111 => {
                return self.executeCallRestart();
            },
        }
    }

    fn decodePushPopRegister(self: *Cpu) void {
        const reg: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrRegStack(reg);
        self.next_op_1 = CpuOp1Union{ .ptr_reg_stack = reg_ptr };
    }

    fn decodeHighRamReferenceFromImmediate(self: *Cpu) MemoryReference {
        const addr = 0xFF00 | @as(u16, self.reg.WZ.Lo);
        return self.mmu.delayedReference(addr);
    }

    // EXECUTE //

    fn executePrefix(self: *Cpu) SelfRefCpuMethod {
        self.reg.IR = self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.executeOpcodePrefixed);
    }

    fn executeIllegalInstruction(self: *Cpu) SelfRefCpuMethod {
        self._illegalInstructionExecuted = true;
        logger.warn("Attempt to decode illegal instruction 0x{X:0>2} on address 0x{X:0>4}", .{ self.reg.IR, self.reg.PC });
        // close enough for now, this should stall the cpu in a state where it never services interrupts, etc
        return SelfRefCpuMethod.init(Cpu.decodeOpcode);
    }

    fn executeNop(self: *Cpu) SelfRefCpuMethod {
        return self.fetchOpcode();
    }

    fn executeHalt(self: *Cpu) SelfRefCpuMethod {
        // TODO
        return self.fetchOpcode();
    }

    fn executeStop(self: *Cpu) SelfRefCpuMethod {
        // TODO
        return self.fetchOpcode();
    }

    fn executeDisableInterrupt(self: *Cpu) SelfRefCpuMethod {
        self.enable_interrupt_next_instruction = false;
        self.enable_interrupt_current_instruction = false;
        self.reg.IME = 0;
        return self.fetchOpcode();
    }

    fn executeEnableInterrupt(self: *Cpu) SelfRefCpuMethod {
        self.enable_interrupt_next_instruction = true;
        return self.fetchOpcode();
    }

    fn executeRegisterToRegisterLoad8Bit(self: *Cpu) SelfRefCpuMethod {
        const to: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const from: u3 = @intCast(self.reg.IR & 0b00_000_111);

        const from_ptr = self.ptrReg8Bit(from);
        const to_ptr = self.ptrReg8Bit(to);

        self.next_op_1 = CpuOp1Union{ .to_8bit = to_ptr };
        self.next_op_2 = CpuOp2Union{ .from_8bit = from_ptr };

        if (from == 0b110) { // from indirect HL
            self.reg.WZ.Lo = from_ptr.read();
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
            return SelfRefCpuMethod.init(Cpu.load8BitAndFetch);
        }

        return self.load8BitAndFetch();
    }

    fn executeLoadImmediateToRegister8Bit(self: *Cpu) SelfRefCpuMethod {
        const to: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        self.next_op_1 = CpuOp1Union{ .to_8bit = self.ptrReg8Bit(to) };
        self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
        return self.load8BitAndFetch();
    }

    fn executeLoadImmediateIndirectFromAccumulator(self: *Cpu) SelfRefCpuMethod {
        return self.fetchImmediateHighAndContinue(Cpu.loadAddrOnTempRegisterFromAccumulator);
    }

    fn executeLoadAccumulatorImmediateIndirect(self: *Cpu) SelfRefCpuMethod {
        return self.fetchImmediateHighAndContinue(Cpu.loadMemoryToTempRegisterThenAccumulator);
    }

    fn executeLoadIndirectRegister16BitAccumulator(self: *Cpu) SelfRefCpuMethod {
        const toAccumulator = (self.reg.IR & 0b00_001_000) != 0;
        const isIndirectHL = (self.reg.IR & 0b00_100_000) != 0;

        var addr: u16 = undefined;
        if (isIndirectHL) {
            addr = self.reg.HL.all();
            const isDecrement = (self.reg.IR & 0b00_010_000) != 0;
            if (isDecrement) {
                self.reg.HL.dec();
            } else {
                self.reg.HL.inc();
            }
        } else {
            const reg: u2 = @intCast((self.reg.IR & 0b00_010_000) >> 4);
            const reg_ptr = self.ptrReg16Bit(reg);
            addr = reg_ptr.all();
        }

        const accumulatorReference = MemoryReference.fromPointer(&self.reg.AF.Hi);

        if (toAccumulator) {
            self.reg.WZ.Lo = self.mmu.read(addr);
            self.next_op_1 = CpuOp1Union{ .to_8bit = accumulatorReference };
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
            return SelfRefCpuMethod.init(Cpu.load8BitAndFetch);
        } else {
            self.next_op_1 = CpuOp1Union{ .to_8bit = self.mmu.delayedReference(addr) };
            self.next_op_2 = CpuOp2Union{ .from_8bit = accumulatorReference };
            return self.load8BitAndFetch();
        }
    }

    fn executeLoadIndirectRegisterCStackAccumulator(self: *Cpu) SelfRefCpuMethod {
        const toAccumulator = (self.reg.IR & 0b00_010_000) != 0;
        const addr = 0xFF00 | @as(u16, self.reg.BC.Lo);

        const accumulatorReference = MemoryReference.fromPointer(&self.reg.AF.Hi);

        if (toAccumulator) {
            self.reg.WZ.Lo = self.mmu.read(addr);
            self.next_op_1 = CpuOp1Union{ .to_8bit = accumulatorReference };
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
            return SelfRefCpuMethod.init(Cpu.load8BitAndFetch);
        } else {
            self.next_op_1 = CpuOp1Union{ .to_8bit = self.mmu.delayedReference(addr) };
            self.next_op_2 = CpuOp2Union{ .from_8bit = accumulatorReference };
            return self.load8BitAndFetch();
        }
    }

    fn executeLoadAccumulatorImmediateIndirectFromHighRam(self: *Cpu) SelfRefCpuMethod {
        self.reg.WZ.Lo = self.decodeHighRamReferenceFromImmediate().read();
        return SelfRefCpuMethod.init(Cpu.loadTempRegisterToAccumulator);
    }

    fn executeLoadFromAccumulatorImmediateIndirectToHighRam(self: *Cpu) SelfRefCpuMethod {
        self.decodeHighRamReferenceFromImmediate().write(self.reg.AF.Hi);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn executeLoad16BitRegisterFetchHighByte(self: *Cpu) SelfRefCpuMethod {
        const reg: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg);
        self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = reg_ptr };
        return self.fetchImmediateHighAndContinue(Cpu.loadTempRegisterTo16BitRegister);
    }

    fn executeLoadStackPointerToImmediateIndirect(self: *Cpu) SelfRefCpuMethod {
        return self.fetchImmediateHighAndContinue(Cpu.loadStackPointerToTempRegisterIndirect);
    }

    fn executeLoadSPRegisterFromHLRegister(self: *Cpu) SelfRefCpuMethod {
        self.reg.SP.setAll(self.reg.HL.all());
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn executePush(self: *Cpu) SelfRefCpuMethod {
        self.decodePushPopRegister();
        return self.decrementSPAndContinue(Cpu.pushHighByte);
    }

    fn executePop(self: *Cpu) SelfRefCpuMethod {
        self.decodePushPopRegister();
        self.reg.WZ.Lo = self.mmu.read(self.reg.SP.all());
        return self.incrementSPAndContinue(Cpu.popHighByte);
    }

    fn executeLoadHLFromAdjustedStackPointer(self: *Cpu) SelfRefCpuMethod {
        self.reg.HL.Lo = self.reg.AF.add_return(self.reg.SP.Lo, self.reg.WZ.Lo, 0, 0);
        return SelfRefCpuMethod.init(Cpu.loadHLFromAdjustedStackPointerHighByte);
    }

    fn executeIncDecRegister8Bit(self: *Cpu) SelfRefCpuMethod {
        const reg: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const reg_ptr = self.ptrReg8Bit(reg);
        self.next_op_1 = CpuOp1Union{ .to_8bit = reg_ptr };

        if (reg == 0b110) { // indirect HL
            self.reg.WZ.Lo = reg_ptr.read();
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
            return SelfRefCpuMethod.init(Cpu.doIncDecRegister);
        }

        self.next_op_2 = CpuOp2Union{ .from_8bit = reg_ptr };
        return self.doIncDecRegister();
    }

    fn executeComplementCarryFlag(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.ccf();
        return self.fetchOpcode();
    }

    fn executeSetCarryFlag(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.scf();
        return self.fetchOpcode();
    }

    fn executeDecimalAdjustAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.daa();
        return self.fetchOpcode();
    }

    fn executeComplementAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.cpl();
        return self.fetchOpcode();
    }

    fn executeIncDecRegister16Bit(self: *Cpu) SelfRefCpuMethod {
        const isDecrement = (self.reg.IR & 0b00_001_000) != 0;
        const reg_code: u2 = @intCast((self.reg.IR & 0b00_110_000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg_code);
        if (isDecrement) {
            reg_ptr.dec();
        } else {
            reg_ptr.inc();
        }
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn executeAddRegister16BitToRegisterHL(self: *Cpu) SelfRefCpuMethod {
        const reg_code: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
        const reg_ptr = self.ptrReg16Bit(reg_code);
        self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = reg_ptr };

        self.reg.HL.Lo = self.reg.AF.add_return(self.reg.HL.Lo, reg_ptr.Lo, 0, self.reg.AF.Lo.Z);
        return SelfRefCpuMethod.init(Cpu.executeAddRegister16BitToRegisterHLHighByte);
    }

    fn executeAddRegister16BitToRegisterHLHighByte(self: *Cpu) SelfRefCpuMethod {
        const reg_ptr = self.next_op_1.ptr_reg_16bit;
        self.reg.HL.Hi = self.reg.AF.add_return(self.reg.HL.Hi, reg_ptr.Hi, 1, self.reg.AF.Lo.Z);
        return self.fetchOpcode();
    }

    fn executeRotateLeftThroughCarryAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.AF.rlc(self.reg.AF.Hi);
        self.reg.AF.Lo.Z = 0;
        return self.fetchOpcode();
    }

    fn executeRotateRightThroughCarryAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.AF.rrc(self.reg.AF.Hi);
        self.reg.AF.Lo.Z = 0;
        return self.fetchOpcode();
    }

    fn executeRotateLeftAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.AF.rl(self.reg.AF.Hi);
        self.reg.AF.Lo.Z = 0;
        return self.fetchOpcode();
    }

    fn executeRotateRightAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.Hi = self.reg.AF.rr(self.reg.AF.Hi);
        self.reg.AF.Lo.Z = 0;
        return self.fetchOpcode();
    }

    fn executeAddToStackPointerRelative(self: *Cpu) SelfRefCpuMethod {
        self.next_op_1 = CpuOp1Union{ .auxIntermediate = self.reg.WZ.Lo };
        self.reg.WZ.Lo = self.reg.AF.add_return(self.reg.SP.Lo, self.next_op_1.auxIntermediate, 0, 0);
        return SelfRefCpuMethod.init(Cpu.addToStackPointerRelativeHighByte);
    }

    fn executeJumpToHL(self: *Cpu) SelfRefCpuMethod {
        self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = &self.reg.HL };
        return self.jumpToRegister();
    }

    fn executeJumpToRelative(self: *Cpu) SelfRefCpuMethod {
        const res = alu.AluRegister.add_interpret_signed_no_flags(self.reg.PC, self.reg.WZ.Lo);
        self.reg.WZ.setAll(res);
        return SelfRefCpuMethod.init(Cpu.loadTempRegisterToProgramCounterAndFetch);
    }

    fn executeJumpToImmediate(self: *Cpu) SelfRefCpuMethod {
        return self.fetchImmediateHighAndContinue(Cpu.loadTempRegisterToProgramCounterAndFetchLater);
    }

    fn executeJumpRelativeConditional(self: *Cpu) SelfRefCpuMethod {
        const cond: u2 = @intCast((self.reg.IR & 0b00_11_000) >> 3);
        if (self.reg.AF.cond(cond)) {
            return self.fetchImmediateAndContinue(Cpu.jumpRelativeConditionalDoRelative);
        } else {
            return self.fetchImmediateAndContinue(Cpu.fetchOpcode);
        }
    }

    fn executeJumpToImmediateConditional(self: *Cpu) SelfRefCpuMethod {
        self.reg.WZ.Hi = self.fetchPC();
        const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
        if (self.reg.AF.cond(cond)) {
            return SelfRefCpuMethod.init(Cpu.loadTempRegisterToProgramCounterAndFetchLater);
        } else {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }
    }

    fn executeCall(self: *Cpu) SelfRefCpuMethod {
        return self.fetchImmediateHighAndContinue(Cpu.pushProgramCounterAndJump);
    }

    fn executeCallConditional(self: *Cpu) SelfRefCpuMethod {
        self.reg.WZ.Hi = self.fetchPC();
        const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
        if (self.reg.AF.cond(cond)) {
            return SelfRefCpuMethod.init(Cpu.pushProgramCounterAndJump);
        } else {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }
    }

    fn executeReturn(self: *Cpu) SelfRefCpuMethod {
        self.next_op_2 = CpuOp2Union{ .jump_sets_ime = false };
        return self.returnFirstByte();
    }

    fn executeReturnConditional(self: *Cpu) SelfRefCpuMethod {
        const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
        if (self.reg.AF.cond(cond)) {
            self.next_op_2 = CpuOp2Union{ .jump_sets_ime = false };
            return SelfRefCpuMethod.init(Cpu.returnFirstByte);
        } else {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        }
    }

    fn executeReturnFromInterrupt(self: *Cpu) SelfRefCpuMethod {
        self.next_op_2 = CpuOp2Union{ .jump_sets_ime = true };
        return self.returnFirstByte();
    }

    fn executeCallRestart(self: *Cpu) SelfRefCpuMethod {
        return self.decrementSPAndContinue(Cpu.restartStoreFirstByte);
    }

    fn executeOpcodePrefixed(self: *Cpu) SelfRefCpuMethod {
        const op1: u2 = @intCast((self.reg.IR & 0b11_000_000) >> 6);
        const op2: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
        const regIdx: u3 = @intCast(self.reg.IR & 0b00_000_111);
        const ref = self.ptrReg8Bit(regIdx);
        self.next_op_3 = CpuOp3Union{ .extendedAluOpTarget = ref };

        if (ref == .mmuRef) {
            self.reg.WZ.Lo = ref.read();
        }

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
                if (ref == .mmuRef) {
                    return SelfRefCpuMethod.init(Cpu.extendedAluOpOnMemory);
                }
                ref.write(self.next_op_1.next_alu_op(&self.reg.AF, ref.read()));
                return self.fetchOpcode();
            },
            0b01 => { // bit test
                self.next_op_1 = CpuOp1Union{ .next_alu_op_bit_test = alu.AluRegister.bit };
                if (ref == .mmuRef) {
                    self.next_op_2 = CpuOp2Union{ .bitIdx = op2 };
                    return SelfRefCpuMethod.init(Cpu.extendedAluOpOnMemoryBitsTest);
                }
                self.next_op_1.next_alu_op_bit_test(&self.reg.AF, ref.read(), op2);
                return self.fetchOpcode();
            },
            else => { // bit set/reset
                self.next_op_1 = CpuOp1Union{ .next_alu_op_bit = switch (op1) {
                    0b10 => alu.AluRegister.res,
                    0b11 => alu.AluRegister.set,
                    else => unreachable,
                } };
                if (ref == .mmuRef) {
                    self.next_op_2 = CpuOp2Union{ .bitIdx = op2 };
                    return SelfRefCpuMethod.init(Cpu.extendedAluOpOnMemoryBits);
                }
                ref.write(self.next_op_1.next_alu_op_bit(ref.read(), op2));
                return self.fetchOpcode();
            },
        }
    }

    // HELPERS //

    fn fetchImmediateAndContinue(self: *Cpu, handler: fn (*Cpu) SelfRefCpuMethod) SelfRefCpuMethod {
        self.reg.WZ.Lo = self.fetchPC();
        return SelfRefCpuMethod.init(handler);
    }

    fn fetchImmediateHighAndContinue(self: *Cpu, handler: fn (*Cpu) SelfRefCpuMethod) SelfRefCpuMethod {
        self.reg.WZ.Hi = self.fetchPC();
        return SelfRefCpuMethod.init(handler);
    }

    fn incrementSPAndContinue(self: *Cpu, handler: fn (*Cpu) SelfRefCpuMethod) SelfRefCpuMethod {
        self.reg.SP.inc();
        return SelfRefCpuMethod.init(handler);
    }

    fn decrementSPAndContinue(self: *Cpu, handler: fn (*Cpu) SelfRefCpuMethod) SelfRefCpuMethod {
        self.reg.SP.dec();
        return SelfRefCpuMethod.init(handler);
    }

    fn fetchPC(self: *Cpu) u8 {
        const val = self.mmu.read(self.reg.PC);
        self.reg.PC += 1;
        return val;
    }

    fn fetchOpcode(self: *Cpu) SelfRefCpuMethod {
        self.reg.IR = self.fetchPC();
        return SelfRefCpuMethod.init(Cpu.decodeOpcode);
    }

    fn load8BitAndFetch(self: *Cpu) SelfRefCpuMethod {
        self.next_op_1.to_8bit.write(self.next_op_2.from_8bit.read());

        if (self.next_op_1.to_8bit == .mmuRef or self.next_op_2.from_8bit == .mmuRef) {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        } else {
            return self.fetchOpcode();
        }
    }

    fn loadMemoryToTempRegisterThenAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.reg.WZ.Lo = self.mmu.read(self.reg.WZ.all());
        self.next_op_1 = CpuOp1Union{ .to_8bit = MemoryReference.fromPointer(&self.reg.AF.Hi) };
        self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
        return SelfRefCpuMethod.init(Cpu.load8BitAndFetch);
    }

    fn loadAddrOnTempRegisterFromAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.next_op_1 = CpuOp1Union{ .to_8bit = self.mmu.delayedReference(self.reg.WZ.all()) };
        self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.AF.Hi) };
        return self.load8BitAndFetch();
    }

    fn loadTempRegisterToAccumulator(self: *Cpu) SelfRefCpuMethod {
        self.next_op_1 = CpuOp1Union{ .to_8bit = MemoryReference.fromPointer(&self.reg.AF.Hi) };
        self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
        return self.load8BitAndFetch();
    }

    fn loadTempRegisterTo16BitRegister(self: *Cpu) SelfRefCpuMethod {
        self.next_op_1.ptr_reg_16bit.setAll(self.reg.WZ.all());
        return self.fetchOpcode();
    }

    fn loadTempRegisterToProgramCounterAndFetch(self: *Cpu) SelfRefCpuMethod {
        self.reg.PC = self.reg.WZ.all();
        return self.fetchOpcode();
    }

    fn loadTempRegisterToProgramCounterAndFetchLater(self: *Cpu) SelfRefCpuMethod {
        self.reg.PC = self.reg.WZ.all();
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn loadStackPointerToTempRegisterIndirect(self: *Cpu) SelfRefCpuMethod {
        self.mmu.write(self.reg.WZ.all(), self.reg.SP.Lo);
        self.reg.WZ.inc();
        self.next_op_1 = CpuOp1Union{ .to_8bit = self.mmu.delayedReference(self.reg.WZ.all()) };
        self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.SP.Hi) };
        return SelfRefCpuMethod.init(Cpu.load8BitAndFetch);
    }

    fn pushHighByte(self: *Cpu) SelfRefCpuMethod {
        self.mmu.write(self.reg.SP.all(), self.next_op_1.ptr_reg_stack.hi());
        return self.decrementSPAndContinue(Cpu.pushLowByte);
    }

    fn pushLowByte(self: *Cpu) SelfRefCpuMethod {
        self.mmu.write(self.reg.SP.all(), self.next_op_1.ptr_reg_stack.lo());
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn popHighByte(self: *Cpu) SelfRefCpuMethod {
        self.reg.WZ.Hi = self.mmu.read(self.reg.SP.all());
        return self.incrementSPAndContinue(Cpu.popCopyToRegister);
    }

    fn popCopyToRegister(self: *Cpu) SelfRefCpuMethod {
        self.next_op_1.ptr_reg_stack.setAll(self.reg.WZ.all());
        return self.fetchOpcode();
    }

    fn loadHLFromAdjustedStackPointerHighByte(self: *Cpu) SelfRefCpuMethod {
        self.reg.HL.Hi = self.reg.AF.add_adj(self.reg.SP.Hi, self.reg.WZ.Lo);
        return self.fetchOpcode();
    }

    fn runAluOperationOnRegister8Bit(self: *Cpu, next: fn (self: *Cpu) SelfRefCpuMethod) SelfRefCpuMethod {
        const reg: u3 = @intCast(self.reg.IR & 0b0000_0_111);
        const reg_ptr = self.ptrReg8Bit(reg);
        self.reg.WZ.Lo = reg_ptr.read();

        if (reg_ptr == .mmuRef) {
            return SelfRefCpuMethod.init(next);
        } else {
            return next(self);
        }
    }

    fn doAdd(self: *Cpu) SelfRefCpuMethod {
        const with_carry: u1 = @intCast((self.reg.IR & 0b0000_1_000) >> 3);
        self.reg.AF.add(self.reg.WZ.Lo, with_carry);
        return self.fetchOpcode();
    }

    fn doSub(self: *Cpu) SelfRefCpuMethod {
        const with_carry: u1 = @intCast((self.reg.IR & 0b0000_1_000) >> 3);
        self.reg.AF.sub(self.reg.WZ.Lo, with_carry);
        return self.fetchOpcode();
    }

    fn doCmp(self: *Cpu) SelfRefCpuMethod {
        const a = self.reg.AF.Hi;
        self.reg.AF.sub(self.reg.WZ.Lo, 0);
        self.reg.AF.Hi = a;
        return self.fetchOpcode();
    }

    fn doAnd(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.and_(self.reg.WZ.Lo);
        return self.fetchOpcode();
    }

    fn doOr(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.or_(self.reg.WZ.Lo);
        return self.fetchOpcode();
    }

    fn doXor(self: *Cpu) SelfRefCpuMethod {
        self.reg.AF.xor(self.reg.WZ.Lo);
        return self.fetchOpcode();
    }

    fn doIncDecRegister(self: *Cpu) SelfRefCpuMethod {
        var val = self.next_op_2.from_8bit.read();
        if (self.reg.IR & 0b00_000_001 == 0) {
            val = self.reg.AF.inc(val);
        } else {
            val = self.reg.AF.dec(val);
        }
        self.next_op_1.to_8bit.write(val);

        if (self.next_op_1.to_8bit == .mmuRef) {
            return SelfRefCpuMethod.init(Cpu.fetchOpcode);
        } else {
            return self.fetchOpcode();
        }
    }

    fn addToStackPointerRelativeHighByte(self: *Cpu) SelfRefCpuMethod {
        self.reg.WZ.Hi = self.reg.AF.add_adj(self.reg.SP.Hi, self.next_op_1.auxIntermediate);
        self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = &self.reg.SP };
        return SelfRefCpuMethod.init(Cpu.loadTempRegisterTo16BitRegister);
    }

    fn jumpRelativeConditionalDoRelative(self: *Cpu) SelfRefCpuMethod {
        const res = alu.AluRegister.add_interpret_signed_no_flags(self.reg.PC, self.reg.WZ.Lo);
        self.reg.WZ.setAll(res);
        self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = &self.reg.WZ };
        return SelfRefCpuMethod.init(Cpu.jumpToRegister);
    }

    fn jumpToRegister(self: *Cpu) SelfRefCpuMethod {
        self.reg.PC = self.next_op_1.ptr_reg_16bit.all();
        return self.fetchOpcode();
    }

    fn jumpToRegisterLater(self: *Cpu) SelfRefCpuMethod {
        self.reg.PC = self.next_op_1.ptr_reg_16bit.all();
        if (self.next_op_2.jump_sets_ime) {
            self.reg.IME = 1;
        }
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn returnFirstByte(self: *Cpu) SelfRefCpuMethod {
        self.reg.WZ.Lo = self.mmu.read(self.reg.SP.all());
        return self.incrementSPAndContinue(Cpu.returnSecondByte);
    }

    fn returnSecondByte(self: *Cpu) SelfRefCpuMethod {
        self.reg.WZ.Hi = self.mmu.read(self.reg.SP.all());
        self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = &self.reg.WZ };
        return self.incrementSPAndContinue(Cpu.jumpToRegisterLater);
    }

    fn restartStoreFirstByte(self: *Cpu) SelfRefCpuMethod {
        self.mmu.write(self.reg.SP.all(), @intCast((self.reg.PC & 0xFF00) >> 8));
        return self.decrementSPAndContinue(Cpu.restartStoreSecondByteAndJump);
    }

    fn restartStoreSecondByteAndJump(self: *Cpu) SelfRefCpuMethod {
        self.mmu.write(self.reg.SP.all(), @intCast(self.reg.PC & 0x00FF));
        self.reg.PC = (self.reg.IR & 0b00_111_000) >> 3;
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn pushProgramCounterAndJump(self: *Cpu) SelfRefCpuMethod {
        return self.decrementSPAndContinue(Cpu.pushProgramCounterHighByteAndJump);
    }

    fn pushProgramCounterHighByteAndJump(self: *Cpu) SelfRefCpuMethod {
        self.mmu.write(self.reg.SP.all(), @intCast((self.reg.PC & 0xFF00) >> 8));
        return self.decrementSPAndContinue(Cpu.pushProgramCounterLowByteAndJump);
    }

    fn pushProgramCounterLowByteAndJump(self: *Cpu) SelfRefCpuMethod {
        self.mmu.write(self.reg.SP.all(), @intCast(self.reg.PC & 0x00FF));
        return self.loadTempRegisterToProgramCounterAndFetchLater();
    }

    fn extendedAluOpOnMemory(self: *Cpu) SelfRefCpuMethod {
        const res = self.next_op_1.next_alu_op(&self.reg.AF, self.reg.WZ.Lo);
        self.next_op_3.extendedAluOpTarget.write(res);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    fn extendedAluOpOnMemoryBitsTest(self: *Cpu) SelfRefCpuMethod {
        self.next_op_1.next_alu_op_bit_test(&self.reg.AF, self.reg.WZ.Lo, self.next_op_2.bitIdx);
        return self.fetchOpcode();
    }

    fn extendedAluOpOnMemoryBits(self: *Cpu) SelfRefCpuMethod {
        const res = self.next_op_1.next_alu_op_bit(self.reg.WZ.Lo, self.next_op_2.bitIdx);
        self.next_op_3.extendedAluOpTarget.write(res);
        return SelfRefCpuMethod.init(Cpu.fetchOpcode);
    }

    // REFERENCES //

    fn ptrReg8Bit(self: *Cpu, idx: u3) MemoryReference {
        return switch (idx) {
            0b000 => MemoryReference.fromPointer(&self.reg.BC.Hi),
            0b001 => MemoryReference.fromPointer(&self.reg.BC.Lo),
            0b010 => MemoryReference.fromPointer(&self.reg.DE.Hi),
            0b011 => MemoryReference.fromPointer(&self.reg.DE.Lo),
            0b100 => MemoryReference.fromPointer(&self.reg.HL.Hi),
            0b101 => MemoryReference.fromPointer(&self.reg.HL.Lo),
            0b110 => self.mmu.delayedReference(self.reg.HL.all()),
            0b111 => MemoryReference.fromPointer(&self.reg.AF.Hi),
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

    fn ptrRegStack(self: *Cpu, idx: u2) StackRegister {
        return switch (idx) {
            0b00 => StackRegister{ .RegisterWithHalves = &self.reg.BC },
            0b01 => StackRegister{ .RegisterWithHalves = &self.reg.DE },
            0b10 => StackRegister{ .RegisterWithHalves = &self.reg.HL },
            0b11 => StackRegister{ .AluRegister = &self.reg.AF },
        };
    }
};
