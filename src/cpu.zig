const std = @import("std");

const alu = @import("alu.zig");
const AluRegister = alu.AluRegister;
const RegisterWithHalves = alu.RegisterWithHalves;
const RegisterFlags = alu.RegisterFlags;
const InterruptKind = @import("interruptKind.zig").InterruptKind;

const MemoryReferenceFn = @import("reference.zig").MemoryReference;

const logger = std.log.scoped(.cpu);

// The reason for this union is that we implement BC, DE, HL as RegisterWithHalves but AF as AluRegister so this union
// allows us to use them indistinctively.
const StackRegister = union(enum) {
    AluRegister: *AluRegister,
    RegisterWithHalves: *RegisterWithHalves,

    fn all(reg: StackRegister) u16 {
        return switch (reg) {
            .AluRegister => |r| r.all(),
            .RegisterWithHalves => |r| r.all(),
        };
    }

    fn setAll(reg: StackRegister, val: u16) void {
        return switch (reg) {
            .AluRegister => |r| r.setAll(val),
            .RegisterWithHalves => |r| r.setAll(val),
        };
    }

    fn hi(reg: StackRegister) u8 {
        return switch (reg) {
            .AluRegister => |r| r.Hi,
            .RegisterWithHalves => |r| r.Hi,
        };
    }

    fn lo(reg: StackRegister) u8 {
        return switch (reg) {
            .AluRegister => |r| r.Lo.all(),
            .RegisterWithHalves => |r| r.Lo,
        };
    }
};

test "StackRegister: all and setAll for AluRegister" {
    var alu_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .Z = 0, .N = 0, .H = 0, .C = 0, .rest = 0 },
    };

    StackRegister.setAll(StackRegister{ .AluRegister = &alu_reg }, 0xABCD);
    const result = StackRegister.all(StackRegister{ .AluRegister = &alu_reg });
    try std.testing.expectEqual(@as(u16, 0xABCD), result);
}

test "StackRegister: all and setAll for RegisterWithHalves" {
    var reg_wh = RegisterWithHalves{
        .Hi = 0,
        .Lo = 0,
    };

    StackRegister.setAll(StackRegister{ .RegisterWithHalves = &reg_wh }, 0x1234);
    const result = StackRegister.all(StackRegister{ .RegisterWithHalves = &reg_wh });
    try std.testing.expectEqual(@as(u16, 0x1234), result);
}

test "StackRegister: hi and lo for AluRegister" {
    var alu_reg = AluRegister{
        .Hi = 0xA1,
        .Lo = RegisterFlags{ .Z = 0, .N = 0, .H = 0, .C = 0, .rest = 0xB2 >> 4 },
    };
    // Assume .Lo.all() gives full byte from bitfields — if not, adjust accordingly.
    StackRegister.setAll(StackRegister{ .AluRegister = &alu_reg }, 0xA1B2);
    const stack_reg = StackRegister{ .AluRegister = &alu_reg };
    try std.testing.expectEqual(@as(u8, 0xA1), StackRegister.hi(stack_reg));
    try std.testing.expectEqual(@as(u8, 0xB2), StackRegister.lo(stack_reg));
}

test "StackRegister: hi and lo for RegisterWithHalves" {
    var reg_wh = RegisterWithHalves{
        .Hi = 0xC3,
        .Lo = 0xD4,
    };
    const stack_reg = StackRegister{ .RegisterWithHalves = &reg_wh };
    try std.testing.expectEqual(@as(u8, 0xC3), StackRegister.hi(stack_reg));
    try std.testing.expectEqual(@as(u8, 0xD4), StackRegister.lo(stack_reg));
}

const RegisterBank = struct {
    AF: AluRegister,
    BC: RegisterWithHalves,
    DE: RegisterWithHalves,

    HL: RegisterWithHalves,

    SP: RegisterWithHalves,
    PC: u16,

    IR: u8,
    WZ: RegisterWithHalves,

    IME: u1,
};

pub const CpuFlag = packed struct {
    illegal: bool = false,
    breakpoint: bool = false,
};

pub fn Cpu(Mmu: type, Interrupt: type) type {
    const interruptVectors = [_]u16{ 0x40, 0x48, 0x50, 0x58, 0x60 };

    const MemoryReference = MemoryReferenceFn(Mmu);

    const CpuOp1Union = union {
        auxIntermediate: u8,
        next_alu_op: *const fn (*AluRegister, u8) u8,
        next_alu_op_bit: *const fn (u8, u3) u8,
        next_alu_op_bit_test: *const fn (*AluRegister, u8, u3) void,
        to_8bit: MemoryReference,
        ptr_reg_stack: StackRegister,
        ptr_reg_16bit: *RegisterWithHalves,
        pending_interrupt: InterruptKind,
    };

    const CpuOp2Union = union {
        bitIdx: u3,
        from_8bit: MemoryReference,
        jump_sets_ime: bool,
    };

    const CpuOp3Union = union {
        extendedAluOpTarget: MemoryReference,
    };

    return struct {
        const This = @This();

        const SelfRefCpuMethod = struct {
            func: *const fn (*This) SelfRefCpuMethod,

            fn init(func: *const fn (*This) SelfRefCpuMethod) SelfRefCpuMethod {
                return SelfRefCpuMethod{
                    .func = func,
                };
            }
        };

        mmu: *Mmu,
        intr: *Interrupt,
        reg: RegisterBank,

        next_tick: SelfRefCpuMethod,
        next_op_1: CpuOp1Union,
        next_op_2: CpuOp2Union,
        next_op_3: CpuOp3Union,

        enable_interrupt_next_instruction: bool,
        enable_interrupt_current_instruction: bool,

        breakpoint_instruction: ?u8,

        flags: CpuFlag,

        pub inline fn init(mmu: *Mmu, intr: *Interrupt, breakpoint_instruction: ?u8) This {
            return This{
                .mmu = mmu,
                .intr = intr,
                .reg = undefined,
                .next_tick = SelfRefCpuMethod.init(This.fetchOpcode),
                .next_op_1 = undefined,
                .next_op_2 = undefined,
                .next_op_3 = undefined,
                .enable_interrupt_next_instruction = false,
                .enable_interrupt_current_instruction = false,
                .breakpoint_instruction = breakpoint_instruction,
                .flags = .{},
            };
        }

        pub inline fn getFlags(self: *const This) CpuFlag {
            return self.flags;
        }

        pub inline fn setFlags(self: *This, flags: CpuFlag) void {
            self.flags = flags;
        }

        pub inline fn resetFlags(self: *This) void {
            self.flags = .{};
        }

        pub inline fn isInstructionBoundary(self: *const This) bool {
            return self.next_tick.func == This.decodeOpcode;
        }

        pub fn tick(self: *This) void {
            if (self.isInstructionBoundary()) {
                self.onInstructionStart();
            }
            self.next_tick = self.next_tick.func(self);
            if (self.isInstructionBoundary()) {
                self.onInstructionEnd();
            }
        }

        fn onInstructionStart(self: *This) void {
            if (self.enable_interrupt_next_instruction) {
                self.enable_interrupt_next_instruction = false;
                self.enable_interrupt_current_instruction = true;
            }

            if (self.reg.IME == 1 and self.intr.pending() != null) {
                // overwrite next tick to start servicing the interrupt instead
                self.next_tick = SelfRefCpuMethod.init(This.executeInterrupt);
            }
        }

        fn onInstructionEnd(self: *This) void {
            if (self.enable_interrupt_current_instruction) {
                self.reg.IME = 1;
                self.enable_interrupt_current_instruction = false;
            }
        }

        // DECODE //

        fn decodeOpcode(self: *This) SelfRefCpuMethod {
            self.flags.illegal = false;
            if (self.breakpoint_instruction != null and self.reg.IR == self.breakpoint_instruction) {
                self.flags.breakpoint = true;
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

        fn decodeBlock0(self: *This) SelfRefCpuMethod {
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
                            return self.fetchImmediateAndContinue(This.executeLoadStackPointerToImmediateIndirect);
                        },
                        0b10 => {
                            return self.executeStop();
                        },
                        0b11 => {
                            return self.fetchImmediateAndContinue(This.executeJumpToRelative);
                        },
                    }
                },
                0b001 => {
                    const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);

                    if (bit3 == 1) {
                        return self.executeAddRegister16BitToRegisterHL();
                    } else {
                        return self.fetchImmediateAndContinue(This.executeLoad16BitRegisterFetchHighByte);
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
                    return self.fetchImmediateAndContinue(This.executeLoadImmediateToRegister8Bit);
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

        fn decodeBlock1(self: *This) SelfRefCpuMethod {
            if (self.reg.IR == 0b01_110_110) {
                return self.executeHalt();
            }
            return self.executeRegisterToRegisterLoad8Bit();
        }

        fn decodeBlock2(self: *This) SelfRefCpuMethod {
            const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
            return switch (infix) {
                0b000 => self.runAluOperationOnRegister8Bit(This.doAdd),
                0b001 => self.runAluOperationOnRegister8Bit(This.doAdd), // with carry
                0b010 => self.runAluOperationOnRegister8Bit(This.doSub),
                0b011 => self.runAluOperationOnRegister8Bit(This.doSub), // with carry
                0b100 => self.runAluOperationOnRegister8Bit(This.doAnd),
                0b101 => self.runAluOperationOnRegister8Bit(This.doXor),
                0b110 => self.runAluOperationOnRegister8Bit(This.doOr),
                0b111 => self.runAluOperationOnRegister8Bit(This.doCmp),
            };
        }

        fn decodeBlock3(self: *This) SelfRefCpuMethod {
            const suffix: u3 = @intCast(self.reg.IR & 0b00_000_111);
            switch (suffix) {
                0b000 => {
                    const bit5: u1 = @intCast((self.reg.IR & 0b00_100_000) >> 5);
                    if (bit5 == 0) {
                        return self.executeReturnConditional();
                    }

                    const bit43: u2 = @intCast((self.reg.IR & 0b00_011_000) >> 3);
                    return switch (bit43) {
                        0b00 => self.fetchImmediateAndContinue(This.executeLoadFromAccumulatorImmediateIndirectToHighRam),
                        0b01 => self.fetchImmediateAndContinue(This.executeAddToStackPointerRelative),
                        0b10 => self.fetchImmediateAndContinue(This.executeLoadAccumulatorImmediateIndirectFromHighRam),
                        0b11 => self.fetchImmediateAndContinue(This.executeLoadHLFromAdjustedStackPointer),
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
                        return self.fetchImmediateAndContinue(This.executeJumpToImmediateConditional);
                    }

                    const bit43: u2 = @intCast((self.reg.IR & 0b00_011_000) >> 3);
                    return switch (bit43) {
                        0b00 => self.executeLoadIndirectRegisterCStackAccumulator(),
                        0b10 => self.executeLoadIndirectRegisterCStackAccumulator(),
                        0b01 => self.fetchImmediateAndContinue(This.executeLoadImmediateIndirectFromAccumulator),
                        0b11 => self.fetchImmediateAndContinue(This.executeLoadAccumulatorImmediateIndirect),
                    };
                },
                0b011 => {
                    const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
                    return switch (infix) {
                        0b000 => self.fetchImmediateAndContinue(This.executeJumpToImmediate),
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
                    return self.fetchImmediateAndContinue(This.executeCallConditional);
                },
                0b101 => {
                    const bit3: u1 = @intCast((self.reg.IR & 0b00_001_000) >> 3);
                    if (bit3 == 0) {
                        return self.executePush();
                    }

                    if ((self.reg.IR & 0b00_110_000) != 0) {
                        return self.executeIllegalInstruction();
                    }

                    return self.fetchImmediateAndContinue(This.executeCall);
                },
                0b110 => {
                    const infix: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
                    return switch (infix) {
                        0b000 => self.fetchImmediateAndContinue(This.doAdd),
                        0b001 => self.fetchImmediateAndContinue(This.doAdd), // with carry
                        0b010 => self.fetchImmediateAndContinue(This.doSub),
                        0b011 => self.fetchImmediateAndContinue(This.doSub), // with carry
                        0b100 => self.fetchImmediateAndContinue(This.doAnd),
                        0b101 => self.fetchImmediateAndContinue(This.doXor),
                        0b110 => self.fetchImmediateAndContinue(This.doOr),
                        0b111 => self.fetchImmediateAndContinue(This.doCmp),
                    };
                },
                0b111 => {
                    return self.executeCallRestart();
                },
            }
        }

        fn decodePushPopRegister(self: *This) void {
            const reg: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
            const reg_ptr = self.ptrRegStack(reg);
            self.next_op_1 = CpuOp1Union{ .ptr_reg_stack = reg_ptr };
        }

        fn decodeHighRamReferenceFromImmediate(self: *This) MemoryReference {
            const addr = 0xFF00 | @as(u16, self.reg.WZ.Lo);
            return MemoryReference.fromMemory(self.mmu, addr);
        }

        // EXECUTE //

        fn executePrefix(self: *This) SelfRefCpuMethod {
            self.reg.IR = self.fetchPC();
            return SelfRefCpuMethod.init(This.executeOpcodePrefixed);
        }

        fn executeIllegalInstruction(self: *This) SelfRefCpuMethod {
            self.flags.illegal = true;
            logger.warn("Attempt to decode illegal instruction 0x{X:0>2} on address 0x{X:0>4}", .{ self.reg.IR, self.reg.PC });
            // close enough for now, this should stall the cpu in a state where it never services interrupts, etc
            return SelfRefCpuMethod.init(This.decodeOpcode);
        }

        fn executeNop(self: *This) SelfRefCpuMethod {
            return self.fetchOpcode();
        }

        fn executeHalt(self: *This) SelfRefCpuMethod {
            // TODO
            return self.fetchOpcode();
        }

        fn executeStop(self: *This) SelfRefCpuMethod {
            // TODO
            return self.fetchOpcode();
        }

        fn executeDisableInterrupt(self: *This) SelfRefCpuMethod {
            self.enable_interrupt_next_instruction = false;
            self.enable_interrupt_current_instruction = false;
            self.reg.IME = 0;
            return self.fetchOpcode();
        }

        fn executeEnableInterrupt(self: *This) SelfRefCpuMethod {
            self.enable_interrupt_next_instruction = true;
            return self.fetchOpcode();
        }

        fn executeRegisterToRegisterLoad8Bit(self: *This) SelfRefCpuMethod {
            const to: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
            const from: u3 = @intCast(self.reg.IR & 0b00_000_111);

            const from_ptr = self.ptrReg8Bit(from);
            const to_ptr = self.ptrReg8Bit(to);

            self.next_op_1 = CpuOp1Union{ .to_8bit = to_ptr };
            self.next_op_2 = CpuOp2Union{ .from_8bit = from_ptr };

            if (from == 0b110) { // from indirect HL
                self.reg.WZ.Lo = from_ptr.read();
                self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
                return SelfRefCpuMethod.init(This.load8BitAndFetch);
            }

            return self.load8BitAndFetch();
        }

        fn executeLoadImmediateToRegister8Bit(self: *This) SelfRefCpuMethod {
            const to: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
            self.next_op_1 = CpuOp1Union{ .to_8bit = self.ptrReg8Bit(to) };
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
            return self.load8BitAndFetch();
        }

        fn executeLoadImmediateIndirectFromAccumulator(self: *This) SelfRefCpuMethod {
            return self.fetchImmediateHighAndContinue(This.loadAddrOnTempRegisterFromAccumulator);
        }

        fn executeLoadAccumulatorImmediateIndirect(self: *This) SelfRefCpuMethod {
            return self.fetchImmediateHighAndContinue(This.loadMemoryToTempRegisterThenAccumulator);
        }

        fn executeLoadIndirectRegister16BitAccumulator(self: *This) SelfRefCpuMethod {
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
                return SelfRefCpuMethod.init(This.load8BitAndFetch);
            } else {
                self.next_op_1 = CpuOp1Union{ .to_8bit = MemoryReference.fromMemory(self.mmu, addr) };
                self.next_op_2 = CpuOp2Union{ .from_8bit = accumulatorReference };
                return self.load8BitAndFetch();
            }
        }

        fn executeLoadIndirectRegisterCStackAccumulator(self: *This) SelfRefCpuMethod {
            const toAccumulator = (self.reg.IR & 0b00_010_000) != 0;
            const addr = 0xFF00 | @as(u16, self.reg.BC.Lo);

            const accumulatorReference = MemoryReference.fromPointer(&self.reg.AF.Hi);

            if (toAccumulator) {
                self.reg.WZ.Lo = self.mmu.read(addr);
                self.next_op_1 = CpuOp1Union{ .to_8bit = accumulatorReference };
                self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
                return SelfRefCpuMethod.init(This.load8BitAndFetch);
            } else {
                self.next_op_1 = CpuOp1Union{ .to_8bit = MemoryReference.fromMemory(self.mmu, addr) };
                self.next_op_2 = CpuOp2Union{ .from_8bit = accumulatorReference };
                return self.load8BitAndFetch();
            }
        }

        fn executeLoadAccumulatorImmediateIndirectFromHighRam(self: *This) SelfRefCpuMethod {
            self.reg.WZ.Lo = self.decodeHighRamReferenceFromImmediate().read();
            return SelfRefCpuMethod.init(This.loadTempRegisterToAccumulator);
        }

        fn executeLoadFromAccumulatorImmediateIndirectToHighRam(self: *This) SelfRefCpuMethod {
            self.decodeHighRamReferenceFromImmediate().write(self.reg.AF.Hi);
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn executeLoad16BitRegisterFetchHighByte(self: *This) SelfRefCpuMethod {
            const reg: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
            const reg_ptr = self.ptrReg16Bit(reg);
            self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = reg_ptr };
            return self.fetchImmediateHighAndContinue(This.loadTempRegisterTo16BitRegister);
        }

        fn executeLoadStackPointerToImmediateIndirect(self: *This) SelfRefCpuMethod {
            return self.fetchImmediateHighAndContinue(This.loadStackPointerToTempRegisterIndirect);
        }

        fn executeLoadSPRegisterFromHLRegister(self: *This) SelfRefCpuMethod {
            self.reg.SP.setAll(self.reg.HL.all());
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn executePush(self: *This) SelfRefCpuMethod {
            self.decodePushPopRegister();
            return self.decrementSPAndContinue(This.pushHighByte);
        }

        fn executePop(self: *This) SelfRefCpuMethod {
            self.decodePushPopRegister();
            self.reg.WZ.Lo = self.mmu.read(self.reg.SP.all());
            return self.incrementSPAndContinue(This.popHighByte);
        }

        fn executeLoadHLFromAdjustedStackPointer(self: *This) SelfRefCpuMethod {
            self.reg.HL.Lo = self.reg.AF.add_return(self.reg.SP.Lo, self.reg.WZ.Lo, 0, 0);
            return SelfRefCpuMethod.init(This.loadHLFromAdjustedStackPointerHighByte);
        }

        fn executeIncDecRegister8Bit(self: *This) SelfRefCpuMethod {
            const reg: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
            const reg_ptr = self.ptrReg8Bit(reg);
            self.next_op_1 = CpuOp1Union{ .to_8bit = reg_ptr };

            if (reg == 0b110) { // indirect HL
                self.reg.WZ.Lo = reg_ptr.read();
                self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
                return SelfRefCpuMethod.init(This.doIncDecRegister);
            }

            self.next_op_2 = CpuOp2Union{ .from_8bit = reg_ptr };
            return self.doIncDecRegister();
        }

        fn executeComplementCarryFlag(self: *This) SelfRefCpuMethod {
            self.reg.AF.ccf();
            return self.fetchOpcode();
        }

        fn executeSetCarryFlag(self: *This) SelfRefCpuMethod {
            self.reg.AF.scf();
            return self.fetchOpcode();
        }

        fn executeDecimalAdjustAccumulator(self: *This) SelfRefCpuMethod {
            self.reg.AF.daa();
            return self.fetchOpcode();
        }

        fn executeComplementAccumulator(self: *This) SelfRefCpuMethod {
            self.reg.AF.cpl();
            return self.fetchOpcode();
        }

        fn executeIncDecRegister16Bit(self: *This) SelfRefCpuMethod {
            const isDecrement = (self.reg.IR & 0b00_001_000) != 0;
            const reg_code: u2 = @intCast((self.reg.IR & 0b00_110_000) >> 4);
            const reg_ptr = self.ptrReg16Bit(reg_code);
            if (isDecrement) {
                reg_ptr.dec();
            } else {
                reg_ptr.inc();
            }
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn executeAddRegister16BitToRegisterHL(self: *This) SelfRefCpuMethod {
            const reg_code: u2 = @intCast((self.reg.IR & 0b00_11_0000) >> 4);
            const reg_ptr = self.ptrReg16Bit(reg_code);
            self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = reg_ptr };

            self.reg.HL.Lo = self.reg.AF.add_return(self.reg.HL.Lo, reg_ptr.Lo, 0, self.reg.AF.Lo.Z);
            return SelfRefCpuMethod.init(This.executeAddRegister16BitToRegisterHLHighByte);
        }

        fn executeAddRegister16BitToRegisterHLHighByte(self: *This) SelfRefCpuMethod {
            const reg_ptr = self.next_op_1.ptr_reg_16bit;
            self.reg.HL.Hi = self.reg.AF.add_return(self.reg.HL.Hi, reg_ptr.Hi, 1, self.reg.AF.Lo.Z);
            return self.fetchOpcode();
        }

        fn executeRotateLeftThroughCarryAccumulator(self: *This) SelfRefCpuMethod {
            self.reg.AF.Hi = self.reg.AF.rlc(self.reg.AF.Hi);
            self.reg.AF.Lo.Z = 0;
            return self.fetchOpcode();
        }

        fn executeRotateRightThroughCarryAccumulator(self: *This) SelfRefCpuMethod {
            self.reg.AF.Hi = self.reg.AF.rrc(self.reg.AF.Hi);
            self.reg.AF.Lo.Z = 0;
            return self.fetchOpcode();
        }

        fn executeRotateLeftAccumulator(self: *This) SelfRefCpuMethod {
            self.reg.AF.Hi = self.reg.AF.rl(self.reg.AF.Hi);
            self.reg.AF.Lo.Z = 0;
            return self.fetchOpcode();
        }

        fn executeRotateRightAccumulator(self: *This) SelfRefCpuMethod {
            self.reg.AF.Hi = self.reg.AF.rr(self.reg.AF.Hi);
            self.reg.AF.Lo.Z = 0;
            return self.fetchOpcode();
        }

        fn executeAddToStackPointerRelative(self: *This) SelfRefCpuMethod {
            self.next_op_1 = CpuOp1Union{ .auxIntermediate = self.reg.WZ.Lo };
            self.reg.WZ.Lo = self.reg.AF.add_return(self.reg.SP.Lo, self.next_op_1.auxIntermediate, 0, 0);
            return SelfRefCpuMethod.init(This.addToStackPointerRelativeHighByte);
        }

        fn executeJumpToHL(self: *This) SelfRefCpuMethod {
            self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = &self.reg.HL };
            return self.jumpToRegister();
        }

        fn executeJumpToRelative(self: *This) SelfRefCpuMethod {
            const res = AluRegister.add_interpret_signed_no_flags(self.reg.PC, self.reg.WZ.Lo);
            self.reg.WZ.setAll(res);
            return SelfRefCpuMethod.init(This.loadTempRegisterToProgramCounterAndFetch);
        }

        fn executeJumpToImmediate(self: *This) SelfRefCpuMethod {
            return self.fetchImmediateHighAndContinue(This.loadTempRegisterToProgramCounterAndFetchLater);
        }

        fn executeJumpRelativeConditional(self: *This) SelfRefCpuMethod {
            const cond: u2 = @intCast((self.reg.IR & 0b00_11_000) >> 3);
            if (self.reg.AF.cond(cond)) {
                return self.fetchImmediateAndContinue(This.jumpRelativeConditionalDoRelative);
            } else {
                return self.fetchImmediateAndContinue(This.fetchOpcode);
            }
        }

        fn executeJumpToImmediateConditional(self: *This) SelfRefCpuMethod {
            self.reg.WZ.Hi = self.fetchPC();
            const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
            if (self.reg.AF.cond(cond)) {
                return SelfRefCpuMethod.init(This.loadTempRegisterToProgramCounterAndFetchLater);
            } else {
                return SelfRefCpuMethod.init(This.fetchOpcode);
            }
        }

        fn executeCall(self: *This) SelfRefCpuMethod {
            return self.fetchImmediateHighAndContinue(This.pushProgramCounterAndJump);
        }

        fn executeCallConditional(self: *This) SelfRefCpuMethod {
            self.reg.WZ.Hi = self.fetchPC();
            const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
            if (self.reg.AF.cond(cond)) {
                return SelfRefCpuMethod.init(This.pushProgramCounterAndJump);
            } else {
                return SelfRefCpuMethod.init(This.fetchOpcode);
            }
        }

        fn executeReturn(self: *This) SelfRefCpuMethod {
            self.next_op_2 = CpuOp2Union{ .jump_sets_ime = false };
            return self.returnFirstByte();
        }

        fn executeReturnConditional(self: *This) SelfRefCpuMethod {
            const cond: u2 = @intCast((self.reg.IR & 0b000_11_000) >> 3);
            if (self.reg.AF.cond(cond)) {
                self.next_op_2 = CpuOp2Union{ .jump_sets_ime = false };
                return SelfRefCpuMethod.init(This.returnFirstByte);
            } else {
                return SelfRefCpuMethod.init(This.fetchOpcode);
            }
        }

        fn executeReturnFromInterrupt(self: *This) SelfRefCpuMethod {
            self.next_op_2 = CpuOp2Union{ .jump_sets_ime = true };
            return self.returnFirstByte();
        }

        fn executeCallRestart(self: *This) SelfRefCpuMethod {
            return self.decrementSPAndContinue(This.restartStoreFirstByte);
        }

        fn executeOpcodePrefixed(self: *This) SelfRefCpuMethod {
            const op1: u2 = @intCast((self.reg.IR & 0b11_000_000) >> 6);
            const op2: u3 = @intCast((self.reg.IR & 0b00_111_000) >> 3);
            const regIdx: u3 = @intCast(self.reg.IR & 0b00_000_111);
            const ref = self.ptrReg8Bit(regIdx);
            self.next_op_3 = CpuOp3Union{ .extendedAluOpTarget = ref };

            if (ref == .memRef) {
                self.reg.WZ.Lo = ref.read();
            }

            switch (op1) {
                0b00 => { // shift/rotate/swap
                    self.next_op_1 = CpuOp1Union{ .next_alu_op = switch (op2) {
                        0b000 => AluRegister.rlc,
                        0b001 => AluRegister.rrc,
                        0b010 => AluRegister.rl,
                        0b011 => AluRegister.rr,
                        0b100 => AluRegister.sla,
                        0b101 => AluRegister.sra,
                        0b110 => AluRegister.swap,
                        0b111 => AluRegister.srl,
                    } };
                    if (ref == .memRef) {
                        return SelfRefCpuMethod.init(This.extendedAluOpOnMemory);
                    }
                    ref.write(self.next_op_1.next_alu_op(&self.reg.AF, ref.read()));
                    return self.fetchOpcode();
                },
                0b01 => { // bit test
                    self.next_op_1 = CpuOp1Union{ .next_alu_op_bit_test = AluRegister.bit };
                    if (ref == .memRef) {
                        self.next_op_2 = CpuOp2Union{ .bitIdx = op2 };
                        return SelfRefCpuMethod.init(This.extendedAluOpOnMemoryBitsTest);
                    }
                    self.next_op_1.next_alu_op_bit_test(&self.reg.AF, ref.read(), op2);
                    return self.fetchOpcode();
                },
                else => { // bit set/reset
                    self.next_op_1 = CpuOp1Union{ .next_alu_op_bit = switch (op1) {
                        0b10 => AluRegister.res,
                        0b11 => AluRegister.set,
                        else => unreachable,
                    } };
                    if (ref == .memRef) {
                        self.next_op_2 = CpuOp2Union{ .bitIdx = op2 };
                        return SelfRefCpuMethod.init(This.extendedAluOpOnMemoryBits);
                    }
                    ref.write(self.next_op_1.next_alu_op_bit(ref.read(), op2));
                    return self.fetchOpcode();
                },
            }
        }

        fn executeInterrupt(self: *This) SelfRefCpuMethod {
            // https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#isr-and-nmi
            return self.decrementPCAndContinue(This.decrementSPForIRQ);
        }

        // HELPERS //

        fn fetchImmediateAndContinue(self: *This, handler: fn (*This) SelfRefCpuMethod) SelfRefCpuMethod {
            self.reg.WZ.Lo = self.fetchPC();
            return SelfRefCpuMethod.init(handler);
        }

        fn fetchImmediateHighAndContinue(self: *This, handler: fn (*This) SelfRefCpuMethod) SelfRefCpuMethod {
            self.reg.WZ.Hi = self.fetchPC();
            return SelfRefCpuMethod.init(handler);
        }

        fn incrementSPAndContinue(self: *This, handler: fn (*This) SelfRefCpuMethod) SelfRefCpuMethod {
            self.reg.SP.inc();
            return SelfRefCpuMethod.init(handler);
        }

        fn decrementSPAndContinue(self: *This, handler: fn (*This) SelfRefCpuMethod) SelfRefCpuMethod {
            self.reg.SP.dec();
            return SelfRefCpuMethod.init(handler);
        }

        fn decrementPCAndContinue(self: *This, handler: fn (*This) SelfRefCpuMethod) SelfRefCpuMethod {
            self.reg.PC -%= 1;
            return SelfRefCpuMethod.init(handler);
        }

        fn fetchPC(self: *This) u8 {
            const val = self.mmu.read(self.reg.PC);
            self.reg.PC +%= 1;
            return val;
        }

        fn fetchOpcode(self: *This) SelfRefCpuMethod {
            self.reg.IR = self.fetchPC();
            return SelfRefCpuMethod.init(This.decodeOpcode);
        }

        fn load8BitAndFetch(self: *This) SelfRefCpuMethod {
            self.next_op_1.to_8bit.write(self.next_op_2.from_8bit.read());

            if (self.next_op_1.to_8bit == .memRef or self.next_op_2.from_8bit == .memRef) {
                return SelfRefCpuMethod.init(This.fetchOpcode);
            } else {
                return self.fetchOpcode();
            }
        }

        fn loadMemoryToTempRegisterThenAccumulator(self: *This) SelfRefCpuMethod {
            self.reg.WZ.Lo = self.mmu.read(self.reg.WZ.all());
            self.next_op_1 = CpuOp1Union{ .to_8bit = MemoryReference.fromPointer(&self.reg.AF.Hi) };
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
            return SelfRefCpuMethod.init(This.load8BitAndFetch);
        }

        fn loadAddrOnTempRegisterFromAccumulator(self: *This) SelfRefCpuMethod {
            self.next_op_1 = CpuOp1Union{ .to_8bit = MemoryReference.fromMemory(self.mmu, self.reg.WZ.all()) };
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.AF.Hi) };
            return self.load8BitAndFetch();
        }

        fn loadTempRegisterToAccumulator(self: *This) SelfRefCpuMethod {
            self.next_op_1 = CpuOp1Union{ .to_8bit = MemoryReference.fromPointer(&self.reg.AF.Hi) };
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.WZ.Lo) };
            return self.load8BitAndFetch();
        }

        fn loadTempRegisterTo16BitRegister(self: *This) SelfRefCpuMethod {
            self.next_op_1.ptr_reg_16bit.setAll(self.reg.WZ.all());
            return self.fetchOpcode();
        }

        fn loadTempRegisterToProgramCounterAndFetch(self: *This) SelfRefCpuMethod {
            self.reg.PC = self.reg.WZ.all();
            return self.fetchOpcode();
        }

        fn loadTempRegisterToProgramCounterAndFetchLater(self: *This) SelfRefCpuMethod {
            self.reg.PC = self.reg.WZ.all();
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn loadStackPointerToTempRegisterIndirect(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.WZ.all(), self.reg.SP.Lo);
            self.reg.WZ.inc();
            self.next_op_1 = CpuOp1Union{ .to_8bit = MemoryReference.fromMemory(self.mmu, self.reg.WZ.all()) };
            self.next_op_2 = CpuOp2Union{ .from_8bit = MemoryReference.fromPointer(&self.reg.SP.Hi) };
            return SelfRefCpuMethod.init(This.load8BitAndFetch);
        }

        fn pushHighByte(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.SP.all(), self.next_op_1.ptr_reg_stack.hi());
            return self.decrementSPAndContinue(This.pushLowByte);
        }

        fn pushLowByte(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.SP.all(), self.next_op_1.ptr_reg_stack.lo());
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn popHighByte(self: *This) SelfRefCpuMethod {
            self.reg.WZ.Hi = self.mmu.read(self.reg.SP.all());
            return self.incrementSPAndContinue(This.popCopyToRegister);
        }

        fn popCopyToRegister(self: *This) SelfRefCpuMethod {
            self.next_op_1.ptr_reg_stack.setAll(self.reg.WZ.all());
            return self.fetchOpcode();
        }

        fn loadHLFromAdjustedStackPointerHighByte(self: *This) SelfRefCpuMethod {
            self.reg.HL.Hi = self.reg.AF.add_adj(self.reg.SP.Hi, self.reg.WZ.Lo);
            return self.fetchOpcode();
        }

        fn runAluOperationOnRegister8Bit(self: *This, next: fn (self: *This) SelfRefCpuMethod) SelfRefCpuMethod {
            const reg: u3 = @intCast(self.reg.IR & 0b0000_0_111);
            const reg_ptr = self.ptrReg8Bit(reg);
            self.reg.WZ.Lo = reg_ptr.read();

            if (reg_ptr == .memRef) {
                return SelfRefCpuMethod.init(next);
            } else {
                return next(self);
            }
        }

        fn doAdd(self: *This) SelfRefCpuMethod {
            const with_carry: u1 = @intCast((self.reg.IR & 0b0000_1_000) >> 3);
            self.reg.AF.add(self.reg.WZ.Lo, with_carry);
            return self.fetchOpcode();
        }

        fn doSub(self: *This) SelfRefCpuMethod {
            const with_carry: u1 = @intCast((self.reg.IR & 0b0000_1_000) >> 3);
            self.reg.AF.sub(self.reg.WZ.Lo, with_carry);
            return self.fetchOpcode();
        }

        fn doCmp(self: *This) SelfRefCpuMethod {
            const a = self.reg.AF.Hi;
            self.reg.AF.sub(self.reg.WZ.Lo, 0);
            self.reg.AF.Hi = a;
            return self.fetchOpcode();
        }

        fn doAnd(self: *This) SelfRefCpuMethod {
            self.reg.AF.and_(self.reg.WZ.Lo);
            return self.fetchOpcode();
        }

        fn doOr(self: *This) SelfRefCpuMethod {
            self.reg.AF.or_(self.reg.WZ.Lo);
            return self.fetchOpcode();
        }

        fn doXor(self: *This) SelfRefCpuMethod {
            self.reg.AF.xor(self.reg.WZ.Lo);
            return self.fetchOpcode();
        }

        fn doIncDecRegister(self: *This) SelfRefCpuMethod {
            var val = self.next_op_2.from_8bit.read();
            if (self.reg.IR & 0b00_000_001 == 0) {
                val = self.reg.AF.inc(val);
            } else {
                val = self.reg.AF.dec(val);
            }
            self.next_op_1.to_8bit.write(val);

            if (self.next_op_1.to_8bit == .memRef) {
                return SelfRefCpuMethod.init(This.fetchOpcode);
            } else {
                return self.fetchOpcode();
            }
        }

        fn addToStackPointerRelativeHighByte(self: *This) SelfRefCpuMethod {
            self.reg.WZ.Hi = self.reg.AF.add_adj(self.reg.SP.Hi, self.next_op_1.auxIntermediate);
            self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = &self.reg.SP };
            return SelfRefCpuMethod.init(This.loadTempRegisterTo16BitRegister);
        }

        fn jumpRelativeConditionalDoRelative(self: *This) SelfRefCpuMethod {
            const res = AluRegister.add_interpret_signed_no_flags(self.reg.PC, self.reg.WZ.Lo);
            self.reg.WZ.setAll(res);
            self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = &self.reg.WZ };
            return SelfRefCpuMethod.init(This.jumpToRegister);
        }

        fn jumpToRegister(self: *This) SelfRefCpuMethod {
            self.reg.PC = self.next_op_1.ptr_reg_16bit.all();
            return self.fetchOpcode();
        }

        fn jumpToRegisterLater(self: *This) SelfRefCpuMethod {
            self.reg.PC = self.next_op_1.ptr_reg_16bit.all();
            if (self.next_op_2.jump_sets_ime) {
                self.reg.IME = 1;
            }
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn returnFirstByte(self: *This) SelfRefCpuMethod {
            self.reg.WZ.Lo = self.mmu.read(self.reg.SP.all());
            return self.incrementSPAndContinue(This.returnSecondByte);
        }

        fn returnSecondByte(self: *This) SelfRefCpuMethod {
            self.reg.WZ.Hi = self.mmu.read(self.reg.SP.all());
            self.next_op_1 = CpuOp1Union{ .ptr_reg_16bit = &self.reg.WZ };
            return self.incrementSPAndContinue(This.jumpToRegisterLater);
        }

        fn restartStoreFirstByte(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.SP.all(), @intCast((self.reg.PC & 0xFF00) >> 8));
            return self.decrementSPAndContinue(This.restartStoreSecondByteAndJump);
        }

        fn restartStoreSecondByteAndJump(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.SP.all(), @intCast(self.reg.PC & 0x00FF));
            self.reg.PC = (self.reg.IR & 0b00_111_000) >> 3;
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn pushProgramCounterAndJump(self: *This) SelfRefCpuMethod {
            return self.decrementSPAndContinue(This.pushProgramCounterHighByteAndJump);
        }

        fn pushProgramCounterHighByteAndJump(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.SP.all(), @intCast((self.reg.PC & 0xFF00) >> 8));
            return self.decrementSPAndContinue(This.pushProgramCounterLowByteAndJump);
        }

        fn pushProgramCounterLowByteAndJump(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.SP.all(), @intCast(self.reg.PC & 0x00FF));
            return self.loadTempRegisterToProgramCounterAndFetchLater();
        }

        fn extendedAluOpOnMemory(self: *This) SelfRefCpuMethod {
            const res = self.next_op_1.next_alu_op(&self.reg.AF, self.reg.WZ.Lo);
            self.next_op_3.extendedAluOpTarget.write(res);
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn extendedAluOpOnMemoryBitsTest(self: *This) SelfRefCpuMethod {
            self.next_op_1.next_alu_op_bit_test(&self.reg.AF, self.reg.WZ.Lo, self.next_op_2.bitIdx);
            return self.fetchOpcode();
        }

        fn extendedAluOpOnMemoryBits(self: *This) SelfRefCpuMethod {
            const res = self.next_op_1.next_alu_op_bit(self.reg.WZ.Lo, self.next_op_2.bitIdx);
            self.next_op_3.extendedAluOpTarget.write(res);
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        fn decrementSPForIRQ(self: *This) SelfRefCpuMethod {
            return self.decrementSPAndContinue(This.pushPCHighForIRQ);
        }

        fn pushPCHighForIRQ(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.SP.all(), @intCast((self.reg.PC & 0xFF00) >> 8));
            return self.decrementSPAndContinue(This.pushPCLowAndStartIRQ);
        }

        fn pushPCLowAndStartIRQ(self: *This) SelfRefCpuMethod {
            self.mmu.write(self.reg.SP.all(), @intCast(self.reg.PC & 0x00FF));
            const kind = self.intr.pending().?;
            const addr = interruptVectors[kind.asOffset()];
            self.intr.acknowledge(kind);
            self.reg.IME = 0;
            self.reg.PC = addr;
            return SelfRefCpuMethod.init(This.fetchOpcode);
        }

        // REFERENCES //

        fn ptrReg8Bit(self: *This, idx: u3) MemoryReference {
            return switch (idx) {
                0b000 => MemoryReference.fromPointer(&self.reg.BC.Hi),
                0b001 => MemoryReference.fromPointer(&self.reg.BC.Lo),
                0b010 => MemoryReference.fromPointer(&self.reg.DE.Hi),
                0b011 => MemoryReference.fromPointer(&self.reg.DE.Lo),
                0b100 => MemoryReference.fromPointer(&self.reg.HL.Hi),
                0b101 => MemoryReference.fromPointer(&self.reg.HL.Lo),
                0b110 => MemoryReference.fromMemory(self.mmu, self.reg.HL.all()),
                0b111 => MemoryReference.fromPointer(&self.reg.AF.Hi),
            };
        }

        fn ptrReg16Bit(self: *This, idx: u2) *RegisterWithHalves {
            return switch (idx) {
                0b00 => &self.reg.BC,
                0b01 => &self.reg.DE,
                0b10 => &self.reg.HL,
                0b11 => &self.reg.SP,
            };
        }

        fn ptrRegStack(self: *This, idx: u2) StackRegister {
            return switch (idx) {
                0b00 => StackRegister{ .RegisterWithHalves = &self.reg.BC },
                0b01 => StackRegister{ .RegisterWithHalves = &self.reg.DE },
                0b10 => StackRegister{ .RegisterWithHalves = &self.reg.HL },
                0b11 => StackRegister{ .AluRegister = &self.reg.AF },
            };
        }
    };
}

const MockMmu = @import("mmu.zig").MockMmu;
const MockInterrupt = struct {};
const MockedCpu = Cpu(MockMmu, MockInterrupt);

test "Cpu: ptrReg8Bit maps correctly" {
    var mem = MockMmu{};
    var intr = MockInterrupt{};
    var cpu = MockedCpu.init(&mem, &intr, null);

    cpu.reg.BC.Hi = 0x10;
    cpu.reg.BC.Lo = 0x11;
    cpu.reg.DE.Hi = 0x12;
    cpu.reg.DE.Lo = 0x13;
    cpu.reg.HL.Hi = 0x14;
    cpu.reg.HL.Lo = 0x15;
    cpu.reg.AF.Hi = 0x16;

    try std.testing.expectEqual(@as(u8, 0x10), cpu.ptrReg8Bit(0b000).read());
    try std.testing.expectEqual(@as(u8, 0x11), cpu.ptrReg8Bit(0b001).read());
    try std.testing.expectEqual(@as(u8, 0x12), cpu.ptrReg8Bit(0b010).read());
    try std.testing.expectEqual(@as(u8, 0x13), cpu.ptrReg8Bit(0b011).read());
    try std.testing.expectEqual(@as(u8, 0x14), cpu.ptrReg8Bit(0b100).read());
    try std.testing.expectEqual(@as(u8, 0x15), cpu.ptrReg8Bit(0b101).read());
    try std.testing.expectEqual(@as(u8, 0x16), cpu.ptrReg8Bit(0b111).read());

    // For 0b110 (indirect HL), confirm it returns the expected memory reference address
    cpu.reg.HL.Hi = 0x12;
    cpu.reg.HL.Lo = 0x34;
    const ref = cpu.ptrReg8Bit(0b110);
    try std.testing.expectEqual(@as(u16, 0x1234), ref.memRef.addr);
}

test "Cpu: ptrReg16Bit maps correctly" {
    var mem = MockMmu{};
    var intr = MockInterrupt{};
    var cpu = MockedCpu.init(&mem, &intr, null);

    cpu.reg.BC.setAll(0x1111);
    cpu.reg.DE.setAll(0x2222);
    cpu.reg.HL.setAll(0x3333);
    cpu.reg.SP.setAll(0x4444);

    try std.testing.expectEqual(@as(u16, 0x1111), cpu.ptrReg16Bit(0b00).all());
    try std.testing.expectEqual(@as(u16, 0x2222), cpu.ptrReg16Bit(0b01).all());
    try std.testing.expectEqual(@as(u16, 0x3333), cpu.ptrReg16Bit(0b10).all());
    try std.testing.expectEqual(@as(u16, 0x4444), cpu.ptrReg16Bit(0b11).all());
}

test "Cpu: ptrRegStack maps correctly" {
    var mem = MockMmu{};
    var intr = MockInterrupt{};
    var cpu = MockedCpu.init(&mem, &intr, null);

    cpu.reg.BC.setAll(0xAAAA);
    cpu.reg.DE.setAll(0xBBBB);
    cpu.reg.HL.setAll(0xCCCC);
    cpu.reg.AF.setAll(0xDDDD);

    const bc = cpu.ptrRegStack(0b00);
    const de = cpu.ptrRegStack(0b01);
    const hl = cpu.ptrRegStack(0b10);
    const af = cpu.ptrRegStack(0b11);

    try std.testing.expectEqual(@as(u16, 0xAAAA), bc.all());
    try std.testing.expectEqual(@as(u16, 0xBBBB), de.all());
    try std.testing.expectEqual(@as(u16, 0xCCCC), hl.all());
    try std.testing.expectEqual(@as(u16, 0xDDDD), af.all());
}
