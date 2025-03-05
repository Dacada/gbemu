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
    SP: u16,
    PC: u16,
};

const CpuState = enum {
    fetch_opcode_only,
    start_current_opcode,
    store_value_then_fetch,
    finish_fetching_address,
    store_or_load_address_accumulator,
};

pub const Cpu = struct {
    mmu: mmu.Mmu,
    register_bank: RegisterBank,

    state: CpuState,
    current_opcode: u8,
    value_to_store: u8,
    ptr_to_store_to: *u8,
    address_being_read: u16,

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
            .value_to_store = undefined,
            .ptr_to_store_to = undefined,
            .address_being_read = undefined,
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
            CpuState.store_value_then_fetch => self.storeValue(),
            CpuState.finish_fetching_address => try self.finishFetchingAddress(),
            CpuState.store_or_load_address_accumulator => try self.storeOrLoadReadAddress(),
        }
    }

    fn fetchNextOpcode(self: *Cpu) mmu.MmuMemoryError!void {
        self.current_opcode = try self.mmu.read(self.register_bank.PC);
        self.register_bank.PC += 1;
        self.state = CpuState.start_current_opcode;
    }

    fn storeValue(self: *Cpu) void {
        self.ptr_to_store_to.* = self.value_to_store;
        self.ptr_to_store_to = undefined;
        self.value_to_store = undefined;
        self.state = CpuState.fetch_opcode_only;
    }

    fn finishFetchingAddress(self: *Cpu) !void {
        var val: u16 = try self.mmu.read(self.register_bank.PC);
        val <<= 8;
        self.address_being_read |= val;
        self.register_bank.PC += 1;
        self.state = CpuState.store_or_load_address_accumulator;
    }

    fn storeOrLoadReadAddress(self: *Cpu) !void {
        const rw = (self.current_opcode & 0b000_1_0000) >> 4;
        if (rw == 1) {
            self.register_bank.AF.Hi = try self.mmu.read(self.address_being_read);
        } else {
            try self.mmu.write(self.address_being_read, self.register_bank.AF.Hi);
        }
        self.address_being_read = undefined;
        self.state = CpuState.fetch_opcode_only;
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
        // NOP
        if (self.current_opcode == 0) {
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

        // LD register or indirect (HL) immediate
        else if (self.current_opcode & 0b11_000_111 == 0b00_000_110) {
            const to: u3 = @intCast((self.current_opcode & 0b00_111_000) >> 3);
            const to_ptr = try self.ptrOpcodeRegOrMmu(to, false);

            const val = try self.mmu.read(self.register_bank.PC);
            self.register_bank.PC += 1;

            if (to == 0b110) {
                self.value_to_store = val;
                self.ptr_to_store_to = to_ptr;
                self.state = CpuState.store_value_then_fetch;
            } else {
                to_ptr.* = val;
                self.state = CpuState.fetch_opcode_only;
            }
        }

        // LD accumulator indirect
        else if (self.current_opcode & 0b111_00_111 == 0b000_00_010) {
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
            self.state = CpuState.fetch_opcode_only;
        }

        // LD accumulator direct
        else if (self.current_opcode & 0b111_0_1111 == 0b111_0_1010) {
            self.address_being_read = try self.mmu.read(self.register_bank.PC);
            self.register_bank.PC += 1;
            self.state = CpuState.finish_fetching_address;
        }

        // LDH accumulator indirect
        else if (self.current_opcode & 0b111_0_1111 == 0b111_0_0010) {
            const addr: u16 = 0xFF00 | @as(u16, self.register_bank.BC.Lo);
            const rw = (self.current_opcode & 0b000_1_0000) >> 4;
            if (rw == 1) {
                self.register_bank.AF.Hi = try self.mmu.read(addr);
            } else {
                try self.mmu.write(addr, self.register_bank.AF.Hi);
            }
            self.state = CpuState.fetch_opcode_only;
        }

        // LDH accumulator direct
        else if (self.current_opcode & 0b111_0_1111 == 0b111_0_0000) {
            const immediate: u16 = try self.mmu.read(self.register_bank.PC);
            self.register_bank.PC += 1;
            self.address_being_read = 0xFF00 | immediate;
            self.state = CpuState.store_or_load_address_accumulator;
        }

        // LD accumulator indirect HL inc/dec
        else if (self.current_opcode & 0b111_00_111 == 0b001_00_010) {
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

            self.state = CpuState.fetch_opcode_only;
        }

        // Catch anything else
        else {
            return CpuError.IllegalInstruction;
        }
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
