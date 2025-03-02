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
        if (self.current_opcode == 0xFD) {
            // Helper for testing, at least for now
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

// Very specific tests

const TestRamState = struct {
    address: u16,
    value: u8,
};

const TestCpuState = struct {
    flagZ: u1 = 0,
    flagN: u1 = 0,
    flagH: u1 = 0,
    flagC: u1 = 0,

    regA: u8 = 0,
    regB: u8 = 0,
    regC: u8 = 0,
    regD: u8 = 0,
    regE: u8 = 0,
    regH: u8 = 0,
    regL: u8 = 0,

    regSP: u16 = 0,
    regPC: u16 = 0,

    addresses: std.ArrayList(TestRamState),

    fn init() *TestCpuState {
        // this should panic since we're only ever running this in a debug context
        const state = std.testing.allocator.create(TestCpuState) catch unreachable;
        state.* = TestCpuState{
            .addresses = std.ArrayList(TestRamState).init(std.testing.allocator),
        };
        return state;
    }

    fn deinit(self: *TestCpuState) void {
        self.addresses.deinit();
        std.testing.allocator.destroy(self);
    }

    fn fZ(self: *TestCpuState, v: u1) *TestCpuState {
        self.flagZ = v;
        return self;
    }

    fn fN(self: *TestCpuState, v: u1) *TestCpuState {
        self.flagN = v;
        return self;
    }

    fn fH(self: *TestCpuState, v: u1) *TestCpuState {
        self.flagH = v;
        return self;
    }

    fn fC(self: *TestCpuState, v: u1) *TestCpuState {
        self.flagC = v;
        return self;
    }

    fn rA(self: *TestCpuState, v: u8) *TestCpuState {
        self.regA = v;
        return self;
    }

    fn rB(self: *TestCpuState, v: u8) *TestCpuState {
        self.regB = v;
        return self;
    }

    fn rC(self: *TestCpuState, v: u8) *TestCpuState {
        self.regC = v;
        return self;
    }

    fn rBC(self: *TestCpuState, v: u16) *TestCpuState {
        self.regB = @intCast((v & 0xFF00) >> 8);
        self.regC = @intCast(v & 0x00FF);
        return self;
    }

    fn rD(self: *TestCpuState, v: u8) *TestCpuState {
        self.regD = v;
        return self;
    }

    fn rE(self: *TestCpuState, v: u8) *TestCpuState {
        self.regE = v;
        return self;
    }

    fn rDE(self: *TestCpuState, v: u16) *TestCpuState {
        self.regD = @intCast((v & 0xFF00) >> 8);
        self.regE = @intCast(v & 0x00FF);
        return self;
    }

    fn rH(self: *TestCpuState, v: u8) *TestCpuState {
        self.regH = v;
        return self;
    }

    fn rL(self: *TestCpuState, v: u8) *TestCpuState {
        self.regL = v;
        return self;
    }

    fn rHL(self: *TestCpuState, v: u16) *TestCpuState {
        self.regH = @intCast((v & 0xFF00) >> 8);
        self.regL = @intCast(v & 0x00FF);
        return self;
    }

    fn rSP(self: *TestCpuState, v: u16) *TestCpuState {
        self.regSP = v;
        return self;
    }

    fn rPC(self: *TestCpuState, v: u16) *TestCpuState {
        self.regPC = v;
        return self;
    }

    fn ram(self: *TestCpuState, addr: u16, val: u8) *TestCpuState {
        // again, rely on this always running under debug context so it will panic
        self.addresses.append(TestRamState{ .address = addr, .value = val }) catch unreachable;
        return self;
    }

    fn reg(self: *TestCpuState, r: u3, val: u8) *TestCpuState {
        switch (r) {
            0b111 => self.regA = val,
            0b000 => self.regB = val,
            0b001 => self.regC = val,
            0b010 => self.regD = val,
            0b011 => self.regE = val,
            0b100 => self.regH = val,
            0b101 => self.regL = val,
            else => undefined,
        }
        return self;
    }

    fn reg16(self: *TestCpuState, r: u1, val: u16) *TestCpuState {
        return switch (r) {
            0b0 => self.rBC(val),
            0b1 => self.rDE(val),
        };
    }
};

fn make_cpu(rom: []const u8, exram: []u8) !Cpu {
    var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
    mmu_.zeroize();
    var cpu = Cpu.init(mmu_);
    cpu.zeroize_regs();
    return cpu;
}

fn destroy_cpu(cpu: *const Cpu) void {
    cpu.mmu.deinit();
}

fn expect_cpu_state(cpu: *const Cpu, state: *TestCpuState) !void {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);
    @memset(exram, 0);
    var mmu_ = try mmu.Mmu.init(cpu.mmu.rom, exram, std.testing.allocator);
    defer mmu_.deinit();
    mmu_.zeroize();

    for (state.addresses.items) |s| {
        try mmu_.write(s.address, s.value);
    }

    try std.testing.expectEqual(state.flagZ, cpu.register_bank.AF.Lo.Z);
    try std.testing.expectEqual(state.flagN, cpu.register_bank.AF.Lo.N);
    try std.testing.expectEqual(state.flagH, cpu.register_bank.AF.Lo.H);
    try std.testing.expectEqual(state.flagC, cpu.register_bank.AF.Lo.C);
    try std.testing.expectEqual(state.regA, cpu.register_bank.AF.Hi);
    try std.testing.expectEqual(state.regB, cpu.register_bank.BC.Hi);
    try std.testing.expectEqual(state.regC, cpu.register_bank.BC.Lo);
    try std.testing.expectEqual(state.regD, cpu.register_bank.DE.Hi);
    try std.testing.expectEqual(state.regE, cpu.register_bank.DE.Lo);
    try std.testing.expectEqual(state.regH, cpu.register_bank.HL.Hi);
    try std.testing.expectEqual(state.regL, cpu.register_bank.HL.Lo);
    try std.testing.expectEqual(state.regSP, cpu.register_bank.SP);
    try std.testing.expectEqual(state.regPC, cpu.register_bank.PC);

    try std.testing.expectEqualSlices(u8, mmu_.vram, cpu.mmu.vram);
    try std.testing.expectEqualSlices(u8, mmu_.exram, cpu.mmu.vram);
    try std.testing.expectEqualSlices(u8, mmu_.wram, cpu.mmu.wram);
    try std.testing.expectEqualSlices(u8, mmu_.oam, cpu.mmu.oam);
    try std.testing.expectEqualSlices(u8, mmu_.io, cpu.mmu.io);
    try std.testing.expectEqualSlices(u8, mmu_.hram, cpu.mmu.hram);
    try std.testing.expectEqual(mmu_.ie, cpu.mmu.ie);
}

fn map_initial_state(cpu: *Cpu, initial_state: *TestCpuState) !void {
    cpu.register_bank.AF.Lo.Z = initial_state.flagZ;
    cpu.register_bank.AF.Lo.N = initial_state.flagN;
    cpu.register_bank.AF.Lo.H = initial_state.flagH;
    cpu.register_bank.AF.Lo.C = initial_state.flagC;
    cpu.register_bank.AF.Hi = initial_state.regA;
    cpu.register_bank.BC.Hi = initial_state.regB;
    cpu.register_bank.BC.Lo = initial_state.regC;
    cpu.register_bank.DE.Hi = initial_state.regD;
    cpu.register_bank.DE.Lo = initial_state.regE;
    cpu.register_bank.HL.Hi = initial_state.regH;
    cpu.register_bank.HL.Lo = initial_state.regL;
    cpu.register_bank.SP = initial_state.regSP;
    cpu.register_bank.PC = initial_state.regPC;

    for (initial_state.addresses.items) |state| {
        try cpu.mmu.write(state.address, state.value);
    }

    initial_state.deinit();
}

fn run_test_case(rom: []u8, exram: []u8, program: []const u8, initial_state: *TestCpuState, ticks: []const *TestCpuState) !void {
    var cpu = try make_cpu(rom, exram);
    defer destroy_cpu(&cpu);

    @memset(rom, 0);
    for (program, 0..) |instr, idx| {
        rom[idx] = instr;
    }

    try map_initial_state(&cpu, initial_state);

    defer for (ticks) |state| {
        state.deinit();
    };

    for (ticks, 1..) |state, idx| {
        try cpu.tick();
        expect_cpu_state(&cpu, state) catch |err| {
            std.debug.print("Failed on tick {d}\n", .{idx});
            return err;
        };
    }
}

fn run_program(program: []const u8, initial_state: *TestCpuState) !Cpu {
    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    var cpu = try make_cpu(rom, exram);

    @memset(rom, 0);
    for (program, 0..) |instr, idx| {
        rom[idx] = instr;
    }

    try map_initial_state(&cpu, initial_state);

    while (true) {
        cpu.tick() catch |err| switch (err) {
            CpuError.IllegalInstruction => break,
            else => return err,
        };
    }

    return cpu;
}

test "ld register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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

            try run_test_case(
                rom,
                exram,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .reg(@intCast(from), test_value),
                &[_]*TestCpuState{
                    TestCpuState.init() // load nop
                        .rPC(0x0001)
                        .reg(@intCast(from), test_value),
                    TestCpuState.init() // execute nop and load instruction under test
                        .rPC(0x0002)
                        .reg(@intCast(from), test_value),
                    TestCpuState.init() // execute instruction under test and load illegal instruction
                        .rPC(0x0003)
                        .reg(@intCast(from), test_value)
                        .reg(@intCast(to), test_value),
                },
            );
        }
    }
}

test "ld register indirect from every register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..(0b111 + 1)) |from| {
        if (from == 0b110) {
            continue;
        }

        // Constants
        const instr: u8 = @intCast((0b01 << 6) | (0b110 << 3) | from);
        const test_addr: u16 = 0xD0D0;
        const test_value: u8 = 0xD0; // this makes the reading from H or from L test case easy

        try run_test_case(
            rom,
            exram,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rHL(test_addr)
                .reg(@intCast(from), test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // load nop
                    .rPC(0x0001)
                    .rHL(test_addr)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // execute nop and load instruction under test
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // execute instruction under test (write to ram)
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // load illegal instruction
                    .rPC(0x0003)
                    .rHL(test_addr)
                    .ram(test_addr, test_value)
                    .reg(@intCast(from), test_value),
            },
        );
    }
}

test "ld register indirect to every register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }

        // Constants
        const instr: u8 = @intCast((0b01 << 6) | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD0D0;

        try run_test_case(
            rom,
            exram,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rHL(test_addr)
                .ram(test_addr, test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // load nop
                    .rPC(0x0001)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop and load instruction under test
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute instruction under test (read from ram)
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .reg(@intCast(to), test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // load illegal instruction
                    .rPC(0x0003)
                    .rHL(test_addr)
                    .reg(@intCast(to), test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "ld immediate" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }

        // Constants
        const instr: u8 = @intCast(0b00 << 6 | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;

        try run_test_case(
            rom,
            exram,
            &[_]u8{
                0x00,
                instr,
                test_value,
                0xFD,
            },
            TestCpuState.init(),
            &[_]*TestCpuState{
                TestCpuState.init() // load nop
                    .rPC(0x0001),
                TestCpuState.init() // execute nop and load instruction under test
                    .rPC(0x0002),
                TestCpuState.init() // execute instruction under test: load immediate into register
                    .rPC(0x0003)
                    .reg(@intCast(to), test_value),
                TestCpuState.init() // load illegal instruction
                    .rPC(0x0004)
                    .reg(@intCast(to), test_value),
            },
        );
    }
}

test "ld immediate indirect" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b00_110_110;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD0D0;

    try run_test_case(
        rom,
        exram,
        &[_]u8{
            0x00,
            instr,
            test_value,
            0xFD,
        },
        TestCpuState.init()
            .rHL(test_addr),
        &[_]*TestCpuState{
            TestCpuState.init() // load nop
                .rPC(0x0001)
                .rHL(test_addr),
            TestCpuState.init() // execute nop and load instruction under test
                .rPC(0x0002)
                .rHL(test_addr),
            TestCpuState.init() // execute instruction under test: retrieve immediate
                .rPC(0x0003)
                .rHL(test_addr),
            TestCpuState.init() // execute instruction under test: load immediate to ram
                .rPC(0x0003)
                .rHL(test_addr)
                .ram(test_addr, test_value),
            TestCpuState.init() // load illegal instruction
                .rPC(0x0004)
                .rHL(test_addr)
                .ram(test_addr, test_value),
        },
    );
}

test "ld accumulator indirect" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..2) |from| {
        // Constants
        const instr: u8 = @intCast(0b000_0_1010 | (from << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD00D;

        try run_test_case(
            rom,
            exram,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .reg16(@intCast(from), test_addr)
                .ram(test_addr, test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // load nop
                    .rPC(0x0001)
                    .reg16(@intCast(from), test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop and load instruction under test
                    .rPC(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute instruction under test: load from ram
                    .rPC(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // load illegal instruction
                    .rPC(0x0003)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "ld from accumulator indirect" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..2) |from| {
        // Constants
        const instr: u8 = @intCast(0b000_0_0010 | (from << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD00D;

        try run_test_case(
            rom,
            exram,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .reg16(@intCast(from), test_addr)
                .rA(test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // load nop
                    .rPC(0x0001)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute nop and load instruction under test
                    .rPC(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute instruction under test: load from ram
                    .rPC(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // load illegal instruction
                    .rPC(0x0003)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "ld to accumulator direct" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11111010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD00D;

    try run_test_case(
        rom,
        exram,
        &[_]u8{
            0x00,
            instr,
            @intCast(test_addr & 0xFF),
            @intCast((test_addr & 0xFF00) >> 8),
            0xFD,
        },
        TestCpuState.init()
            .ram(test_addr, test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // load nop
                .rPC(0x0001)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop and load instruction under test
                .rPC(0x0002)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute instruction under test: get lsb of address
                .rPC(0x0003)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute instruction under test: get msb of address
                .rPC(0x0004)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute instruction under test: store address value to A
                .rPC(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // fetch illegal instruction
                .rPC(0x0005)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "ld from accumulator direct" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11101010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD00D;

    try run_test_case(
        rom,
        exram,
        &[_]u8{
            0x00,
            instr,
            @intCast(test_addr & 0xFF),
            @intCast((test_addr & 0xFF00) >> 8),
            0xFD,
        },
        TestCpuState.init()
            .rA(test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // load nop
                .rPC(0x0001)
                .rA(test_value),
            TestCpuState.init() // execute nop and load instruction under test
                .rPC(0x0002)
                .rA(test_value),
            TestCpuState.init() // execute instruction under test: get lsb of address
                .rPC(0x0003)
                .rA(test_value),
            TestCpuState.init() // execute instruction under test: get msb of address
                .rPC(0x0004)
                .rA(test_value),
            TestCpuState.init() // execute instruction under test: store address value to A
                .rPC(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // fetch illegal instruction
                .rPC(0x0005)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

// Now follow some test programs, implemented as I add instructions.

test "test program 1" {
    // Only LD instructions, register or memory, no immediate

    // This program swaps the values of B and the value from dereferencing RAM on the address you get from using B on high and low. In pseudocode:

    //   B = 0xD0
    //   RAM[0xD0D0] = 0xFF
    //   ---
    //   H = B
    //   L = B
    //   B = RAM[HL]
    //   RAM[HL] = H
    //   ---
    //   ASSERT B == 0xFF
    //   ASSERT HL == 0xD0D0
    //   ASSERT RAM[0xD0D0] = 0xD0

    const cpu = try run_program(
        &[_]u8{
            0x68, // LD L, B
            0x65, // LD H, L
            0x46, // LD B, (HL)
            0x74, // LD (HL), H
            0x00, // NOP
            0xFD, // (illegal)
        },
        TestCpuState.init()
            .rB(0xD0)
            .ram(0xD0D0, 0xFF),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Hi);
    try std.testing.expectEqual(0xD0D0, cpu.register_bank.HL.all());
    try std.testing.expectEqual(0xD0, try cpu.mmu.read(0xD0D0));
}

test "test program 2" {
    // Only LD instructions, register, memory, immediate

    // This program simply loads the value on RAM address 0xD00D into registers B and C. Then puts something else in there.

    //   RAM[0xD00D] = 0xFF
    //   ---
    //   H = 0xD0
    //   L = 0x0D
    //   B = RAM[HL]
    //   C = B
    //   RAM[HL] = 0x00
    //   ---
    //   ASSERT B == 0xFF
    //   ASSERT C == 0xFF
    //   ASSERT RAM[0xD00D] == 0x00

    const cpu = try run_program(
        &[_]u8{
            0x26, // LD H, 0xD0
            0xD0,
            0x2E, // LD L, 0x0D
            0x0D,
            0x46, // LD B, (HL)
            0x48, // LD C, B
            0x36, // LD (HL), 0x00
            0x00,
            0x00, // NOP
            0xFD, // (illegal)
        },
        TestCpuState.init()
            .ram(0xD00D, 0xFF),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Hi);
    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Lo);
    try std.testing.expectEqual(0x00, cpu.mmu.read(0xD00D));
}

test "test program 3" {
    // Only LD instructions indirect from and to A

    // This program loads the value from (BC) into (DE) using A as an intermediary.

    // RAM[0xD00D] = 0xFF
    // BC = 0xD00D
    // DE = 0xDDDD
    // ---
    // A = RAM[BC]
    // RAM[DE] = A
    // ---
    // ASSERT A == 0xFF
    // ASSERT RAM[0xDDDD] == 0xFF

    const cpu = try run_program(
        &[_]u8{
            0x0A, // LD A, (BC)
            0x12, // LD (DE), A
            0x00, // NOP
            0xFD, // (illegal)
        },
        TestCpuState.init()
            .ram(0xD00D, 0xFF)
            .rBC(0xD00D)
            .rDE(0xDDDD),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.AF.Hi);
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xDDDD));
}

test "test program 4" {
    // Only LD instructions direct from and to A

    // This program loads the value from 0xD00D into 0xDDDD using A as an intermediary.

    // RAM[0xD00D] = 0xFF
    // ---
    // A = RAM[0xD00D]
    // RAM[0xDDDD] = A
    // ---
    // ASSERT A == 0xFF
    // ASSERT RAM[0xDDDD] == 0xFF

    const cpu = try run_program(
        &[_]u8{
            0xFA, // LD A, 0xD00D
            0x0D,
            0xD0,
            0xEA, // LD 0xDDDD, A
            0xDD,
            0xDD,
            0x00, // NOP
            0xFD, // (illegal)
        },
        TestCpuState.init()
            .ram(0xD00D, 0xFF),
    );
    defer destroy_cpu(&cpu);

    try std.testing.expectEqual(0xFF, cpu.register_bank.AF.Hi);
    try std.testing.expectEqual(0xFF, cpu.mmu.read(0xDDDD));
}
