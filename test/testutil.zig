const std = @import("std");
const Cpu = @import("lib").cpu.Cpu;
const CpuError = @import("lib").cpu.CpuError;
const Mmu = @import("lib").mmu.Mmu;

const TestRamState = struct {
    address: u16,
    value: u8,
};

pub const TestCpuState = struct {
    flagZ: u1 = 0,
    flagN: u1 = 0,
    flagH: u1 = 0,
    flagC: u1 = 0,
    flagPadding: u4 = 0,

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

    pub fn init() *TestCpuState {
        // this should panic since we're only ever running this in a debug context
        const state = std.testing.allocator.create(TestCpuState) catch unreachable;
        state.* = TestCpuState{
            .addresses = std.ArrayList(TestRamState).init(std.testing.allocator),
        };
        return state;
    }

    pub fn deinit(self: *TestCpuState) void {
        self.addresses.deinit();
        std.testing.allocator.destroy(self);
    }

    pub fn fZ(self: *TestCpuState, v: u1) *TestCpuState {
        self.flagZ = v;
        return self;
    }

    pub fn fN(self: *TestCpuState, v: u1) *TestCpuState {
        self.flagN = v;
        return self;
    }

    pub fn fH(self: *TestCpuState, v: u1) *TestCpuState {
        self.flagH = v;
        return self;
    }

    pub fn fC(self: *TestCpuState, v: u1) *TestCpuState {
        self.flagC = v;
        return self;
    }

    pub fn rA(self: *TestCpuState, v: u8) *TestCpuState {
        self.regA = v;
        return self;
    }

    pub fn rF(self: *TestCpuState, v: u8) *TestCpuState {
        self.flagZ = @intCast((v & 0b1000_0000) >> 7);
        self.flagN = @intCast((v & 0b0100_0000) >> 6);
        self.flagH = @intCast((v & 0b0010_0000) >> 5);
        self.flagC = @intCast((v & 0b0001_0000) >> 4);
        self.flagPadding = @intCast(v & 0xF);
        return self;
    }

    pub fn rAF(self: *TestCpuState, v: u16) *TestCpuState {
        self.regA = @intCast((v & 0xFF00) >> 8);
        _ = self.rF(@intCast(v & 0xFF));
        return self;
    }

    pub fn rB(self: *TestCpuState, v: u8) *TestCpuState {
        self.regB = v;
        return self;
    }

    pub fn rC(self: *TestCpuState, v: u8) *TestCpuState {
        self.regC = v;
        return self;
    }

    pub fn rBC(self: *TestCpuState, v: u16) *TestCpuState {
        self.regB = @intCast((v & 0xFF00) >> 8);
        self.regC = @intCast(v & 0x00FF);
        return self;
    }

    pub fn rD(self: *TestCpuState, v: u8) *TestCpuState {
        self.regD = v;
        return self;
    }

    pub fn rE(self: *TestCpuState, v: u8) *TestCpuState {
        self.regE = v;
        return self;
    }

    pub fn rDE(self: *TestCpuState, v: u16) *TestCpuState {
        self.regD = @intCast((v & 0xFF00) >> 8);
        self.regE = @intCast(v & 0x00FF);
        return self;
    }

    pub fn rH(self: *TestCpuState, v: u8) *TestCpuState {
        self.regH = v;
        return self;
    }

    pub fn rL(self: *TestCpuState, v: u8) *TestCpuState {
        self.regL = v;
        return self;
    }

    pub fn rHL(self: *TestCpuState, v: u16) *TestCpuState {
        self.regH = @intCast((v & 0xFF00) >> 8);
        self.regL = @intCast(v & 0x00FF);
        return self;
    }

    pub fn rSP(self: *TestCpuState, v: u16) *TestCpuState {
        self.regSP = v;
        return self;
    }

    pub fn rPC(self: *TestCpuState, v: u16) *TestCpuState {
        self.regPC = v;
        return self;
    }

    pub fn ram(self: *TestCpuState, addr: u16, val: u8) *TestCpuState {
        // again, rely on this always running under debug context so it will panic
        self.addresses.append(TestRamState{ .address = addr, .value = val }) catch unreachable;
        return self;
    }

    pub fn reg(self: *TestCpuState, r: u3, val: u8) *TestCpuState {
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

    pub fn reg16(self: *TestCpuState, r: u2, val: u16) *TestCpuState {
        return switch (r) {
            0b00 => self.rBC(val),
            0b01 => self.rDE(val),
            0b10 => self.rHL(val),
            0b11 => self.rSP(val),
        };
    }

    pub fn reg16p(self: *TestCpuState, r: u2, val: u16) *TestCpuState {
        return switch (r) {
            0b00 => self.rBC(val),
            0b01 => self.rDE(val),
            0b10 => self.rHL(val),
            0b11 => self.rAF(val),
        };
    }
};

fn make_cpu(rom: []const u8, exram: []u8) !Cpu {
    var mmu = try Mmu.init(rom, exram, std.testing.allocator);
    mmu.zeroize();
    var cpu = Cpu.init(mmu);
    cpu.zeroize_regs();
    return cpu;
}

pub fn destroy_cpu(cpu: *const Cpu) void {
    cpu.mmu.deinit();
}

fn expect_cpu_state(cpu: *const Cpu, state: *TestCpuState) !void {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);
    @memset(exram, 0);
    var mmu = try Mmu.init(cpu.mmu.rom, exram, std.testing.allocator);
    defer mmu.deinit();
    mmu.zeroize();

    for (state.addresses.items) |s| {
        try mmu.write(s.address, s.value);
    }

    try std.testing.expectEqual(state.flagZ, cpu.reg.AF.Lo.Z);
    try std.testing.expectEqual(state.flagN, cpu.reg.AF.Lo.N);
    try std.testing.expectEqual(state.flagH, cpu.reg.AF.Lo.H);
    try std.testing.expectEqual(state.flagC, cpu.reg.AF.Lo.C);
    try std.testing.expectEqual(state.flagPadding, cpu.reg.AF.Lo.rest);
    try std.testing.expectEqual(state.regA, cpu.reg.AF.Hi);
    try std.testing.expectEqual(state.regB, cpu.reg.BC.Hi);
    try std.testing.expectEqual(state.regC, cpu.reg.BC.Lo);
    try std.testing.expectEqual(state.regD, cpu.reg.DE.Hi);
    try std.testing.expectEqual(state.regE, cpu.reg.DE.Lo);
    try std.testing.expectEqual(state.regH, cpu.reg.HL.Hi);
    try std.testing.expectEqual(state.regL, cpu.reg.HL.Lo);
    try std.testing.expectEqual(state.regSP, cpu.reg.SP.all());
    try std.testing.expectEqual(state.regPC, cpu.reg.PC);

    try std.testing.expectEqualSlices(u8, mmu.vram, cpu.mmu.vram);
    try std.testing.expectEqualSlices(u8, mmu.exram, cpu.mmu.vram);
    try std.testing.expectEqualSlices(u8, mmu.wram, cpu.mmu.wram);
    try std.testing.expectEqualSlices(u8, mmu.oam, cpu.mmu.oam);
    try std.testing.expectEqualSlices(u8, mmu.io, cpu.mmu.io);
    try std.testing.expectEqualSlices(u8, mmu.hram, cpu.mmu.hram);
    try std.testing.expectEqual(mmu.ie, cpu.mmu.ie);
}

fn map_initial_state(cpu: *Cpu, initial_state: *TestCpuState) !void {
    cpu.reg.AF.Lo.Z = initial_state.flagZ;
    cpu.reg.AF.Lo.N = initial_state.flagN;
    cpu.reg.AF.Lo.H = initial_state.flagH;
    cpu.reg.AF.Lo.C = initial_state.flagC;
    cpu.reg.AF.Lo.rest = initial_state.flagPadding;
    cpu.reg.AF.Hi = initial_state.regA;
    cpu.reg.BC.Hi = initial_state.regB;
    cpu.reg.BC.Lo = initial_state.regC;
    cpu.reg.DE.Hi = initial_state.regD;
    cpu.reg.DE.Lo = initial_state.regE;
    cpu.reg.HL.Hi = initial_state.regH;
    cpu.reg.HL.Lo = initial_state.regL;
    cpu.reg.SP.setAll(initial_state.regSP);
    cpu.reg.PC = initial_state.regPC;

    for (initial_state.addresses.items) |state| {
        try cpu.mmu.write(state.address, state.value);
    }

    initial_state.deinit();
}

pub fn run_test_case(name: []const u8, rom: []u8, exram: []u8, program: []const u8, initial_state: *TestCpuState, ticks: []const *TestCpuState) !void {
    std.debug.print("Running test case: {s}\n", .{name});

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

pub fn run_program(name: []const u8, program: []const u8) !Cpu {
    std.debug.print("Running program: {s}\n", .{name});

    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    var cpu = try make_cpu(rom, exram);

    @memset(rom, 0);
    for (program, 0..) |instr, idx| {
        rom[idx] = instr;
    }

    while (true) {
        cpu.tick() catch |err| switch (err) {
            CpuError.IllegalInstruction => break,
            else => return err,
        };
    }

    return cpu;
}
