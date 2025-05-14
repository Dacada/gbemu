const std = @import("std");
const Cpu = @import("lib").cpu.Cpu;
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

    regIME: u1 = 0,

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

    pub fn rIME(self: *TestCpuState, v: u1) *TestCpuState {
        self.regIME = v;
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

fn setFlagRead(flagOpaque: *anyopaque) u8 {
    const flag: *bool = @ptrCast(flagOpaque);
    flag.* = true;
    return 0;
}

fn setFlagWrite(flagOpaque: *anyopaque, _: u8) void {
    const flag: *bool = @ptrCast(flagOpaque);
    flag.* = true;
}

fn makeMmu() !Mmu {
    var mmu = Mmu.init();
    mmu.zeroize();
    return mmu;
}

fn makeCpu() !Cpu {
    const mmu = try makeMmu();
    var cpu = Cpu.init(mmu);
    cpu.zeroize_regs();
    return cpu;
}

pub fn destroyCpu(cpu: *const Cpu) void {
    cpu.mmu.deinit();
}

fn expectCpuState(cpu: *const Cpu, state: *TestCpuState, program: []const u8) !void {
    var expectedMemory: [0xFFFF + 1]u8 = undefined;
    for (program, 0..) |instr, i| {
        expectedMemory[i] = instr;
    }
    for (state.addresses.items) |s| {
        expectedMemory[s.address] = s.value;
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
    try std.testing.expectEqual(state.regIME, cpu.reg.IME);

    try std.testing.expectEqualSlices(u8, expectedMemory, cpu.mmu.memory);
}

fn mapInitialState(cpu: *Cpu, initial_state: *TestCpuState, program: []const u8) !void {
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
    cpu.reg.IME = initial_state.regIME;

    for (initial_state.addresses.items) |state| {
        cpu.mmu.write(state.address, state.value);
    }
    for (program, 0..) |instr, idx| {
        cpu.mmu.memory[idx] = instr;
    }

    initial_state.deinit();
}

pub fn runTestCase(name: []const u8, program: []const u8, initial_state: *TestCpuState, ticks: []const *TestCpuState) !void {
    var cpu = try makeCpu();
    defer destroyCpu(&cpu);

    try mapInitialState(&cpu, initial_state, program);

    defer for (ticks) |state| {
        state.deinit();
    };

    for (ticks, 1..) |state, idx| {
        cpu.tick();
        try std.testing.expect(!cpu.mmu.illegalMemoryOperationHappened);
        try std.testing.expect(!cpu.illegalInstructionExecuted);
        expectCpuState(&cpu, state, program) catch |err| {
            std.debug.print("{s}:\n  Failed on tick {d}\n", .{ name, idx });
            return err;
        };
    }
}

pub fn runProgram(name: []const u8, program: []const u8) !Cpu {
    std.debug.print("Running program: {s}\n", .{name});
    var cpu = try makeCpu();

    for (program, 0..) |instr, idx| {
        cpu.mmu.memory[idx] = instr;
    }

    while (true) {
        cpu.tick();
        if (cpu.illegalInstructionExecuted) {
            break;
        }
        try std.testing.expect(!cpu.mmu.illegalMemoryOperationHappened);
    }

    return cpu;
}
