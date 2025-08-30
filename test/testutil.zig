const std = @import("std");
const lib = @import("lib");

const InterruptKind = lib.interrupt_kind.InterruptKind;

pub const FakeInterrupt = struct {
    pub fn pending(_: *FakeInterrupt) ?InterruptKind {
        return null;
    }
    pub fn acknowledge(_: *FakeInterrupt, _: InterruptKind) void {}
};

const Mmu = lib.mmu.MockMmu;
const Cpu = lib.cpu.Cpu(Mmu, FakeInterrupt);

const logger = std.log.scoped(.testutil);

const TestRamState = struct {
    address: u16,
    value: u8,
};

pub const TestCpuState = struct {
    flag_z: u1 = 0,
    flag_n: u1 = 0,
    flag_h: u1 = 0,
    flag_c: u1 = 0,
    flag_padding: u4 = 0,

    reg_a: u8 = 0,
    reg_b: u8 = 0,
    reg_c: u8 = 0,
    reg_d: u8 = 0,
    reg_e: u8 = 0,
    reg_h: u8 = 0,
    reg_l: u8 = 0,

    reg_sp: u16 = 0,
    reg_pc: u16 = 0,

    reg_ime: u1 = 0,

    addresses: std.ArrayList(TestRamState),

    pub fn init() *TestCpuState {
        const state = std.testing.allocator.create(TestCpuState) catch @panic("allocation failed");
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
        self.flag_z = v;
        return self;
    }

    pub fn fN(self: *TestCpuState, v: u1) *TestCpuState {
        self.flag_n = v;
        return self;
    }

    pub fn fH(self: *TestCpuState, v: u1) *TestCpuState {
        self.flag_h = v;
        return self;
    }

    pub fn fC(self: *TestCpuState, v: u1) *TestCpuState {
        self.flag_c = v;
        return self;
    }

    pub fn rA(self: *TestCpuState, v: u8) *TestCpuState {
        self.reg_a = v;
        return self;
    }

    pub fn rF(self: *TestCpuState, v: u8) *TestCpuState {
        self.flag_z = @intCast((v & 0b1000_0000) >> 7);
        self.flag_n = @intCast((v & 0b0100_0000) >> 6);
        self.flag_h = @intCast((v & 0b0010_0000) >> 5);
        self.flag_c = @intCast((v & 0b0001_0000) >> 4);
        self.flag_padding = @intCast(v & 0xF);
        return self;
    }

    pub fn rAf(self: *TestCpuState, v: u16) *TestCpuState {
        self.reg_a = @intCast((v & 0xFF00) >> 8);
        _ = self.rF(@intCast(v & 0xFF));
        return self;
    }

    pub fn rB(self: *TestCpuState, v: u8) *TestCpuState {
        self.reg_b = v;
        return self;
    }

    pub fn rC(self: *TestCpuState, v: u8) *TestCpuState {
        self.reg_c = v;
        return self;
    }

    pub fn rBc(self: *TestCpuState, v: u16) *TestCpuState {
        self.reg_b = @intCast((v & 0xFF00) >> 8);
        self.reg_c = @intCast(v & 0x00FF);
        return self;
    }

    pub fn rD(self: *TestCpuState, v: u8) *TestCpuState {
        self.reg_d = v;
        return self;
    }

    pub fn rE(self: *TestCpuState, v: u8) *TestCpuState {
        self.reg_e = v;
        return self;
    }

    pub fn rDe(self: *TestCpuState, v: u16) *TestCpuState {
        self.reg_d = @intCast((v & 0xFF00) >> 8);
        self.reg_e = @intCast(v & 0x00FF);
        return self;
    }

    pub fn rH(self: *TestCpuState, v: u8) *TestCpuState {
        self.reg_h = v;
        return self;
    }

    pub fn rL(self: *TestCpuState, v: u8) *TestCpuState {
        self.reg_l = v;
        return self;
    }

    pub fn rHL(self: *TestCpuState, v: u16) *TestCpuState {
        self.reg_h = @intCast((v & 0xFF00) >> 8);
        self.reg_l = @intCast(v & 0x00FF);
        return self;
    }

    pub fn rSp(self: *TestCpuState, v: u16) *TestCpuState {
        self.reg_sp = v;
        return self;
    }

    pub fn rPc(self: *TestCpuState, v: u16) *TestCpuState {
        self.reg_pc = v;
        return self;
    }

    pub fn rIme(self: *TestCpuState, v: u1) *TestCpuState {
        self.reg_ime = v;
        return self;
    }

    pub fn ram(self: *TestCpuState, addr: u16, val: u8) *TestCpuState {
        // again, rely on this always running under debug context so it will panic
        self.addresses.append(TestRamState{ .address = addr, .value = val }) catch unreachable;
        return self;
    }

    pub fn reg(self: *TestCpuState, r: u3, val: u8) *TestCpuState {
        switch (r) {
            0b111 => self.reg_a = val,
            0b000 => self.reg_b = val,
            0b001 => self.reg_c = val,
            0b010 => self.reg_d = val,
            0b011 => self.reg_e = val,
            0b100 => self.reg_h = val,
            0b101 => self.reg_l = val,
            else => undefined,
        }
        return self;
    }

    pub fn reg16(self: *TestCpuState, r: u2, val: u16) *TestCpuState {
        return switch (r) {
            0b00 => self.rBc(val),
            0b01 => self.rDe(val),
            0b10 => self.rHL(val),
            0b11 => self.rSp(val),
        };
    }

    pub fn reg16p(self: *TestCpuState, r: u2, val: u16) *TestCpuState {
        return switch (r) {
            0b00 => self.rBc(val),
            0b01 => self.rDe(val),
            0b10 => self.rHL(val),
            0b11 => self.rAf(val),
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

fn expectCpuState(cpu: *const Cpu, state: *TestCpuState, program: []const u8) !void {
    try std.testing.expectEqual(state.flag_z, cpu.reg.af.lo.z);
    try std.testing.expectEqual(state.flag_n, cpu.reg.af.lo.n);
    try std.testing.expectEqual(state.flag_h, cpu.reg.af.lo.h);
    try std.testing.expectEqual(state.flag_c, cpu.reg.af.lo.c);
    try std.testing.expectEqual(state.flag_padding, cpu.reg.af.lo.rest);
    try std.testing.expectEqual(state.reg_a, cpu.reg.af.hi);
    try std.testing.expectEqual(state.reg_b, cpu.reg.bc.hi);
    try std.testing.expectEqual(state.reg_c, cpu.reg.bc.lo);
    try std.testing.expectEqual(state.reg_d, cpu.reg.de.hi);
    try std.testing.expectEqual(state.reg_e, cpu.reg.de.lo);
    try std.testing.expectEqual(state.reg_h, cpu.reg.hl.hi);
    try std.testing.expectEqual(state.reg_l, cpu.reg.hl.lo);
    try std.testing.expectEqual(state.reg_sp, cpu.reg.sp.all());
    try std.testing.expectEqual(state.reg_pc, cpu.reg.pc);
    try std.testing.expectEqual(state.reg_ime, cpu.reg.ime);

    var expect_memory = [_]u8{0} ** (0xFFFF + 1);
    for (program, 0..) |instr, i| {
        expect_memory[i] = instr;
    }
    for (state.addresses.items) |s| {
        expect_memory[s.address] = s.value;
    }

    try std.testing.expectEqualSlices(u8, &expect_memory, &Mmu.backing_array);
}

fn mapInitialState(cpu: *Cpu, mmu: *Mmu, initial_state: *TestCpuState, program: []const u8) !void {
    cpu.reg.af.lo.z = initial_state.flag_z;
    cpu.reg.af.lo.n = initial_state.flag_n;
    cpu.reg.af.lo.h = initial_state.flag_h;
    cpu.reg.af.lo.c = initial_state.flag_c;
    cpu.reg.af.lo.rest = initial_state.flag_padding;
    cpu.reg.af.hi = initial_state.reg_a;
    cpu.reg.bc.hi = initial_state.reg_b;
    cpu.reg.bc.lo = initial_state.reg_c;
    cpu.reg.de.hi = initial_state.reg_d;
    cpu.reg.de.lo = initial_state.reg_e;
    cpu.reg.hl.hi = initial_state.reg_h;
    cpu.reg.hl.lo = initial_state.reg_l;
    cpu.reg.sp.setAll(initial_state.reg_sp);
    cpu.reg.pc = initial_state.reg_pc;
    cpu.reg.ime = initial_state.reg_ime;

    for (initial_state.addresses.items) |state| {
        mmu.write(state.address, state.value);
    }
    for (program, 0..) |b, i| {
        mmu.write(@intCast(i), b);
    }

    initial_state.deinit();
}

fn zeroizeRegisters(cpu: *Cpu) void {
    cpu.reg.af.setAll(0x0000);
    cpu.reg.bc.setAll(0x0000);
    cpu.reg.de.setAll(0x0000);
    cpu.reg.hl.setAll(0x0000);
    cpu.reg.sp.setAll(0x0000);
    cpu.reg.pc = 0x0000;
}

pub fn runTestCase(name: []const u8, program: []const u8, initial_state: *TestCpuState, ticks: []const *TestCpuState) !void {
    @memset(&Mmu.backing_array, 0);
    var mmu = Mmu{};
    var intr = FakeInterrupt{};
    var cpu = Cpu.init(&mmu, &intr, null);
    zeroizeRegisters(&cpu);

    try mapInitialState(&cpu, &mmu, initial_state, program);

    defer for (ticks) |state| {
        state.deinit();
    };

    for (ticks, 1..) |state, idx| {
        cpu.tick();
        try std.testing.expect(!mmu.flags.illegal);
        try std.testing.expect(!cpu.flags.illegal);
        expectCpuState(&cpu, state, program) catch |err| {
            logger.debug("{s}: Failed on tick {d}", .{ name, idx });
            return err;
        };
    }
}

pub fn runProgram(program: []const u8) !Cpu {
    @memset(&Mmu.backing_array, 0);
    var mmu = Mmu{};
    var intr = FakeInterrupt{};
    var cpu = Cpu.init(&mmu, &intr, 0x40);
    zeroizeRegisters(&cpu);

    for (program, 0..) |b, i| {
        mmu.write(@intCast(i), b);
    }

    while (true) {
        cpu.tick();
        if (cpu.flags.breakpoint) {
            cpu.reg.pc -= 1;
            break;
        }
        try std.testing.expect(!cpu.flags.illegal);
        try std.testing.expect(!mmu.flags.illegal);
    }

    return cpu;
}
