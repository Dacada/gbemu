const std = @import("std");
const mmu = @import("mmu.zig");

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
    init,
    start_current_opcode,
};

pub const Cpu = struct {
    mmu: mmu.Mmu,
    register_bank: RegisterBank,

    state: CpuState,
    current_opcode: u8,

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

            .state = CpuState.init,
            .current_opcode = undefined,
        };
    }

    pub fn tick(self: *Cpu) mmu.MmuMemoryError!void {
        switch (self.state) {
            CpuState.init => try self.fetchNextOpcode(),
            CpuState.start_current_opcode => try self.decodeAndSetup(),
        }
    }

    fn fetchNextOpcode(self: *Cpu) mmu.MmuMemoryError!void {
        self.current_opcode = try self.mmu.read(self.register_bank.PC);
        self.state = CpuState.start_current_opcode;
    }

    fn indexRegister(self: *Cpu, idx: u3, comptime ro: bool) if (ro) mmu.MmuMemoryError!*const u8 else mmu.MmuMemoryError!*u8 {
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

    fn decodeAndSetup(self: *Cpu) mmu.MmuMemoryError!void {
        defer self.register_bank.PC += 1;

        // NOP
        if (self.current_opcode == 0) {
            self.state = CpuState.init;
        }

        // HALT
        if (self.current_opcode == 0x76) {
            // TODO: Figure out how HALT works
            self.state = CpuState.init;
        }

        // LD register
        else if ((self.current_opcode & 0b01_000_000) >> 6 == 0b01) {
            const to: u3 = @intCast((self.current_opcode & 0b00_111_000) >> 3);
            const from: u3 = @intCast(self.current_opcode & 0b00_000_111);
            try self.doLDRegister(from, to);
            self.state = CpuState.init;
        }
    }

    fn doLDRegister(self: *Cpu, from: u3, to: u3) mmu.MmuMemoryError!void {
        const from_reg = try self.indexRegister(from, true);
        const to_reg = try self.indexRegister(to, false);
        to_reg.* = from_reg.*;
    }
};

test "fetch" {
    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);
    rom[0] = 0xAA;
    var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
    defer mmu_.deinit(std.testing.allocator);
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
    var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
    defer mmu_.deinit(std.testing.allocator);
    var cpu = Cpu.init(mmu_);

    try cpu.tick();
    try cpu.tick();

    try std.testing.expectEqual(1, cpu.register_bank.PC);
    try std.testing.expectEqual(CpuState.init, cpu.state);
}

fn test_setup_reg_from(cpu: *Cpu, reg: u3, val: u8) !void {
    switch (reg) {
        0b000 => cpu.register_bank.BC.Hi = val,
        0b001 => cpu.register_bank.BC.Lo = val,
        0b010 => cpu.register_bank.DE.Hi = val,
        0b011 => cpu.register_bank.DE.Lo = val,
        0b100 => cpu.register_bank.HL.Hi = val,
        0b101 => cpu.register_bank.HL.Lo = val,
        0b111 => cpu.register_bank.AF.Hi = val,
        0b110 => {
            return std.testing.expect(false);
        },
    }
}

fn test_check_reg_to(cpu: *const Cpu, from: u3, to: u3, val: u8, val_for_HL: u8) !void {
    for (0..(0b111 + 1)) |addr_| {
        const addr: u3 = @intCast(addr_);
        const actual = switch (addr) {
            0b000 => cpu.register_bank.BC.Hi,
            0b001 => cpu.register_bank.BC.Lo,
            0b010 => cpu.register_bank.DE.Hi,
            0b011 => cpu.register_bank.DE.Lo,
            0b100 => cpu.register_bank.HL.Hi,
            0b101 => cpu.register_bank.HL.Lo,
            0b111 => cpu.register_bank.AF.Hi,
            0b110 => {
                continue;
            },
        };
        if (addr == from or addr == to) {
            try std.testing.expectEqual(val, actual);
        } else {
            if (addr == 0b100 or addr == 0b101) {
                try std.testing.expectEqual(val_for_HL, actual);
            } else {
                try std.testing.expectEqual(0, actual);
            }
        }
    }
}

test "ld register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |from| {
        if (from == 0b110) {
            continue;
        }
        for (0..(0b111 + 1)) |to| {
            if (to == 0b110) {
                continue;
            }
            const instr: u8 = @intCast((0b01 << 6) | (to << 3) | from);
            const test_value: u8 = 0x0D;

            var rom = try std.testing.allocator.alloc(u8, 0x8000);
            defer std.testing.allocator.free(rom);
            rom[0] = instr;

            var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
            defer mmu_.deinit(std.testing.allocator);

            var cpu = Cpu.init(mmu_);
            cpu.register_bank.AF.setAll(0x0000);
            cpu.register_bank.BC.setAll(0x0000);
            cpu.register_bank.DE.setAll(0x0000);
            cpu.register_bank.HL.setAll(0x0000);
            cpu.register_bank.SP = 0x0000;
            cpu.register_bank.PC = 0x0000;
            try test_setup_reg_from(&cpu, @intCast(from), test_value);

            try cpu.tick();
            try cpu.tick();

            try test_check_reg_to(&cpu, @intCast(from), @intCast(to), test_value, 0);
        }
    }
}

test "ld register indirect from every register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |from| {
        if (from == 0b110) {
            continue;
        }
        const instr: u8 = @intCast((0b01 << 6) | (0b110 << 3) | from);
        const test_value: u8 = 0xD0;
        const test_addr: u16 = 0xD0D0;

        var rom = try std.testing.allocator.alloc(u8, 0x8000);
        defer std.testing.allocator.free(rom);
        rom[0] = instr;

        var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_.deinit(std.testing.allocator);

        var cpu = Cpu.init(mmu_);
        cpu.register_bank.AF.setAll(0x0000);
        cpu.register_bank.BC.setAll(0x0000);
        cpu.register_bank.DE.setAll(0x0000);
        cpu.register_bank.HL.setAll(test_addr);
        cpu.register_bank.SP = 0x0000;
        cpu.register_bank.PC = 0x0000;
        try test_setup_reg_from(&cpu, @intCast(from), test_value);

        try cpu.tick();
        try cpu.tick();

        try std.testing.expectEqual(test_value, mmu_.read(test_addr));
    }
}

test "ld register indirect to every register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }
        const instr: u8 = @intCast((0b01 << 6) | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD0D0;

        var rom = try std.testing.allocator.alloc(u8, 0x8000);
        defer std.testing.allocator.free(rom);
        rom[0] = instr;

        var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
        defer mmu_.deinit(std.testing.allocator);

        var cpu = Cpu.init(mmu_);
        cpu.register_bank.AF.setAll(0x0000);
        cpu.register_bank.BC.setAll(0x0000);
        cpu.register_bank.DE.setAll(0x0000);
        cpu.register_bank.HL.setAll(test_addr);
        cpu.register_bank.SP = 0x0000;
        cpu.register_bank.PC = 0x0000;
        try mmu_.write(test_addr, test_value);

        try cpu.tick();
        try cpu.tick();

        try test_check_reg_to(&cpu, 0b110, @intCast(to), test_value, 0xD0);
    }
}
