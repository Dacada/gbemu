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
};

pub const Cpu = struct {
    mmu: mmu.Mmu,
    register_bank: RegisterBank,

    state: CpuState,
    current_opcode: u8,
    value_to_store: u8,
    address_to_store_to: *u8,

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
            .address_to_store_to = undefined,
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
        }
    }

    fn fetchNextOpcode(self: *Cpu) mmu.MmuMemoryError!void {
        self.current_opcode = try self.mmu.read(self.register_bank.PC);
        self.register_bank.PC += 1;
        self.state = CpuState.start_current_opcode;
    }

    fn storeValue(self: *Cpu) void {
        self.address_to_store_to.* = self.value_to_store;
        self.address_to_store_to = undefined;
        self.value_to_store = undefined;
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
                self.address_to_store_to = to_ptr;
                self.state = CpuState.store_value_then_fetch;
            } else {
                to_ptr.* = val;
                self.state = CpuState.fetch_opcode_only;
            }
        }
    }
};

// Helper functions for testing

fn helper_set_cpu_register(cpu: *Cpu, reg: u3, val: u8) void {
    switch (reg) {
        0b000 => cpu.register_bank.BC.Hi = val,
        0b001 => cpu.register_bank.BC.Lo = val,
        0b010 => cpu.register_bank.DE.Hi = val,
        0b011 => cpu.register_bank.DE.Lo = val,
        0b100 => cpu.register_bank.HL.Hi = val,
        0b101 => cpu.register_bank.HL.Lo = val,
        0b111 => cpu.register_bank.AF.Hi = val,
        else => undefined,
    }
}

fn helper_expect_cpu_equal(actual: *const Cpu, expected: *const Cpu) !void {
    try std.testing.expectEqualSlices(u8, expected.mmu.rom, actual.mmu.rom);
    try std.testing.expectEqualSlices(u8, expected.mmu.wram, actual.mmu.wram);
    try std.testing.expectEqual(expected.register_bank.AF.all(), actual.register_bank.AF.all());
    try std.testing.expectEqual(expected.register_bank.BC.all(), actual.register_bank.BC.all());
    try std.testing.expectEqual(expected.register_bank.DE.all(), actual.register_bank.DE.all());
    try std.testing.expectEqual(expected.register_bank.HL.all(), actual.register_bank.HL.all());
    try std.testing.expectEqual(expected.register_bank.SP, actual.register_bank.SP);
    try std.testing.expectEqual(expected.register_bank.PC, actual.register_bank.PC);
}

fn make_cpu(exram: []u8, rom: []const u8, pc: ?u16, sp: ?u16, regs: ?[]const struct { u3, u8 }, addrs: ?[]const struct { u16, u8 }) !Cpu {
    var mmu_ = try mmu.Mmu.init(rom, exram, std.testing.allocator);
    mmu_.zeroize();

    var cpu = Cpu.init(mmu_);
    cpu.zeroize_regs();

    if (pc != null) {
        cpu.register_bank.PC = pc.?;
    }

    if (sp != null) {
        cpu.register_bank.SP = sp.?;
    }

    if (regs != null) {
        for (regs.?) |regval| {
            helper_set_cpu_register(&cpu, regval[0], regval[1]);
        }
    }

    if (addrs != null) {
        for (addrs.?) |addrval| {
            try mmu_.write(addrval[0], addrval[1]);
        }
    }

    return cpu;
}

fn destroy_cpu(cpu: *Cpu) void {
    cpu.mmu.deinit();
}

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

            // Constants
            const instr: u8 = @intCast((0b01 << 6) | (to << 3) | from);
            const test_value: u8 = 0x0D;

            var rom = try std.testing.allocator.alloc(u8, 0x8000);
            defer std.testing.allocator.free(rom);
            @memset(rom, 0x00);
            rom[0] = 0x00;
            rom[1] = instr;
            rom[2] = 0xFD;

            // setup CPU state
            var cpu = try make_cpu(exram, rom, null, null, &[_]struct { u3, u8 }{.{ @intCast(from), test_value }}, null);
            defer destroy_cpu(&cpu);

            // Setup expected state after first tick
            var cpu_1 = try make_cpu(exram, rom, 0x0001, null, &[_]struct { u3, u8 }{.{ @intCast(from), test_value }}, null);
            defer destroy_cpu(&cpu_1);

            // Setup expected state after second tick
            var cpu_2 = try make_cpu(exram, rom, 0x0002, null, &[_]struct { u3, u8 }{.{ @intCast(from), test_value }}, null);
            defer destroy_cpu(&cpu_2);

            // Setup expected state after third tick
            var cpu_3 = try make_cpu(exram, rom, 0x0003, null, &[_]struct { u3, u8 }{ .{ @intCast(from), test_value }, .{ @intCast(to), test_value } }, null);
            defer destroy_cpu(&cpu_3);

            // Execute
            try cpu.tick(); // load nop
            try helper_expect_cpu_equal(&cpu, &cpu_1);
            try cpu.tick(); // execute nop and load instruction under test
            try helper_expect_cpu_equal(&cpu, &cpu_2);
            try cpu.tick(); // execute instruction under test and load illegal instruction
            try helper_expect_cpu_equal(&cpu, &cpu_3);
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

        // Constants
        const instr: u8 = @intCast((0b01 << 6) | (0b110 << 3) | from);
        const test_addr: u16 = 0xD0D0;
        const test_value: u8 = 0xD0; // this makes the reading from H or from L test case easy

        var rom = try std.testing.allocator.alloc(u8, 0x8000);
        defer std.testing.allocator.free(rom);
        rom[0] = 0x00;
        rom[1] = instr;
        rom[2] = 0xFD;

        // Setup cpu state
        var cpu = try make_cpu(exram, rom, null, null, &[_]struct { u3, u8 }{ .{ @intCast(from), test_value }, .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, null);
        defer destroy_cpu(&cpu);

        // Setup expected state after first tick
        var cpu_1 = try make_cpu(exram, rom, 0x0001, null, &[_]struct { u3, u8 }{ .{ @intCast(from), test_value }, .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, null);
        defer destroy_cpu(&cpu_1);

        // Setup expected state after second tick
        var cpu_2 = try make_cpu(exram, rom, 0x0002, null, &[_]struct { u3, u8 }{ .{ @intCast(from), test_value }, .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, null);
        defer destroy_cpu(&cpu_2);

        // Setup expected state after third tick
        var cpu_3 = try make_cpu(exram, rom, 0x0002, null, &[_]struct { u3, u8 }{ .{ @intCast(from), test_value }, .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
        defer destroy_cpu(&cpu_3);

        // Setup expected state after fourth tick
        var cpu_4 = try make_cpu(exram, rom, 0x0003, null, &[_]struct { u3, u8 }{ .{ @intCast(from), test_value }, .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
        defer destroy_cpu(&cpu_4);

        try cpu.tick(); // load nop
        try helper_expect_cpu_equal(&cpu, &cpu_1);
        try cpu.tick(); // execute nop and load instruction under test
        try helper_expect_cpu_equal(&cpu, &cpu_2);
        try cpu.tick(); // execute instruction under test (write to ram)
        try helper_expect_cpu_equal(&cpu, &cpu_3);
        try cpu.tick(); // load illegal instruction
        try helper_expect_cpu_equal(&cpu, &cpu_4);
    }
}

test "ld register indirect to every register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }
        // Constants
        const instr: u8 = @intCast((0b01 << 6) | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD0D0;

        var rom = try std.testing.allocator.alloc(u8, 0x8000);
        defer std.testing.allocator.free(rom);
        rom[0] = 0x00;
        rom[1] = instr;
        rom[2] = 0xFD;

        // Setup cpu state
        var cpu = try make_cpu(exram, rom, null, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
        defer destroy_cpu(&cpu);

        // Setup expected state after first tick
        var cpu_1 = try make_cpu(exram, rom, 0x0001, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
        defer destroy_cpu(&cpu_1);

        // Setup expected state after second tick
        var cpu_2 = try make_cpu(exram, rom, 0x0002, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
        defer destroy_cpu(&cpu_2);

        // Setup expected state after second tick
        var cpu_3 = try make_cpu(exram, rom, 0x0002, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF }, .{ @intCast(to), test_value } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
        defer destroy_cpu(&cpu_3);

        // Setup expected state after second tick
        var cpu_4 = try make_cpu(exram, rom, 0x0003, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF }, .{ @intCast(to), test_value } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
        defer destroy_cpu(&cpu_4);

        try cpu.tick(); // load nop
        try helper_expect_cpu_equal(&cpu, &cpu_1);
        try cpu.tick(); // execute nop and load instruction under test
        try helper_expect_cpu_equal(&cpu, &cpu_2);
        try cpu.tick(); // execute instruction under test (read from ram)
        try helper_expect_cpu_equal(&cpu, &cpu_3);
        try cpu.tick(); // load illegal instruction
        try helper_expect_cpu_equal(&cpu, &cpu_4);
    }
}

test "ld immediate" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }

        // Constants
        const instr: u8 = @intCast(0b00 << 6 | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;

        var rom = try std.testing.allocator.alloc(u8, 0x8000);
        defer std.testing.allocator.free(rom);
        @memset(rom, 0x00);
        rom[0] = 0x00;
        rom[1] = instr;
        rom[2] = test_value;
        rom[3] = 0xFD;

        // setup CPU state
        var cpu = try make_cpu(exram, rom, null, null, null, null);
        defer destroy_cpu(&cpu);

        // Setup expected state after first tick
        var cpu_1 = try make_cpu(exram, rom, 0x0001, null, null, null);
        defer destroy_cpu(&cpu_1);

        // Setup expected state after second tick
        var cpu_2 = try make_cpu(exram, rom, 0x0002, null, null, null);
        defer destroy_cpu(&cpu_2);

        // Setup expected state after third tick
        var cpu_3 = try make_cpu(exram, rom, 0x0003, null, &[_]struct { u3, u8 }{.{ @intCast(to), test_value }}, null);
        defer destroy_cpu(&cpu_3);

        // Setup expected state after fourth tick
        var cpu_4 = try make_cpu(exram, rom, 0x0004, null, &[_]struct { u3, u8 }{.{ @intCast(to), test_value }}, null);
        defer destroy_cpu(&cpu_4);

        // Execute
        try cpu.tick(); // load nop
        try helper_expect_cpu_equal(&cpu, &cpu_1);
        try cpu.tick(); // execute nop and load instruction under test
        try helper_expect_cpu_equal(&cpu, &cpu_2);
        try cpu.tick(); // execute instruction under test: load immediate into register
        try helper_expect_cpu_equal(&cpu, &cpu_3);
        try cpu.tick(); // load illegal instruction
        try helper_expect_cpu_equal(&cpu, &cpu_4);
    }
}

test "ld immediate indirect" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    // Constants
    const instr: u8 = 0b00_110_110;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD0D0;

    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);
    @memset(rom, 0x00);
    rom[0] = 0x00;
    rom[1] = instr;
    rom[2] = test_value;
    rom[3] = 0xFD;

    // setup CPU state
    var cpu = try make_cpu(exram, rom, null, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, null);
    defer destroy_cpu(&cpu);

    // Setup expected state after first tick
    var cpu_1 = try make_cpu(exram, rom, 0x0001, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, null);
    defer destroy_cpu(&cpu_1);

    // Setup expected state after second tick
    var cpu_2 = try make_cpu(exram, rom, 0x0002, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, null);
    defer destroy_cpu(&cpu_2);

    // Setup expected state after third tick
    var cpu_3 = try make_cpu(exram, rom, 0x0003, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, null);
    defer destroy_cpu(&cpu_3);

    // Setup expected state after fourth tick
    var cpu_4 = try make_cpu(exram, rom, 0x0003, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
    defer destroy_cpu(&cpu_4);

    // Setup expected state after fifth tick
    var cpu_5 = try make_cpu(exram, rom, 0x0004, null, &[_]struct { u3, u8 }{ .{ 0b100, (test_addr & 0xFF00) >> 8 }, .{ 0b101, test_addr & 0xFF } }, &[_]struct { u16, u8 }{.{ test_addr, test_value }});
    defer destroy_cpu(&cpu_5);

    // Execute
    try cpu.tick(); // load nop
    try helper_expect_cpu_equal(&cpu, &cpu_1);
    try cpu.tick(); // execute nop and load instruction under test
    try helper_expect_cpu_equal(&cpu, &cpu_2);
    try cpu.tick(); // execute instruction under test: retrieve immediate
    try helper_expect_cpu_equal(&cpu, &cpu_3);
    try cpu.tick(); // execute instruction under test: load immediate to ram
    try helper_expect_cpu_equal(&cpu, &cpu_4);
    try cpu.tick(); // load illegal instruction
    try helper_expect_cpu_equal(&cpu, &cpu_5);
}

// Now follow some test programs, implemented as I add instructions.

test "test program 1" {
    // Only LD instructions, register or memory, no immediate

    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    var cpu = try make_cpu(exram, rom, null, null, null, null);
    defer destroy_cpu(&cpu);

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

    cpu.register_bank.BC.Hi = 0xD0;
    try cpu.mmu.write(0xD0D0, 0xFF);

    rom[0] = 0x68; // LD L, B
    rom[1] = 0x65; // LD H, L
    rom[2] = 0x46; // LD B, (HL)
    rom[3] = 0x74; // LD (HL), H
    rom[4] = 0x00; // NOP
    rom[5] = 0xFD; // (illegal)

    try cpu.tick(); // fetch instr 1
    try cpu.tick(); // exec instr 1, load instr 2
    try cpu.tick(); // exec instr 2, load instr 3
    try cpu.tick(); // exec instr 3
    try cpu.tick(); // load instr 4
    try cpu.tick(); // exec instr 4
    try cpu.tick(); // load instr 5
    try cpu.tick(); // exec instr 5, load instr 6

    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Hi);
    try std.testing.expectEqual(0xD0D0, cpu.register_bank.HL.all());
    try std.testing.expectEqual(0xD0, try cpu.mmu.read(0xD0D0));
}

test "test program 2" {
    // Only LD instructions, register, memory, immediate

    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    var cpu = try make_cpu(exram, rom, null, null, null, null);
    defer destroy_cpu(&cpu);

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

    try cpu.mmu.write(0xD00D, 0xFF);

    rom[0] = 0x26; // LD H, 0xD0
    rom[1] = 0xD0;
    rom[2] = 0x2E; // LD L, 0x0D
    rom[3] = 0x0D;
    rom[4] = 0x46; // LD B, (HL)
    rom[5] = 0x48; // LD C, B
    rom[6] = 0x36; // LD (HL), 0x00
    rom[7] = 0x00;
    rom[8] = 0x00; // NOP
    rom[9] = 0xFD; // (illegal)

    try cpu.tick(); // fetch instr 1
    try cpu.tick(); // exec instr 1
    try cpu.tick(); // fetch instr 2
    try cpu.tick(); // exc instr 2
    try cpu.tick(); // fetch instr 3
    try cpu.tick(); // exc instr 3, fetch instr 4
    try cpu.tick(); // exc instr 4, fetch instr 5
    try cpu.tick(); // exc instr 5 (fetch operand)
    try cpu.tick(); // exc instr 5 (store operand)
    try cpu.tick(); // fetch instr 6
    try cpu.tick(); // exc instr 6, fetch instr 7

    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Hi);
    try std.testing.expectEqual(0xFF, cpu.register_bank.BC.Lo);
    try std.testing.expectEqual(0x00, cpu.mmu.read(0xD00D));
}
