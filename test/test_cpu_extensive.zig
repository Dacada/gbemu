const std = @import("std");
const testutil = @import("testutil.zig");
const runTestCase = testutil.runTestCase;
const TestCpuState = testutil.TestCpuState;
const alu = @import("lib").alu;

// IUT = Instruction Under Test

test "Load register (register)" {
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

            const name = try std.fmt.allocPrint(std.testing.allocator, "Load register (from={b} to={b})", .{ from, to });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .reg(@intCast(from), test_value),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .reg(@intCast(from), test_value),
                    TestCpuState.init() // execute nop | load iut(PC) from ram
                        .rPc(0x0002)
                        .reg(@intCast(from), test_value),
                    TestCpuState.init() // execute iut | read (PC) from ram
                        .rPc(0x0003)
                        .reg(@intCast(from), test_value)
                        .reg(@intCast(to), test_value),
                },
            );
        }
    }
}

test "Load register (immediate)" {
    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }

        // Constants
        const instr: u8 = @intCast(0b00 << 6 | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load register (immediate) (to={b})", .{to});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                test_value,
                0xFD,
            },
            TestCpuState.init(),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002),
                TestCpuState.init() // execute iut: read immediate(PC) from ram
                    .rPc(0x0003),
                TestCpuState.init() // read (PC) from ram | write immediate to reg
                    .rPc(0x0004)
                    .reg(@intCast(to), test_value),
            },
        );
    }
}

test "Load register (indirect HL)" {
    for (0..(0b111 + 1)) |to| {
        if (to == 0b110) {
            continue;
        }

        // Constants
        const instr: u8 = @intCast((0b01 << 6) | (to << 3) | 0b110);
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD0D0;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load register (indirect HL) (to={b})", .{to});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rHL(test_addr)
                .ram(test_addr, test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute iut: read data(HL) from ram
                    .rPc(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram | write data to reg
                    .rPc(0x0003)
                    .rHL(test_addr)
                    .reg(@intCast(to), test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load from register (indirect HL)" {
    for (0..(0b111 + 1)) |from| {
        if (from == 0b110) {
            continue;
        }

        // Constants
        const instr: u8 = @intCast((0b01 << 6) | (0b110 << 3) | from);
        const test_addr: u16 = 0xD0D0;
        const test_value: u8 = 0xD0; // this makes the reading from H or from L test case easy

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD from register (indirect HL) (from={b})", .{from});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rHL(test_addr)
                .reg(@intCast(from), test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rHL(test_addr)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rHL(test_addr)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // execute iut: write reg(HL) to ram
                    .rPc(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // read (PC) from ram
                    .rPc(0x0003)
                    .rHL(test_addr)
                    .ram(test_addr, test_value)
                    .reg(@intCast(from), test_value),
            },
        );
    }
}

test "Load from immediate data (indirect HL)" {
    // Constants
    const instr: u8 = 0b00_110_110;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD0D0;

    try runTestCase(
        "Load from immediate data (indirect HL)",
        &[_]u8{
            0x00,
            instr,
            test_value,
            0xFD,
        },
        TestCpuState.init()
            .rHL(test_addr),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rHL(test_addr),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rHL(test_addr),
            TestCpuState.init() // execute iut: read immediate(PC) from ram
                .rPc(0x0003)
                .rHL(test_addr),
            TestCpuState.init() // execute iut: write immediate(HL) to ram
                .rPc(0x0003)
                .rHL(test_addr)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPc(0x0004)
                .rHL(test_addr)
                .ram(test_addr, test_value),
        },
    );
}

test "Load accumulator (indirect)" {
    for (0..2) |from| {
        // Constants
        const instr: u8 = @intCast(0b000_0_1010 | (from << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD00D;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load accumulator (indirect) (from={b})", .{from});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .reg16(@intCast(from), test_addr)
                .ram(test_addr, test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .reg16(@intCast(from), test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute iut: read data(reg) from ram
                    .rPc(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram | write data to A
                    .rPc(0x0003)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load from accumulator (indirect)" {
    for (0..2) |from| {
        // Constants
        const instr: u8 = @intCast(0b000_0_0010 | (from << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD00D;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load from accumulator (indirect) (from={b})", .{from});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .reg16(@intCast(from), test_addr)
                .rA(test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute iut: write A(reg) to ram
                    .rPc(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram
                    .rPc(0x0003)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load accumulator (direct)" {
    // Constants
    const instr: u8 = 0b11111010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD00D;

    try runTestCase(
        "Load accumulator (direct)",
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
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read address_lsb(PC) from ram
                .rPc(0x0003)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read address_msb(PC) from ram
                .rPc(0x0004)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read data(address) from ram
                .rPc(0x0004)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram | write data to A
                .rPc(0x0005)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load from accumulator (direct)" {
    // Constants
    const instr: u8 = 0b11101010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD00D;

    try runTestCase(
        "Load from accumulator (direct)",
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
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rA(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rA(test_value),
            TestCpuState.init() // execute iut: read address_lsb(PC) from ram
                .rPc(0x0003)
                .rA(test_value),
            TestCpuState.init() // execute iut: read address_msb(PC) from ram
                .rPc(0x0004)
                .rA(test_value),
            TestCpuState.init() // execute iut: write A(address) to ram
                .rPc(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPc(0x0005)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load accumulator (indirect 0xFF00 + C)" {
    // Constants
    const instr: u8 = 0b11110010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try runTestCase(
        "Load accumulator (indirect 0xFF00 + C)",
        &[_]u8{
            0x00,
            instr,
            0xFD,
        },
        TestCpuState.init()
            .rC(test_addr & 0xFF)
            .ram(test_addr, test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rC(test_addr & 0xFF)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rC(test_addr & 0xFF)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read data(0xFF00+C) from ram
                .rPc(0x0002)
                .rC(test_addr & 0xFF)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram | write data to A
                .rPc(0x0003)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load from accumulator (indirect 0xFF00 + C)" {
    // Constants
    const instr: u8 = 0b11100010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try runTestCase(
        "Load from accumulator (indirect 0xFF00 + C)",
        &[_]u8{
            0x00,
            instr,
            0xFD,
        },
        TestCpuState.init()
            .rC(test_addr & 0xFF)
            .rA(test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rC(test_addr & 0xFF)
                .rA(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rC(test_addr & 0xFF)
                .rA(test_value),
            TestCpuState.init() // execute iut: write A(0xFF00+C) to ram
                .rPc(0x0002)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPc(0x0003)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load accumulator (direct 0xFF00 + n)" {
    // Constants
    const instr: u8 = 0b11110000;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try runTestCase(
        "Load accumulator (direct 0xFF00 + n)",
        &[_]u8{
            0x00,
            instr,
            test_addr & 0x00FF,
            0xFD,
        },
        TestCpuState.init()
            .ram(test_addr, test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read immediate(PC) from ram
                .rPc(0x0003)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read data(0xFF00+immediate) from ram
                .rPc(0x0003)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPc(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load from accumulator (direct 0xFF00 + n)" {
    // Constants
    const instr: u8 = 0b11100000;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try runTestCase(
        "Load from accumulator (direct 0xFF00 + n)",
        &[_]u8{
            0x00,
            instr,
            test_addr & 0x00FF,
            0xFD,
        },
        TestCpuState.init()
            .rA(test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rA(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rA(test_value),
            TestCpuState.init() // execute iut: read immediate(PC) from ram
                .rPc(0x0003)
                .rA(test_value),
            TestCpuState.init() // execute iut: write A(0xFF00+immediate) to ram
                .rPc(0x0003)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPc(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load accumulator (indirect HL)" {
    inline for (0..2) |incdec| {
        // Constants
        const instr: u8 = @intCast(0b00101010 | (incdec << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xFFAA;
        const test_addr_next = switch (incdec) {
            0 => test_addr + 1,
            1 => test_addr - 1,
            else => unreachable,
        };

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load accumulator (indirect HL) (inc/dec={b})", .{incdec});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rHL(test_addr)
                .ram(test_addr, test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop | load iut(PC) from ram
                    .rPc(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute iut: read data(HL) from ram | inc/dec HL
                    .rPc(0x0002)
                    .rHL(test_addr_next)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram | write data to A
                    .rPc(0x0003)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load from accumulator (indirect HL)" {
    inline for (0..2) |incdec| {
        // Constants
        const instr: u8 = @intCast(0b00100010 | (incdec << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xFFAA;
        const test_addr_next = switch (incdec) {
            0 => test_addr + 1,
            1 => test_addr - 1,
            else => unreachable,
        };

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load from accumulator (indirect HL) (inc/dev={b})", .{incdec});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rHL(test_addr)
                .rA(test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rHL(test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rHL(test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute iut: write A(HL) to ram
                    .rPc(0x0002)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram
                    .rPc(0x0003)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load 16-bit register" {
    inline for (0..(0b11 + 1)) |reg| {
        // Constants
        const instr: u8 = @intCast(0b00000001 | (reg << 4));
        const test_value: u16 = 0xABCD;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load 16-bit register (reg={b})", .{reg});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                test_value & 0x00FF,
                (test_value & 0xFF00) >> 8,
                0xFD,
            },
            TestCpuState.init(),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002),
                TestCpuState.init() // execute iut: read data_lsb(PC) from ram
                    .rPc(0x0003),
                TestCpuState.init() // execute iut: read data_msb(PC) from ram
                    .rPc(0x0004),
                TestCpuState.init() // read (PC) | write data to reg
                    .rPc(0x0005)
                    .reg16(reg, test_value),
            },
        );
    }
}

test "Load from stack pointer (direct)" {
    // Constants
    const instr = 0b00001000;
    const test_addr = 0xD00D;
    const test_value = 0xFFAA;

    try runTestCase(
        "Load from stack pointer (direct)",
        &[_]u8{
            0x00,
            instr,
            @intCast(test_addr & 0xFF),
            @intCast((test_addr & 0xFF00) >> 8),
            0xFD,
        },
        TestCpuState.init()
            .rSp(test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rSp(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rSp(test_value),
            TestCpuState.init() // execute iut: read address_lsb(PC) from ram
                .rPc(0x0003)
                .rSp(test_value),
            TestCpuState.init() // execute iut: read address_msb(PC) from ram
                .rPc(0x0004)
                .rSp(test_value),
            TestCpuState.init() // execute iut: write SP_lsb(address) to ram
                .rPc(0x0004)
                .rSp(test_value)
                .ram(test_addr, test_value & 0xFF),
            TestCpuState.init() // execute iut: write SP_msb(address) to ram
                .rPc(0x0004)
                .rSp(test_value)
                .ram(test_addr, test_value & 0xFF)
                .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
            TestCpuState.init() // read (PC) from ram
                .rPc(0x0005)
                .rSp(test_value)
                .ram(test_addr, test_value & 0xFF)
                .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
        },
    );
}

test "Load stack pointer from HL" {
    // Constants
    const instr = 0b11111001;
    const test_value = 0xFFAA;

    try runTestCase(
        "Load stack pointer from HL",
        &[_]u8{
            0x00,
            instr,
            0xFD,
        },
        TestCpuState.init()
            .rHL(test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rHL(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rHL(test_value),
            TestCpuState.init() // execute iut: write HL to SP
                .rPc(0x0002)
                .rHL(test_value)
                .rSp(test_value),
            TestCpuState.init() // read (PC) from ram
                .rPc(0x0003)
                .rHL(test_value)
                .rSp(test_value),
        },
    );
}

test "Push to stack" {
    inline for (0..(0b11 + 1)) |reg| {
        // Constants
        const instr: u8 = @intCast(0b11000101 | (reg << 4));
        const test_value: u16 = 0xABCD;
        const test_addr: u16 = 0xFFAA;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Push to stack (reg={b})", .{reg});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rSp(test_addr)
                .reg16p(reg, test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rSp(test_addr)
                    .reg16p(reg, test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rSp(test_addr)
                    .reg16p(reg, test_value),
                TestCpuState.init() // execute iut: decrease SP
                    .rPc(0x0002)
                    .rSp(test_addr - 1)
                    .reg16p(reg, test_value),
                TestCpuState.init() // execute iut: write reg_msb[SP] to ram | decrease SP
                    .rPc(0x0002)
                    .rSp(test_addr - 2)
                    .reg16p(reg, test_value)
                    .ram(test_addr - 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // execute iut: write reg_lsb[SP] to ram
                    .rPc(0x0002)
                    .rSp(test_addr - 2)
                    .reg16p(reg, test_value)
                    .ram(test_addr - 1, (test_value & 0xFF00) >> 8)
                    .ram(test_addr - 2, (test_value & 0xFF)),
                TestCpuState.init() // read (PC)
                    .rPc(0x0003)
                    .rSp(test_addr - 2)
                    .reg16p(reg, test_value)
                    .ram(test_addr - 1, (test_value & 0xFF00) >> 8)
                    .ram(test_addr - 2, (test_value & 0xFF)),
            },
        );
    }
}

test "Pop from stack" {
    inline for (0..(0b11 + 1)) |reg| {
        // Constants
        const instr: u8 = @intCast(0b11000001 | (reg << 4));
        const test_value: u16 = 0xABCD;
        const test_addr: u16 = 0xFFAA;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Pop from stack (reg={b})", .{reg});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rSp(test_addr)
                .ram(test_addr, test_value & 0xFF)
                .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rSp(test_addr)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rSp(test_addr)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // execute iut: read data_lsb[SP] | increase SP
                    .rPc(0x0002)
                    .rSp(test_addr + 1)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // execute iut: read data_msb[SP] | increase SP
                    .rPc(0x0002)
                    .rSp(test_addr + 2)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // read (PC) | write data to reg
                    .rPc(0x0003)
                    .rSp(test_addr + 2)
                    .reg16p(reg, test_value)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
            },
        );
    }
}

test "Load HL from adjusted SP" {
    inline for (.{ 0x00, 0x01, 0x04, 0x0F, 0xF4, 0xFF }) |e| {
        // Constants
        const instr: u8 = 0b11111000;
        const test_value: u16 = 0x100D;

        const unsigned_e: u8 = @intCast(e);

        var reg = alu.AluRegister{
            .hi = 0x00,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        const expected_lo = reg.addReturn(@intCast(test_value & 0xFF), unsigned_e, 0, 0);
        const expected_hi = reg.addAdj(@intCast((test_value & 0xFF00) >> 8), unsigned_e);
        const expected = (@as(u16, expected_hi) << 8) | expected_lo;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load HL from adjusted SP (e={d})", .{e});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                unsigned_e,
                0xFD,
            },
            TestCpuState.init()
                .rSp(test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rSp(test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rSp(test_value),
                TestCpuState.init() // execute iut: read e(PC) from ram
                    .rPc(0x0003)
                    .rSp(test_value),
                TestCpuState.init() // execute iut: add e to SP_lsb
                    .rPc(0x0003)
                    .fC(reg.lo.c)
                    .fH(reg.lo.h)
                    .rSp(test_value)
                    .rL(expected_lo),
                TestCpuState.init() // read (PC) | execute iut: add carry to SP_msb
                    .rPc(0x0004)
                    .fC(reg.lo.c)
                    .fH(reg.lo.h)
                    .rSp(test_value)
                    .rHL(expected),
            },
        );
    }
}

test "Add (register)" {
    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            for (0..(0b111 + 1)) |reg| {
                if (reg == 0b110 or reg == 0b111) {
                    continue;
                }

                // Constants
                const reg_u8: u8 = @intCast(reg);
                const reg_u3: u3 = @intCast(reg_u8);
                const val_u8: u8 = @intCast(val);

                const carry_flag: u1 = @intCast(with_carry & 0b01);
                const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

                const instr: u8 = 0b10000000 | reg_u8 | (@as(u8, carry_instr) << 3);
                const test_val: u8 = 0xBB;

                var res = alu.AluRegister{
                    .hi = test_val,
                    .lo = alu.RegisterFlags{
                        .c = carry_flag,
                        .h = 0,
                        .n = 0,
                        .z = 0,
                        .rest = 0,
                    },
                };
                res.add(val_u8, carry_instr);

                const name = try std.fmt.allocPrint(std.testing.allocator, "Add (reg={b}) (val={x}) (with_carry={b})", .{ reg, val, with_carry });
                defer std.testing.allocator.free(name);
                try runTestCase(
                    name,
                    &[_]u8{
                        0x00,
                        instr,
                        0xFD,
                    },
                    TestCpuState.init()
                        .rA(test_val)
                        .fC(carry_flag)
                        .reg(reg_u3, val_u8),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPc(0x0001)
                            .rA(test_val)
                            .fC(carry_flag)
                            .reg(reg_u3, val_u8),
                        TestCpuState.init() // execute nop | read iut(PC) from ram
                            .rPc(0x0002)
                            .rA(test_val)
                            .fC(carry_flag)
                            .reg(reg_u3, val_u8),
                        TestCpuState.init() // execute iut: add reg to a | read (PC)
                            .rPc(0x0003)
                            .reg(reg_u3, val_u8)
                            .rAf(res.all()),
                    },
                );
            }
        }
    }
}

test "Add (register A)" {
    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const instr: u8 = 0b10000111 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .hi = val_u8,
                .lo = alu.RegisterFlags{
                    .c = carry_flag,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.add(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Add (reg=111) (val={x}) (with_carry={b})", .{ val, with_carry });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rA(val_u8)
                    .fC(carry_flag),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rA(val_u8)
                        .fC(carry_flag),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rA(val_u8)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: add A to itself | read (PC)
                        .rPc(0x0003)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "Add (indirect HL)" {
    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const test_val: u8 = 0xAA;
            const test_addr: u16 = 0xD00D;
            const instr: u8 = 0b10000110 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .hi = test_val,
                .lo = alu.RegisterFlags{
                    .c = carry_flag,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.add(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Add (indirect HL) (val={x}) (with_carry={b})", .{ val, with_carry });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rA(test_val)
                    .fC(carry_flag)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute iut: read val(HL) from ram
                        .rPc(0x0002)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute iut: add val to A | read (PC)
                        .rPc(0x0003)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "Add (immediate)" {
    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const test_val: u8 = 0xAA;
            const instr: u8 = 0b11000110 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .hi = test_val,
                .lo = alu.RegisterFlags{
                    .c = carry_flag,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.add(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Add (immediate) (val={x}) (with_carry={b})", .{ val, with_carry });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    val_u8,
                    0xFD,
                },
                TestCpuState.init()
                    .rA(test_val)
                    .fC(carry_flag),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: read val(PC) from ram
                        .rPc(0x0003)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: add val to A | read (PC)
                        .rPc(0x0004)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "Sub (register)" {
    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0xC, 0xF, 0xCC, 0xFF }) |val| {
            for (0..(0b111 + 1)) |reg| {
                if (reg == 0b110 or reg == 0b111) {
                    continue;
                }

                // Constants
                const reg_u8: u8 = @intCast(reg);
                const reg_u3: u3 = @intCast(reg_u8);
                const val_u8: u8 = @intCast(val);

                const carry_flag: u1 = @intCast(with_carry & 0b01);
                const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

                const instr: u8 = 0b10010000 | reg_u8 | (@as(u8, carry_instr) << 3);
                const test_val: u8 = 0xBB;

                var res = alu.AluRegister{
                    .hi = test_val,
                    .lo = alu.RegisterFlags{
                        .c = carry_flag,
                        .h = 0,
                        .n = 0,
                        .z = 0,
                        .rest = 0,
                    },
                };
                res.sub(val_u8, carry_instr);

                const name = try std.fmt.allocPrint(std.testing.allocator, "Sub (reg={b}) (val={x}) (with_carry={b})", .{ reg, val, with_carry });
                defer std.testing.allocator.free(name);
                try runTestCase(
                    name,
                    &[_]u8{
                        0x00,
                        instr,
                        0xFD,
                    },
                    TestCpuState.init()
                        .rA(test_val)
                        .fC(carry_flag)
                        .reg(reg_u3, val_u8),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPc(0x0001)
                            .rA(test_val)
                            .fC(carry_flag)
                            .reg(reg_u3, val_u8),
                        TestCpuState.init() // execute nop | read iut(PC) from ram
                            .rPc(0x0002)
                            .rA(test_val)
                            .fC(carry_flag)
                            .reg(reg_u3, val_u8),
                        TestCpuState.init() // execute iut: sub reg from a | read (PC)
                            .rPc(0x0003)
                            .reg(reg_u3, val_u8)
                            .rAf(res.all()),
                    },
                );
            }
        }
    }
}

test "Sub (register A)" {
    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0xC, 0xF, 0xCC, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const instr: u8 = 0b10010111 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .hi = val_u8,
                .lo = alu.RegisterFlags{
                    .c = carry_flag,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.sub(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Sub (reg=111) (val={x}) (with_carry={b})", .{ val, with_carry });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rA(val_u8)
                    .fC(carry_flag),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rA(val_u8)
                        .fC(carry_flag),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rA(val_u8)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: sub A from itself | read (PC)
                        .rPc(0x0003)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "Sub (indirect HL)" {
    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const test_val: u8 = 0xAA;
            const test_addr: u16 = 0xD00D;
            const instr: u8 = 0b10010110 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .hi = test_val,
                .lo = alu.RegisterFlags{
                    .c = carry_flag,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.sub(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Sub (indirect HL) (val={x}) (with_carry={b})", .{ val, with_carry });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rA(test_val)
                    .fC(carry_flag)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute iut: read val(HL) from ram
                        .rPc(0x0002)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute iut: sub val from A | read (PC)
                        .rPc(0x0003)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "Sub (immediate)" {
    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const test_val: u8 = 0xAA;
            const instr: u8 = 0b11010110 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .hi = test_val,
                .lo = alu.RegisterFlags{
                    .c = carry_flag,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.sub(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Sub (immediate) (val={x}) (with_carry={b})", .{ val, with_carry });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    val_u8,
                    0xFD,
                },
                TestCpuState.init()
                    .rA(test_val)
                    .fC(carry_flag),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: read val(PC) from ram
                        .rPc(0x0003)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: sub val from A | read (PC)
                        .rPc(0x0004)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "CP (register)" {
    inline for (.{ 0x00, 0x01, 0xC, 0xF, 0xCC, 0xFF }) |val| {
        for (0..(0b111 + 1)) |reg| {
            if (reg == 0b110 or reg == 0b111) {
                continue;
            }

            // Constants
            const reg_u8: u8 = @intCast(reg);
            const reg_u3: u3 = @intCast(reg_u8);
            const val_u8: u8 = @intCast(val);

            const instr: u8 = 0b10111000 | reg_u8;
            const test_val: u8 = 0xBB;

            var res = alu.AluRegister{
                .hi = test_val,
                .lo = alu.RegisterFlags{
                    .c = 0,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.sub(val_u8, 0);

            const name = try std.fmt.allocPrint(std.testing.allocator, "CP (reg={b}) (val={x})", .{ reg, val });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rA(test_val)
                    .reg(reg_u3, val_u8),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rA(test_val)
                        .reg(reg_u3, val_u8),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rA(test_val)
                        .reg(reg_u3, val_u8),
                    TestCpuState.init() // execute iut: sub reg from a, store flags only | read (PC)
                        .rPc(0x0003)
                        .reg(reg_u3, val_u8)
                        .rA(test_val)
                        .rF(res.lo.all()),
                },
            );
        }
    }
}

test "CP (register A)" {
    inline for (.{ 0x00, 0x01, 0xC, 0xF, 0xCC, 0xFF }) |val| {
        // Constants
        const val_u8: u8 = @intCast(val);

        const instr: u8 = 0b10111111;

        var res = alu.AluRegister{
            .hi = val_u8,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.sub(val_u8, 0);

        const name = try std.fmt.allocPrint(std.testing.allocator, "CP (reg=111) (val={x})", .{val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rA(val_u8),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(val_u8),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(val_u8),
                TestCpuState.init() // execute iut: sub A from itself, store flags only | read (PC)
                    .rPc(0x0003)
                    .rA(val_u8)
                    .rF(res.lo.all()),
            },
        );
    }
}

test "CP (indirect HL)" {
    inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
        // Constants
        const val_u8: u8 = @intCast(val);

        const test_val: u8 = 0xAA;
        const test_addr: u16 = 0xD00D;
        const instr: u8 = 0b10111110;

        var res = alu.AluRegister{
            .hi = test_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.sub(val_u8, 0);

        const name = try std.fmt.allocPrint(std.testing.allocator, "CP (indirect HL) (val={x})", .{
            val,
        });
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rA(test_val)
                .ram(test_addr, val_u8)
                .rHL(test_addr),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(test_val)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(test_val)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                TestCpuState.init() // execute iut: read val(HL) from ram
                    .rPc(0x0002)
                    .rA(test_val)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                TestCpuState.init() // execute iut: sub val from A, store flags only | read (PC)
                    .rPc(0x0003)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr)
                    .rA(test_val)
                    .rF(res.lo.all()),
            },
        );
    }
}

test "CP (immediate)" {
    inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
        // Constants
        const val_u8: u8 = @intCast(val);

        const test_val: u8 = 0xAA;
        const instr: u8 = 0b11111110;

        var res = alu.AluRegister{
            .hi = test_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.sub(val_u8, 0);

        const name = try std.fmt.allocPrint(std.testing.allocator, "CP (immediate) (val={x})", .{val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                val_u8,
                0xFD,
            },
            TestCpuState.init()
                .rA(test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(test_val),
                TestCpuState.init() // execute iut: read val(PC) from ram
                    .rPc(0x0003)
                    .rA(test_val),
                TestCpuState.init() // execute iut: sub val from A, store flags only | read (PC)
                    .rPc(0x0004)
                    .rA(test_val)
                    .rF(res.lo.all()),
            },
        );
    }
}

test "INC/DEC Register" {
    inline for (.{ 0, 1 }) |incdec| {
        inline for (.{ 0x00, 0xFF, 0xF0, 0x0F, 0xAA }) |test_val| {
            inline for (0..(0b111 + 1)) |reg| {
                if (reg == 0b110) {
                    continue;
                }

                // Constants
                const instr: u8 = 0b00_000_10_0 | (reg << 3) | incdec;

                var rreg = alu.AluRegister{
                    .hi = test_val,
                    .lo = alu.RegisterFlags{
                        .c = 0,
                        .h = 0,
                        .n = 0,
                        .z = 0,
                        .rest = 0,
                    },
                };
                const res = if (incdec == 0)
                    rreg.inc(test_val)
                else
                    rreg.dec(test_val);

                const name = try std.fmt.allocPrint(std.testing.allocator, "INC/DEC (inc/dec={b}) (test_val={x}) (reg={b})", .{ incdec, test_val, reg });
                defer std.testing.allocator.free(name);
                try runTestCase(
                    name,
                    &[_]u8{
                        0x00,
                        instr,
                        0xFD,
                    },
                    TestCpuState.init()
                        .reg(reg, test_val),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPc(0x0001)
                            .reg(reg, test_val),
                        TestCpuState.init() // execute nop | read iut(PC) from ram
                            .rPc(0x0002)
                            .reg(reg, test_val),
                        TestCpuState.init() // execute iut: inc/dec reg | read (PC)
                            .rPc(0x0003)
                            .reg(reg, res)
                            .rF(rreg.lo.all()),
                    },
                );
            }
        }
    }
}

test "INC/DEC Indirect" {
    inline for (.{ 0, 1 }) |incdec| {
        inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
            // Constants
            const instr: u8 = 0b00_110_10_0 | incdec;
            const test_addr = 0xD00D;

            var reg = alu.AluRegister{
                .hi = test_val,
                .lo = alu.RegisterFlags{
                    .c = 0,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            const res = if (incdec == 0)
                reg.inc(test_val)
            else
                reg.dec(test_val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "INC/DEC Indirect (inc/dec={b}) (test_val={x})", .{ incdec, test_val });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rHL(test_addr)
                        .ram(test_addr, test_val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rHL(test_addr)
                        .ram(test_addr, test_val),
                    TestCpuState.init() // execute iut: read val(HL) from ram
                        .rPc(0x0002)
                        .rHL(test_addr)
                        .ram(test_addr, test_val),
                    TestCpuState.init() // execute iut: inc/dec val | write val(HL) to ram
                        .rPc(0x0002)
                        .rHL(test_addr)
                        .ram(test_addr, res)
                        .rF(reg.lo.all()),
                    TestCpuState.init() // read (PC)
                        .rPc(0x0003)
                        .rHL(test_addr)
                        .ram(test_addr, res)
                        .rF(reg.lo.all()),
                },
            );
        }
    }
}

test "AND register" {
    inline for (0..(0b111 + 1)) |reg| {
        if (reg == 0b110 or reg == 0b111) {
            continue;
        }
        inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
            // Constants
            const instr: u8 = 0b10100_000 | reg;

            const reg_val = 0xAA;
            var res = alu.AluRegister{
                .hi = reg_val,
                .lo = alu.RegisterFlags{
                    .c = 0,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.and_(test_val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "AND register (reg={b}) (test_val={x})", .{ reg, test_val });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .reg(reg, reg_val)
                    .rA(test_val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute iut: and reg to A | read (PC) from ram
                        .rPc(0x0003)
                        .reg(reg, reg_val)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "AND register A" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10100_111;

        var res = alu.AluRegister{
            .hi = test_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.and_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "AND register A (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rA(test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(test_val),
                TestCpuState.init() // execute iut: and reg to A | read (PC) from ram
                    .rPc(0x0003)
                    .rAf(res.all()),
            },
        );
    }
}

test "AND indirect HL" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10100_110;

        const test_addr = 0xD00D;
        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .hi = reg_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.and_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "AND indirect HL (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rA(reg_val)
                .rHL(test_addr)
                .ram(test_addr, test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: read val from ram(HL)
                    .rPc(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: and val to A | read (PC) from ram
                    .rPc(0x0003)
                    .rAf(res.all())
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
            },
        );
    }
}

test "AND immediate" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b11100110;

        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .hi = reg_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.and_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "AND immediate (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                test_val,
                0xFD,
            },
            TestCpuState.init()
                .rA(reg_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(reg_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: read val from ram(PC)
                    .rPc(0x0003)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: and val to A | read (PC) from ram
                    .rPc(0x0004)
                    .rAf(res.all()),
            },
        );
    }
}

test "OR register" {
    inline for (0..(0b111 + 1)) |reg| {
        if (reg == 0b110 or reg == 0b111) {
            continue;
        }
        inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
            // Constants
            const instr: u8 = 0b10110_000 | reg;

            const reg_val = 0xAA;
            var res = alu.AluRegister{
                .hi = reg_val,
                .lo = alu.RegisterFlags{
                    .c = 0,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.or_(test_val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "OR register (reg={b}) (test_val={x})", .{ reg, test_val });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .reg(reg, reg_val)
                    .rA(test_val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute iut: or reg to A | read (PC) from ram
                        .rPc(0x0003)
                        .reg(reg, reg_val)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "OR register A" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10110_111;

        var res = alu.AluRegister{
            .hi = test_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.or_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "OR register A (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rA(test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(test_val),
                TestCpuState.init() // execute iut: or reg to A | read (PC) from ram
                    .rPc(0x0003)
                    .rAf(res.all()),
            },
        );
    }
}

test "OR indirect HL" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10110_110;

        const test_addr = 0xD00D;
        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .hi = reg_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.or_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "OR indirect HL (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rA(reg_val)
                .rHL(test_addr)
                .ram(test_addr, test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: read val from ram(HL)
                    .rPc(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: or val to A | read (PC) from ram
                    .rPc(0x0003)
                    .rAf(res.all())
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
            },
        );
    }
}

test "OR immediate" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b11110110;

        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .hi = reg_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.or_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "OR immediate (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                test_val,
                0xFD,
            },
            TestCpuState.init()
                .rA(reg_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(reg_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: read val from ram(PC)
                    .rPc(0x0003)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: or val to A | read (PC) from ram
                    .rPc(0x0004)
                    .rAf(res.all()),
            },
        );
    }
}

test "XOR register" {
    inline for (0..(0b111 + 1)) |reg| {
        if (reg == 0b110 or reg == 0b111) {
            continue;
        }
        inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
            // Constants
            const instr: u8 = 0b10101_000 | reg;

            const reg_val = 0xAA;
            var res = alu.AluRegister{
                .hi = reg_val,
                .lo = alu.RegisterFlags{
                    .c = 0,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            res.xor(test_val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "XOR register (reg={b}) (test_val={x})", .{ reg, test_val });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .reg(reg, reg_val)
                    .rA(test_val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute iut: xor reg to A | read (PC) from ram
                        .rPc(0x0003)
                        .reg(reg, reg_val)
                        .rAf(res.all()),
                },
            );
        }
    }
}

test "XOR register A" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10101_111;

        var res = alu.AluRegister{
            .hi = test_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.xor(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "XOR register A (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rA(test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(test_val),
                TestCpuState.init() // execute iut: xor reg to A | read (PC) from ram
                    .rPc(0x0003)
                    .rAf(res.all()),
            },
        );
    }
}

test "XOR indirect HL" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10101_110;

        const test_addr = 0xD00D;
        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .hi = reg_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.xor(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "XOR indirect HL (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rA(reg_val)
                .rHL(test_addr)
                .ram(test_addr, test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: read val from ram(HL)
                    .rPc(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: xor val to A | read (PC) from ram
                    .rPc(0x0003)
                    .rAf(res.all())
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
            },
        );
    }
}

test "XOR immediate" {
    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b11101110;

        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .hi = reg_val,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        res.xor(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "XOR immediate (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                test_val,
                0xFD,
            },
            TestCpuState.init()
                .rA(reg_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rA(reg_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: read val from ram(PC)
                    .rPc(0x0003)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: xor val to A | read (PC) from ram
                    .rPc(0x0004)
                    .rAf(res.all()),
            },
        );
    }
}

test "Complement carry flag" {
    inline for (0..2) |carry| {
        inline for (0..2) |halfcarry| {
            inline for (0..2) |subtract| {
                inline for (0..2) |zero| {
                    // Constants
                    const instr: u8 = 0b00111111;

                    const name = try std.fmt.allocPrint(std.testing.allocator, "Complement carry flag (C={b}) (H={b}) (N={b})", .{ carry, halfcarry, subtract });
                    defer std.testing.allocator.free(name);
                    try runTestCase(
                        name,
                        &[_]u8{
                            0x00,
                            instr,
                            0xFD,
                        },
                        TestCpuState.init()
                            .fC(carry)
                            .fH(halfcarry)
                            .fN(subtract)
                            .fZ(zero),
                        &[_]*TestCpuState{
                            TestCpuState.init() // read nop(PC) from ram
                                .rPc(0x0001)
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero),
                            TestCpuState.init() // execute nop | read iut(PC) from ram
                                .rPc(0x0002)
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero),
                            TestCpuState.init() // execute iut: set flags | read (PC) from ram
                                .rPc(0x0003)
                                .fC(~@as(u1, carry))
                                .fH(0)
                                .fN(0)
                                .fZ(zero),
                        },
                    );
                }
            }
        }
    }
}

test "Set carry flag" {
    inline for (0..2) |carry| {
        inline for (0..2) |halfcarry| {
            inline for (0..2) |subtract| {
                inline for (0..2) |zero| {
                    // Constants
                    const instr: u8 = 0b00110111;

                    const name = try std.fmt.allocPrint(std.testing.allocator, "Set carry flag (C={b}) (H={b}) (N={b})", .{ carry, halfcarry, subtract });
                    defer std.testing.allocator.free(name);
                    try runTestCase(
                        name,
                        &[_]u8{
                            0x00,
                            instr,
                            0xFD,
                        },
                        TestCpuState.init()
                            .fC(carry)
                            .fH(halfcarry)
                            .fN(subtract)
                            .fZ(zero),
                        &[_]*TestCpuState{
                            TestCpuState.init() // read nop(PC) from ram
                                .rPc(0x0001)
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero),
                            TestCpuState.init() // execute nop | read iut(PC) from ram
                                .rPc(0x0002)
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero),
                            TestCpuState.init() // execute iut: set flags | read (PC) from ram
                                .rPc(0x0003)
                                .fC(1)
                                .fH(0)
                                .fN(0)
                                .fZ(zero),
                        },
                    );
                }
            }
        }
    }
}

test "Decimal adjust accumulator" {
    // Constants
    const instr: u8 = 0b00100111;

    const test_value: u8 = 0x2F;
    const test_carry: u1 = 0;
    const test_halfcarry: u1 = 1;
    const test_subtraction: u1 = 1;

    var res = alu.AluRegister{
        .hi = test_value,
        .lo = alu.RegisterFlags{
            .c = test_carry,
            .h = test_halfcarry,
            .n = test_subtraction,
            .z = 0,
            .rest = 0,
        },
    };
    res.daa();

    try runTestCase(
        "Decimal adjust accumulator",
        &[_]u8{
            0x00,
            instr,
            0xFD,
        },
        TestCpuState.init()
            .rA(test_value)
            .fC(test_carry)
            .fH(test_halfcarry)
            .fN(test_subtraction),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rA(test_value)
                .fC(test_carry)
                .fH(test_halfcarry)
                .fN(test_subtraction),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rA(test_value)
                .fC(test_carry)
                .fH(test_halfcarry)
                .fN(test_subtraction),
            TestCpuState.init() // execute iut: set flags | read (PC) from ram
                .rPc(0x0003)
                .rAf(res.all()),
        },
    );
}

test "Complement accumulator" {
    // Constants
    const instr: u8 = 0b00101111;
    const test_value = 0xAA;

    var res = alu.AluRegister{
        .hi = test_value,
        .lo = alu.RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    res.cpl();

    try runTestCase(
        "Complement accumulator",
        &[_]u8{
            0x00,
            instr,
            0xFD,
        },
        TestCpuState.init()
            .rA(test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rA(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rA(test_value),
            TestCpuState.init() // execute iut: complement A | read (PC) from ram
                .rPc(0x0003)
                .rAf(res.all()),
        },
    );
}

test "Inc/Dec register 16" {
    inline for (.{ 0b0, 0b1 }) |incdec| {
        inline for (.{ 0xFFFF, 0x0FFF, 0x00FF, 0x000F, 0x0000 }) |val| {
            for (0..(0b11 + 1)) |reg| {
                // Constants
                const instr: u8 = @intCast(0b00_00_0_011 | (reg << 4) | (incdec << 3));
                const value: u16 = val;

                const expected: u16, _ = if (incdec == 0)
                    @addWithOverflow(value, 1)
                else
                    @subWithOverflow(value, 1);

                const name = try std.fmt.allocPrint(std.testing.allocator, "Increment register 16 (reg={b}) (val={x}) (incdec={b})", .{ reg, value, incdec });
                defer std.testing.allocator.free(name);
                try runTestCase(
                    name,
                    &[_]u8{
                        0x00,
                        instr,
                        0xFD,
                    },
                    TestCpuState.init()
                        .reg16(@intCast(reg), value),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPc(0x0001)
                            .reg16(@intCast(reg), value),
                        TestCpuState.init() // execute nop | read iut(PC) from ram
                            .rPc(0x0002)
                            .reg16(@intCast(reg), value),
                        TestCpuState.init() // execute iut: inc/dec register
                            .rPc(0x0002)
                            .reg16(@intCast(reg), expected),
                        TestCpuState.init() // read (PC) from ram
                            .rPc(0x0003)
                            .reg16(@intCast(reg), expected),
                    },
                );
            }
        }
    }
}

test "Add register 16" {
    inline for (.{ 0xFFFF, 0x0F0F, 0xF0F0, 0x000F, 0x0F00, 0x0000, 0x0101, 0xF1F1 }) |val| {
        inline for (.{ 0b0, 0b1 }) |carry| {
            inline for (.{ 0b0, 0b1 }) |zero| {
                inline for (.{ 0b0, 0b1 }) |halfcarry| {
                    inline for (.{ 0b0, 0b1 }) |subtract| {
                        for (0..(0b11 + 1)) |reg| {
                            if (reg == 0b10) {
                                continue;
                            }
                            // Constants
                            const instr: u8 = @intCast(0b00_00_1001 | (reg << 4));
                            const value: u16 = val;
                            const base: u16 = 0x0F0F;

                            const base_lo: u8 = @intCast(base & 0xFF);
                            const base_hi: u8 = @intCast((base & 0xFF00) >> 8);

                            const value_lo: u8 = @intCast(value & 0xFF);
                            const value_hi: u8 = @intCast((value & 0xFF00) >> 8);

                            var res_1 = alu.AluRegister{
                                .hi = 0,
                                .lo = alu.RegisterFlags{
                                    .c = carry,
                                    .h = halfcarry,
                                    .n = subtract,
                                    .z = zero,
                                    .rest = 0,
                                },
                            };
                            const expected_lo = res_1.addReturn(base_lo, value_lo, 0, zero);
                            var res_2 = res_1;
                            const expected_hi = res_2.addReturn(base_hi, value_hi, 1, zero);

                            const expected: u16 = (@as(u16, expected_hi) << 8) | expected_lo;

                            const name = try std.fmt.allocPrint(std.testing.allocator, "Add register 16 (val={x}) (C={b}) (Z={b}) (H={b}) (N={b})", .{ val, carry, zero, halfcarry, subtract });
                            defer std.testing.allocator.free(name);
                            try runTestCase(
                                name,
                                &[_]u8{
                                    0x00,
                                    instr,
                                    0xFD,
                                },
                                TestCpuState.init()
                                    .fC(carry)
                                    .fH(halfcarry)
                                    .fN(subtract)
                                    .fZ(zero)
                                    .rHL(base)
                                    .reg16(@intCast(reg), value),
                                &[_]*TestCpuState{
                                    TestCpuState.init() // read nop(PC) from ram
                                        .rPc(0x0001)
                                        .fC(carry)
                                        .fH(halfcarry)
                                        .fN(subtract)
                                        .fZ(zero)
                                        .rHL(base)
                                        .reg16(@intCast(reg), value),
                                    TestCpuState.init() // execute nop | read iut(PC) from ram
                                        .rPc(0x0002)
                                        .fC(carry)
                                        .fH(halfcarry)
                                        .fN(subtract)
                                        .fZ(zero)
                                        .rHL(base)
                                        .reg16(@intCast(reg), value),
                                    TestCpuState.init() // execute iut: add lsb
                                        .rPc(0x0002)
                                        .fC(res_1.lo.c)
                                        .fH(res_1.lo.h)
                                        .fN(res_1.lo.n)
                                        .fZ(res_1.lo.z)
                                        .rH(base_hi)
                                        .rL(expected_lo)
                                        .reg16(@intCast(reg), value),
                                    TestCpuState.init() // execute iut: add msb | read (PC) from ram
                                        .rPc(0x0003)
                                        .fC(res_2.lo.c)
                                        .fH(res_2.lo.h)
                                        .fN(res_2.lo.n)
                                        .fZ(res_2.lo.z)
                                        .rHL(expected)
                                        .reg16(@intCast(reg), value),
                                },
                            );
                        }
                    }
                }
            }
        }
    }
}

test "Add register HL" {
    inline for (.{ 0xFFFF, 0x0F0F, 0xF0F0, 0x000F, 0x0F00, 0x0000, 0x0101, 0xF1F1 }) |val| {
        inline for (.{ 0b0, 0b1 }) |carry| {
            inline for (.{ 0b0, 0b1 }) |zero| {
                inline for (.{ 0b0, 0b1 }) |halfcarry| {
                    inline for (.{ 0b0, 0b1 }) |subtract| {
                        // Constants
                        const instr: u8 = 0b00_10_1001;

                        const value: u16 = val;

                        const value_lo: u8 = @intCast(value & 0xFF);
                        const value_hi: u8 = @intCast((value & 0xFF00) >> 8);

                        var res_1 = alu.AluRegister{
                            .hi = 0,
                            .lo = alu.RegisterFlags{
                                .c = carry,
                                .h = halfcarry,
                                .n = subtract,
                                .z = zero,
                                .rest = 0,
                            },
                        };
                        const expected_lo = res_1.addReturn(value_lo, value_lo, 0, zero);
                        var res_2 = res_1;
                        const expected_hi = res_2.addReturn(value_hi, value_hi, 1, zero);

                        const expected: u16 = (@as(u16, expected_hi) << 8) | expected_lo;

                        const name = try std.fmt.allocPrint(std.testing.allocator, "Add register HL (val={x}) (C={b}) (Z={b}) (H={b}) (N={b})", .{ val, carry, zero, halfcarry, subtract });
                        defer std.testing.allocator.free(name);
                        try runTestCase(
                            name,
                            &[_]u8{
                                0x00,
                                instr,
                                0xFD,
                            },
                            TestCpuState.init()
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero)
                                .rHL(value),
                            &[_]*TestCpuState{
                                TestCpuState.init() // read nop(PC) from ram
                                    .rPc(0x0001)
                                    .fC(carry)
                                    .fH(halfcarry)
                                    .fN(subtract)
                                    .fZ(zero)
                                    .rHL(value),
                                TestCpuState.init() // execute nop | read iut(PC) from ram
                                    .rPc(0x0002)
                                    .fC(carry)
                                    .fH(halfcarry)
                                    .fN(subtract)
                                    .fZ(zero)
                                    .rHL(value),
                                TestCpuState.init() // execute iut: add lsb
                                    .rPc(0x0002)
                                    .fC(res_1.lo.c)
                                    .fH(res_1.lo.h)
                                    .fN(res_1.lo.n)
                                    .fZ(res_1.lo.z)
                                    .rH(value_hi)
                                    .rL(expected_lo),
                                TestCpuState.init() // execute iut: add msb | read (PC) from ram
                                    .rPc(0x0003)
                                    .fC(res_2.lo.c)
                                    .fH(res_2.lo.h)
                                    .fN(res_2.lo.n)
                                    .fZ(res_2.lo.z)
                                    .rHL(expected),
                            },
                        );
                    }
                }
            }
        }
    }
}

test "Add SP relative" {
    inline for (.{ 0x00, 0x01, 0x04, 0x0F, 0xF4, 0xFF }) |e| {
        // Constants
        const instr: u8 = 0b11101000;
        const test_value: u16 = 0x100D;

        const unsigned_e: u8 = @intCast(e);

        var reg = alu.AluRegister{
            .hi = 0x00,
            .lo = alu.RegisterFlags{
                .c = 0,
                .h = 0,
                .n = 0,
                .z = 0,
                .rest = 0,
            },
        };
        const expected_lo = reg.addReturn(@intCast(test_value & 0xFF), unsigned_e, 0, 0);
        const expected_hi = reg.addAdj(@intCast((test_value & 0xFF00) >> 8), unsigned_e);
        const expected = (@as(u16, expected_hi) << 8) | expected_lo;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Add SP relative (e={d})", .{e});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                unsigned_e,
                0xFD,
            },
            TestCpuState.init()
                .rSp(test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rSp(test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rSp(test_value),
                TestCpuState.init() // execute iut: read e(PC) from ram
                    .rPc(0x0003)
                    .rSp(test_value),
                TestCpuState.init() // execute iut: add e to SP_lsb
                    .rPc(0x0003)
                    .fC(reg.lo.c)
                    .fH(reg.lo.h)
                    .rSp(test_value),
                TestCpuState.init() // execute iut: add carry/adj to SP_msb
                    .rPc(0x0003)
                    .fC(reg.lo.c)
                    .fH(reg.lo.h)
                    .rSp(test_value),
                TestCpuState.init() // execute iut: assign WZ to SP | read (PC) from ram
                    .rPc(0x0004)
                    .fC(reg.lo.c)
                    .fH(reg.lo.h)
                    .rSp(expected),
            },
        );
    }
}

test "rotate accumulator" {
    inline for (.{ 0b00, 0b01, 0b10, 0b11 }) |op| {
        const aluop: fn (*alu.AluRegister, u8) u8 = switch (op) {
            0b00 => alu.AluRegister.rlc,
            0b01 => alu.AluRegister.rrc,
            0b10 => alu.AluRegister.rl,
            0b11 => alu.AluRegister.rr,
            else => unreachable,
        };
        inline for (.{ 0xA5, 0x00, 0xFF }) |val| {
            const instr = 0b000_00_111 | (@as(u8, op) << 3);

            var reg = alu.AluRegister{
                .hi = 0,
                .lo = alu.RegisterFlags{
                    .c = 0,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            const res = aluop(&reg, val);
            reg.lo.z = 0;

            const name = try std.fmt.allocPrint(std.testing.allocator, "rotate accumulator ({d}) (op={b})", .{ val, op });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rA(val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rA(val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPc(0x0002)
                        .rA(val),
                    TestCpuState.init() // execute iut: operate on reg | read (PC) from ram
                        .rPc(0x0003)
                        .rA(res)
                        .fC(reg.lo.c)
                        .fH(reg.lo.h)
                        .fN(reg.lo.n)
                        .fZ(reg.lo.z),
                },
            );
        }
    }
}

test "rotate/swap register" {
    inline for (.{ 0b000, 0b001, 0b010, 0b011, 0b100, 0b101, 0b110, 0b111 }) |op| {
        const aluop: fn (*alu.AluRegister, u8) u8 = switch (op) {
            0b000 => alu.AluRegister.rlc,
            0b001 => alu.AluRegister.rrc,
            0b010 => alu.AluRegister.rl,
            0b011 => alu.AluRegister.rr,
            0b100 => alu.AluRegister.sla,
            0b101 => alu.AluRegister.sra,
            0b110 => alu.AluRegister.swap,
            0b111 => alu.AluRegister.srl,
            else => unreachable,
        };
        inline for (.{ 0b000, 0b001, 0b010, 0b011, 0b100, 0b101, 0b111 }) |regIdx| {
            inline for (.{ 0xA5, 0x00, 0xFF }) |val| {
                const instr = 0b00_000_000 | (@as(u8, op) << 3) | regIdx;

                var reg = alu.AluRegister{
                    .hi = 0,
                    .lo = alu.RegisterFlags{
                        .c = 0,
                        .h = 0,
                        .n = 0,
                        .z = 0,
                        .rest = 0,
                    },
                };
                const res = aluop(&reg, val);

                const name = try std.fmt.allocPrint(std.testing.allocator, "rotate/swap register ({d}) (op={b}) (reg={b})", .{ val, op, regIdx });
                defer std.testing.allocator.free(name);
                try runTestCase(
                    name,
                    &[_]u8{
                        0x00,
                        0xCB,
                        instr,
                        0xFD,
                    },
                    TestCpuState.init()
                        .reg(regIdx, val),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPc(0x0001)
                            .reg(regIdx, val),
                        TestCpuState.init() // execute nop | read prefix(PC) from ram
                            .rPc(0x0002)
                            .reg(regIdx, val),
                        TestCpuState.init() // read iut(PC) from ram
                            .rPc(0x0003)
                            .reg(regIdx, val),
                        TestCpuState.init() // execute iut: operate on reg | read (PC) from ram
                            .rPc(0x0004)
                            .reg(regIdx, res)
                            .fC(reg.lo.c)
                            .fH(reg.lo.h)
                            .fN(reg.lo.n)
                            .fZ(reg.lo.z),
                    },
                );
            }
        }
    }
}

test "rotate/swap HL indirect" {
    inline for (.{ 0b000, 0b001, 0b010, 0b011, 0b100, 0b101, 0b110, 0b111 }) |op| {
        const aluop: fn (*alu.AluRegister, u8) u8 = switch (op) {
            0b000 => alu.AluRegister.rlc,
            0b001 => alu.AluRegister.rrc,
            0b010 => alu.AluRegister.rl,
            0b011 => alu.AluRegister.rr,
            0b100 => alu.AluRegister.sla,
            0b101 => alu.AluRegister.sra,
            0b110 => alu.AluRegister.swap,
            0b111 => alu.AluRegister.srl,
            else => unreachable,
        };
        inline for (.{ 0xA5, 0x00, 0xFF }) |val| {
            const addr = 0xD00D;
            const instr = 0b00_000_110 | (@as(u8, op) << 3);

            var reg = alu.AluRegister{
                .hi = 0,
                .lo = alu.RegisterFlags{
                    .c = 0,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            const res = aluop(&reg, val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "rotate/swap HL indirect ({d}) (op={b})", .{ val, op });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    0xCB,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rHL(addr)
                    .ram(addr, val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rHL(addr)
                        .ram(addr, val),
                    TestCpuState.init() // execute nop | read prefix(PC) from ram
                        .rPc(0x0002)
                        .rHL(addr)
                        .ram(addr, val),
                    TestCpuState.init() // read iut(PC) from ram
                        .rPc(0x0003)
                        .rHL(addr)
                        .ram(addr, val),
                    TestCpuState.init() // execute iut: read val(HL) from ram
                        .rPc(0x0003)
                        .rHL(addr)
                        .ram(addr, val),
                    TestCpuState.init() // execute iut: operate on val | write val to ram
                        .rPc(0x0003)
                        .rHL(addr)
                        .ram(addr, res)
                        .fC(reg.lo.c)
                        .fH(reg.lo.h)
                        .fN(reg.lo.n)
                        .fZ(reg.lo.z),
                    TestCpuState.init() // execute iut: read (PC)
                        .rPc(0x0004)
                        .rHL(addr)
                        .ram(addr, res)
                        .fC(reg.lo.c)
                        .fH(reg.lo.h)
                        .fN(reg.lo.n)
                        .fZ(reg.lo.z),
                },
            );
        }
    }
}

test "test bit register" {
    inline for (0..8) |regIdxS| {
        if (regIdxS == 0b110) {
            continue;
        }
        const regIdx: u3 = @intCast(regIdxS);
        inline for (0..8) |bitIdxS| {
            const bitIdx: u3 = @intCast(bitIdxS);
            inline for (.{ 0xA5, 0x00, 0xFF }) |val| {
                const instr = 0b01_000_000 | (@as(u8, bitIdx) << 3) | regIdx;

                var reg = alu.AluRegister{
                    .hi = 0,
                    .lo = alu.RegisterFlags{
                        .c = 0,
                        .h = 0,
                        .n = 0,
                        .z = 0,
                        .rest = 0,
                    },
                };
                reg.bit(val, bitIdx);

                const name = try std.fmt.allocPrint(std.testing.allocator, "test bit register ({d}) (bit={b}) (reg={b})", .{ val, bitIdx, regIdx });
                defer std.testing.allocator.free(name);
                try runTestCase(
                    name,
                    &[_]u8{
                        0x00,
                        0xCB,
                        instr,
                        0xFD,
                    },
                    TestCpuState.init()
                        .reg(regIdx, val),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPc(0x0001)
                            .reg(regIdx, val),
                        TestCpuState.init() // execute nop | read prefix(PC) from ram
                            .rPc(0x0002)
                            .reg(regIdx, val),
                        TestCpuState.init() // read iut(PC) from ram
                            .rPc(0x0003)
                            .reg(regIdx, val),
                        TestCpuState.init() // execute iut: operate on reg | read (PC) from ram
                            .rPc(0x0004)
                            .reg(regIdx, val)
                            .fC(reg.lo.c)
                            .fH(reg.lo.h)
                            .fN(reg.lo.n)
                            .fZ(reg.lo.z),
                    },
                );
            }
        }
    }
}

test "test bit HL" {
    inline for (0..8) |bitIdxS| {
        if (bitIdxS == 0b110) {
            continue;
        }
        const bitIdx: u3 = @intCast(bitIdxS);
        inline for (.{ 0xA5, 0x00, 0xFF }) |val| {
            const addr = 0xD00D;
            const instr = 0b01_000_110 | (@as(u8, bitIdx) << 3);

            var reg = alu.AluRegister{
                .hi = 0,
                .lo = alu.RegisterFlags{
                    .c = 0,
                    .h = 0,
                    .n = 0,
                    .z = 0,
                    .rest = 0,
                },
            };
            reg.bit(val, bitIdx);

            const name = try std.fmt.allocPrint(std.testing.allocator, "test bit register HL indirect ({d}) (bit={b})", .{ val, bitIdx });
            defer std.testing.allocator.free(name);
            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    0xCB,
                    instr,
                    0xFD,
                },
                TestCpuState.init()
                    .rHL(addr)
                    .ram(addr, val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPc(0x0001)
                        .rHL(addr)
                        .ram(addr, val),
                    TestCpuState.init() // execute nop | read prefix(PC) from ram
                        .rPc(0x0002)
                        .rHL(addr)
                        .ram(addr, val),
                    TestCpuState.init() // read iut(PC) from ram
                        .rPc(0x0003)
                        .rHL(addr)
                        .ram(addr, val),
                    TestCpuState.init() // execute iut: read val(HL) from ram
                        .rPc(0x0003)
                        .rHL(addr)
                        .ram(addr, val),
                    TestCpuState.init() // execute iut: operate on val | read(PC)
                        .rPc(0x0004)
                        .rHL(addr)
                        .ram(addr, val)
                        .fC(reg.lo.c)
                        .fH(reg.lo.h)
                        .fN(reg.lo.n)
                        .fZ(reg.lo.z),
                },
            );
        }
    }
}

test "set/reset bit register" {
    for (0..2) |setResetS| {
        const setReset: u1 = @intCast(setResetS);
        for (0..8) |regIdxS| {
            if (regIdxS == 0b110) {
                continue;
            }
            const regIdx: u3 = @intCast(regIdxS);
            for (0..8) |bitIdxS| {
                const bitIdx: u3 = @intCast(bitIdxS);
                inline for (.{ 0xA5, 0x00, 0xFF }) |val| {
                    const instr = 0b10_000_000 | (@as(u8, setReset) << 6) | (@as(u8, bitIdx) << 3) | regIdx;

                    const res = if (setReset == 0)
                        alu.AluRegister.res(val, bitIdx)
                    else
                        alu.AluRegister.set(val, bitIdx);

                    const name = try std.fmt.allocPrint(std.testing.allocator, "set/reset bit register ({d}) (bit={b}) (reg={b})", .{ val, bitIdx, regIdx });
                    defer std.testing.allocator.free(name);
                    try runTestCase(
                        name,
                        &[_]u8{
                            0x00,
                            0xCB,
                            instr,
                            0xFD,
                        },
                        TestCpuState.init()
                            .reg(regIdx, val),
                        &[_]*TestCpuState{
                            TestCpuState.init() // read nop(PC) from ram
                                .rPc(0x0001)
                                .reg(regIdx, val),
                            TestCpuState.init() // execute nop | read prefix(PC) from ram
                                .rPc(0x0002)
                                .reg(regIdx, val),
                            TestCpuState.init() // read iut(PC) from ram
                                .rPc(0x0003)
                                .reg(regIdx, val),
                            TestCpuState.init() // execute iut: operate on reg | read (PC) from ram
                                .rPc(0x0004)
                                .reg(regIdx, res),
                        },
                    );
                }
            }
        }
    }
}

test "set/reset bit HL" {
    for (0..2) |setResetS| {
        const setReset: u1 = @intCast(setResetS);
        for (0..8) |bitIdxS| {
            const bitIdx: u3 = @intCast(bitIdxS);
            inline for (.{ 0xA5, 0x00, 0xFF }) |val| {
                const addr = 0xD00D;
                const instr = 0b10_000_110 | (@as(u8, @intCast(setReset)) << 6) | (@as(u8, @intCast(bitIdx)) << 3);

                const res = if (setReset == 0)
                    alu.AluRegister.res(val, bitIdx)
                else
                    alu.AluRegister.set(val, bitIdx);

                const name = try std.fmt.allocPrint(std.testing.allocator, "set/reset bit register HL indirect ({d}) (bit={b})", .{ val, bitIdx });
                defer std.testing.allocator.free(name);
                try runTestCase(
                    name,
                    &[_]u8{
                        0x00,
                        0xCB,
                        instr,
                        0xFD,
                    },
                    TestCpuState.init()
                        .rHL(addr)
                        .ram(addr, val),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPc(0x0001)
                            .rHL(addr)
                            .ram(addr, val),
                        TestCpuState.init() // execute nop | read prefix(PC) from ram
                            .rPc(0x0002)
                            .rHL(addr)
                            .ram(addr, val),
                        TestCpuState.init() // read iut(PC) from ram
                            .rPc(0x0003)
                            .rHL(addr)
                            .ram(addr, val),
                        TestCpuState.init() // execute iut: read val(HL) from ram
                            .rPc(0x0003)
                            .rHL(addr)
                            .ram(addr, val),
                        TestCpuState.init() // execute iut: operate on val | write val(HL) to ram
                            .rPc(0x0003)
                            .rHL(addr)
                            .ram(addr, res),
                        TestCpuState.init() // execute iut: read (PC) from ram
                            .rPc(0x0004)
                            .rHL(addr)
                            .ram(addr, res),
                    },
                );
            }
        }
    }
}

test "test jump immediate" {
    const addr = 0xD00D;
    const instr = 0b11000011;

    const name = try std.fmt.allocPrint(std.testing.allocator, "test jump immediate", .{});
    defer std.testing.allocator.free(name);
    try runTestCase(
        name,
        &[_]u8{
            0x00,
            instr,
            @intCast(addr & 0x00FF),
            @intCast((addr & 0xFF00) >> 8),
        },
        TestCpuState.init(),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002),
            TestCpuState.init() // execute iut: read lo(PC) from ram
                .rPc(0x0003),
            TestCpuState.init() // execute iut: read hi(PC) from ram
                .rPc(0x0004),
            TestCpuState.init() // execute iut: write PC
                .rPc(addr),
            TestCpuState.init() // execute iut: read (PC) from ram
                .rPc(addr + 1),
        },
    );
}

test "test jump HL" {
    const addr = 0xD00D;
    const instr = 0b11101001;

    const name = try std.fmt.allocPrint(std.testing.allocator, "test jump HL", .{});
    defer std.testing.allocator.free(name);
    try runTestCase(
        name,
        &[_]u8{
            0x00,
            instr,
        },
        TestCpuState.init()
            .rHL(addr),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rHL(addr),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rHL(addr),
            TestCpuState.init() // execute iut: write PC | read (PC) from ram
                .rPc(addr + 1)
                .rHL(addr),
        },
    );
}

test "test jump immediate conditional" {
    for (0..4) |cond| {
        for (0..2) |condTrue| {
            const addr = 0xD00D;
            const instr = 0b110_00_010 | (@as(u8, @intCast(cond)) << 3);

            const name = try std.fmt.allocPrint(std.testing.allocator, "test jump immediate conditional (cond={b}) (true={b})", .{ cond, condTrue });
            defer std.testing.allocator.free(name);

            var states = std.ArrayList(*TestCpuState).init(std.testing.allocator);
            defer states.deinit();

            var Z: u1 = 0;
            var C: u1 = 0;
            if ((cond == 0b00 and condTrue == 0) or (cond == 0b01 and condTrue == 1)) {
                Z = 1;
            } else if ((cond == 0b10 and condTrue == 0) or (cond == 0b11 and condTrue == 1)) {
                C = 1;
            }

            try states.append(TestCpuState.init() // read nop(PC) from ram
                .fZ(Z)
                .fC(C)
                .rPc(0x0001));
            try states.append(TestCpuState.init() // execute nop | read iut(PC) from ram
                .fZ(Z)
                .fC(C)
                .rPc(0x0002));
            try states.append(TestCpuState.init() // execute iut: read lo(PC) from ram
                .fZ(Z)
                .fC(C)
                .rPc(0x0003));
            try states.append(TestCpuState.init() // execute iut: read hi(PC) from ram | check condition
                .fZ(Z)
                .fC(C)
                .rPc(0x0004));

            if (condTrue == 1) {
                try states.append(TestCpuState.init() // execute iut: write PC
                    .fZ(Z)
                    .fC(C)
                    .rPc(addr));
                try states.append(TestCpuState.init() // execute iut: read (PC) from ram
                    .fZ(Z)
                    .fC(C)
                    .rPc(addr + 1));
            } else {
                try states.append(TestCpuState.init() // execute iut: read (PC) from ram
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0005));
            }

            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    @intCast(addr & 0x00FF),
                    @intCast((addr & 0xFF00) >> 8),
                },
                TestCpuState.init()
                    .fZ(Z)
                    .fC(C),
                states.items,
            );
        }
    }
}

test "test jump relative" {
    inline for (.{ -1, 0, 1 }) |e| {
        const instr = 0b00011000;

        const name = try std.fmt.allocPrint(std.testing.allocator, "test jump relative (e={d})", .{e});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
                @bitCast(@as(i8, e)),
            },
            TestCpuState.init(),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002),
                TestCpuState.init() // execute iut: read e(PC) from ram
                    .rPc(0x0003),
                TestCpuState.init() // execute iut: calculate PC
                    .rPc(0x0003),
                TestCpuState.init() // execute iut: write PC | read (PC) from ram
                    .rPc(0x0003 + e + 1),
            },
        );
    }
}

test "test jump relative conditional" {
    for (0..4) |cond| {
        for (0..2) |condTrue| {
            inline for (.{ -1, 0, 1 }) |e| {
                const instr = 0b001_00_000 | (@as(u8, @intCast(cond)) << 3);

                const name = try std.fmt.allocPrint(std.testing.allocator, "test jump relative conditional (cond={b}) (true={b}) (e={d})", .{ cond, condTrue, e });
                defer std.testing.allocator.free(name);

                var states = std.ArrayList(*TestCpuState).init(std.testing.allocator);
                defer states.deinit();

                var Z: u1 = 0;
                var C: u1 = 0;
                if ((cond == 0b00 and condTrue == 0) or (cond == 0b01 and condTrue == 1)) {
                    Z = 1;
                } else if ((cond == 0b10 and condTrue == 0) or (cond == 0b11 and condTrue == 1)) {
                    C = 1;
                }

                try states.append(TestCpuState.init() // read nop(PC) from ram
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0001));
                try states.append(TestCpuState.init() // execute nop | read iut(PC) from ram
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0002));
                try states.append(TestCpuState.init() // execute iut: read e(PC) from ram | check condition
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0003));

                if (condTrue == 1) {
                    try states.append(TestCpuState.init() // execute iut: calculate PC
                        .fZ(Z)
                        .fC(C)
                        .rPc(0x0003));
                    try states.append(TestCpuState.init() // execute iut: write PC | read (PC) from ram
                        .fZ(Z)
                        .fC(C)
                        .rPc(0x0003 + e + 1));
                } else {
                    try states.append(TestCpuState.init() // execute iut: read (PC) from ram
                        .fZ(Z)
                        .fC(C)
                        .rPc(0x0004));
                }

                try runTestCase(
                    name,
                    &[_]u8{
                        0x00,
                        instr,
                        @bitCast(@as(i8, e)),
                    },
                    TestCpuState.init()
                        .fZ(Z)
                        .fC(C),
                    states.items,
                );
            }
        }
    }
}

test "test call" {
    const sp = 0xFF50;
    const addr = 0xD00D;
    const instr = 0b11001101;

    const name = try std.fmt.allocPrint(std.testing.allocator, "test call", .{});
    defer std.testing.allocator.free(name);
    try runTestCase(
        name,
        &[_]u8{
            0x00,
            instr,
            @intCast(addr & 0x00FF),
            @intCast((addr & 0xFF00) >> 8),
        },
        TestCpuState.init()
            .rSp(sp),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rSp(sp),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rSp(sp),
            TestCpuState.init() // execute iut: read lo(PC) from ram
                .rPc(0x0003)
                .rSp(sp),
            TestCpuState.init() // execute iut: read hi(PC) from ram
                .rPc(0x0004)
                .rSp(sp),
            TestCpuState.init() // execute iut: decrement SP
                .rPc(0x0004)
                .rSp(sp - 1),
            TestCpuState.init() // execute iut: write PC_hi to ram(SP-1)
                .rPc(0x0004)
                .rSp(sp - 2)
                .ram(sp - 1, 0x00),
            TestCpuState.init() // execute iut: write PC_lo to ram(SP-2) | write PC
                .rPc(addr)
                .rSp(sp - 2)
                .ram(sp - 1, 0x00)
                .ram(sp - 2, 0x04),
            TestCpuState.init() // execute iut: read (PC) from ram
                .rPc(addr + 1)
                .rSp(sp - 2)
                .ram(sp - 1, 0x00)
                .ram(sp - 2, 0x04),
        },
    );
}

test "test call conditional" {
    for (0..4) |cond| {
        for (0..2) |condTrue| {
            const sp = 0xFF50;
            const addr = 0xD00D;
            const instr = 0b110_00_100 | (@as(u8, @intCast(cond)) << 3);

            const name = try std.fmt.allocPrint(std.testing.allocator, "test call conditional (cond={b}) (true={b})", .{ cond, condTrue });
            defer std.testing.allocator.free(name);

            var states = std.ArrayList(*TestCpuState).init(std.testing.allocator);
            defer states.deinit();

            var Z: u1 = 0;
            var C: u1 = 0;
            if ((cond == 0b00 and condTrue == 0) or (cond == 0b01 and condTrue == 1)) {
                Z = 1;
            } else if ((cond == 0b10 and condTrue == 0) or (cond == 0b11 and condTrue == 1)) {
                C = 1;
            }

            try states.append(TestCpuState.init() // read nop(PC) from ram
                .fZ(Z)
                .fC(C)
                .rPc(0x0001)
                .rSp(sp));
            try states.append(TestCpuState.init() // execute nop | read iut(PC) from ram
                .fZ(Z)
                .fC(C)
                .rPc(0x0002)
                .rSp(sp));
            try states.append(TestCpuState.init() // execute iut: read lo(PC) from ram
                .fZ(Z)
                .fC(C)
                .rPc(0x0003)
                .rSp(sp));
            try states.append(TestCpuState.init() // execute iut: read hi(PC) from ram | check condition
                .fZ(Z)
                .fC(C)
                .rPc(0x0004)
                .rSp(sp));

            if (condTrue == 1) {
                try states.append(TestCpuState.init() // execute iut: decrement SP
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0004)
                    .rSp(sp - 1));
                try states.append(TestCpuState.init() // execute iut: write PC_hi to ram(SP-1)
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0004)
                    .rSp(sp - 2)
                    .ram(sp - 1, 0x00));
                try states.append(TestCpuState.init() // execute iut: write PC_lo to ram(SP-2) | write PC
                    .fZ(Z)
                    .fC(C)
                    .rPc(addr)
                    .rSp(sp - 2)
                    .ram(sp - 1, 0x00)
                    .ram(sp - 2, 0x04));
                try states.append(TestCpuState.init() // execute iut: read (PC) from ram
                    .fZ(Z)
                    .fC(C)
                    .rPc(addr + 1)
                    .rSp(sp - 2)
                    .ram(sp - 1, 0x00)
                    .ram(sp - 2, 0x04));
            } else {
                try states.append(TestCpuState.init() // execute iut: read (PC) from ram
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0005)
                    .rSp(sp));
            }

            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                    @intCast(addr & 0x00FF),
                    @intCast((addr & 0xFF00) >> 8),
                },
                TestCpuState.init()
                    .fZ(Z)
                    .fC(C)
                    .rSp(sp),
                states.items,
            );
        }
    }
}

test "test ret" {
    const sp = 0xFF50;
    const instr = 0b11001001;

    const name = try std.fmt.allocPrint(std.testing.allocator, "test ret", .{});
    defer std.testing.allocator.free(name);
    try runTestCase(
        name,
        &[_]u8{
            0x00,
            instr,
        },
        TestCpuState.init()
            .rSp(sp)
            .ram(sp, 0x10)
            .ram(sp + 1, 0x01),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rSp(sp)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rSp(sp)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute iut: read PC_lo(SP) | inc SP
                .rPc(0x0002)
                .rSp(sp + 1)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute iut: read PC_hi(SP) | inc SP
                .rPc(0x0002)
                .rSp(sp + 2)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute iut: write PC
                .rPc(0x0110)
                .rSp(sp + 2)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute iut: read (PC) from ram
                .rPc(0x0111)
                .rSp(sp + 2)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
        },
    );
}

test "test ret conditional" {
    for (0..4) |cond| {
        for (0..2) |condTrue| {
            const sp = 0xFF50;
            const instr = 0b110_00_000 | (@as(u8, @intCast(cond)) << 3);

            const name = try std.fmt.allocPrint(std.testing.allocator, "test ret conditional (cond={b}) (true={b})", .{ cond, condTrue });
            defer std.testing.allocator.free(name);

            var states = std.ArrayList(*TestCpuState).init(std.testing.allocator);
            defer states.deinit();

            var Z: u1 = 0;
            var C: u1 = 0;
            if ((cond == 0b00 and condTrue == 0) or (cond == 0b01 and condTrue == 1)) {
                Z = 1;
            } else if ((cond == 0b10 and condTrue == 0) or (cond == 0b11 and condTrue == 1)) {
                C = 1;
            }

            try states.append(TestCpuState.init() // read nop(PC) from ram
                .fZ(Z)
                .fC(C)
                .rPc(0x0001)
                .rSp(sp)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01));
            try states.append(TestCpuState.init() // execute nop | read iut(PC) from ram
                .fZ(Z)
                .fC(C)
                .rPc(0x0002)
                .rSp(sp)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01));
            try states.append(TestCpuState.init() // execute iut | check condition
                .fZ(Z)
                .fC(C)
                .rPc(0x0002)
                .rSp(sp)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01));

            if (condTrue == 1) {
                try states.append(TestCpuState.init() // execute iut: inc SP | read PC_lo(SP)
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0002)
                    .rSp(sp + 1)
                    .ram(sp, 0x10)
                    .ram(sp + 1, 0x01));
                try states.append(TestCpuState.init() // execute iut: inc SP | read PC_hi(SP)
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0002)
                    .rSp(sp + 2)
                    .ram(sp, 0x10)
                    .ram(sp + 1, 0x01));
                try states.append(TestCpuState.init() // execute iut: write PC
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0110)
                    .rSp(sp + 2)
                    .ram(sp, 0x10)
                    .ram(sp + 1, 0x01));
                try states.append(TestCpuState.init() // execute iut: read (PC) from ram
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0111)
                    .rSp(sp + 2)
                    .ram(sp, 0x10)
                    .ram(sp + 1, 0x01));
            } else {
                try states.append(TestCpuState.init() // execute iut: read (PC) from ram
                    .fZ(Z)
                    .fC(C)
                    .rPc(0x0003)
                    .rSp(sp)
                    .ram(sp, 0x10)
                    .ram(sp + 1, 0x01));
            }

            try runTestCase(
                name,
                &[_]u8{
                    0x00,
                    instr,
                },
                TestCpuState.init()
                    .fZ(Z)
                    .fC(C)
                    .rSp(sp)
                    .ram(sp, 0x10)
                    .ram(sp + 1, 0x01),
                states.items,
            );
        }
    }
}

test "test reti" {
    const sp = 0xFF50;
    const instr = 0b11011001;

    const name = try std.fmt.allocPrint(std.testing.allocator, "test reti", .{});
    defer std.testing.allocator.free(name);
    try runTestCase(
        name,
        &[_]u8{
            0x00,
            instr,
        },
        TestCpuState.init()
            .rSp(sp)
            .ram(sp, 0x10)
            .ram(sp + 1, 0x01),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001)
                .rSp(sp)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPc(0x0002)
                .rSp(sp)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute iut: read PC_lo(SP) | inc SP
                .rPc(0x0002)
                .rSp(sp + 1)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute iut: read PC_hi(SP) | inc SP
                .rPc(0x0002)
                .rSp(sp + 2)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute iut: write PC | set IME
                .rPc(0x0110)
                .rSp(sp + 2)
                .rIme(1)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
            TestCpuState.init() // execute iut: read (PC) from ram
                .rPc(0x0111)
                .rSp(sp + 2)
                .rIme(1)
                .ram(sp, 0x10)
                .ram(sp + 1, 0x01),
        },
    );
}

test "test rst" {
    for (0..8) |nS| {
        const n: u3 = @intCast(nS);
        const sp = 0xFF50;
        const instr = 0b11_000_111 | (@as(u8, @intCast(n)) << 3);
        const addr: u16 = @intCast(n);

        const name = try std.fmt.allocPrint(std.testing.allocator, "test rst", .{});
        defer std.testing.allocator.free(name);
        try runTestCase(
            name,
            &[_]u8{
                0x00,
                instr,
            },
            TestCpuState.init()
                .rSp(sp),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPc(0x0001)
                    .rSp(sp),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPc(0x0002)
                    .rSp(sp),
                TestCpuState.init() // execute iut: decrement SP
                    .rPc(0x0002)
                    .rSp(sp - 1),
                TestCpuState.init() // execute iut: write PC_hi to ram(SP-1)
                    .rPc(0x0002)
                    .rSp(sp - 2)
                    .ram(sp - 1, 0x00),
                TestCpuState.init() // execute iut: write PC_lo to ram(SP-2) | write PC
                    .rPc(addr)
                    .rSp(sp - 2)
                    .ram(sp - 1, 0x00)
                    .ram(sp - 2, 0x02),
                TestCpuState.init() // execute iut: read (PC) from ram
                    .rPc(addr + 1)
                    .rSp(sp - 2)
                    .ram(sp - 1, 0x00)
                    .ram(sp - 2, 0x02),
            },
        );
    }
}

test "test ei di" {
    const ei = 0xFB;
    const di = 0xF3;

    const name = try std.fmt.allocPrint(std.testing.allocator, "test ei di", .{});
    defer std.testing.allocator.free(name);
    try runTestCase(
        name,
        &[_]u8{
            0x00,
            di,
            0x00,
            ei,
            0x01, // LD BC, u16
            0x00,
            0x00,
            di,
            ei,
            di,
            0x00,
            0x00,
            0x00,
            0xFD,
        },
        TestCpuState.init(),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPc(0x0001),
            TestCpuState.init() // execute nop | read di(PC) from ram
                .rPc(0x0002),
            TestCpuState.init() // execute di: disable interrupts | read nop(PC) from ram
                .rPc(0x0003),
            TestCpuState.init() // execute nop | read ei(PC) from ram
                .rPc(0x0004),
            TestCpuState.init() // execute ei: schedule interrupt enable | read ld(PC) from ram
                .rPc(0x0005),
            TestCpuState.init() // execute ld: read immediate(PC)
                .rPc(0x0006),
            TestCpuState.init() // execute ld: read immediate(PC)
                .rPc(0x0007),
            TestCpuState.init() // execute ld: read di(PC) from ram | interrupts are enabled
                .rPc(0x0008)
                .rIme(1),
            TestCpuState.init() // execute di: disable interrupts | read ei(PC) from ram
                .rPc(0x0009),
            TestCpuState.init() // execute ei: schedule interrupt enable | read di(PC) from ram
                .rPc(0x000A),
            TestCpuState.init() // execute di: cancel interrupt enable | read nop(PC) from ram
                .rPc(0x000B),
            TestCpuState.init() // execute nop | read nop(PC) from ram
                .rPc(0x000C),
            TestCpuState.init() // execute nop | read nop(PC) from ram
                .rPc(0x000D),
            TestCpuState.init() // execute nop | read 0xFD(PC) from ram
                .rPc(0x000E),
        },
    );
}
