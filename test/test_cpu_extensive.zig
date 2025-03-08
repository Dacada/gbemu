const std = @import("std");
const testutil = @import("testutil.zig");
const run_test_case = testutil.run_test_case;
const TestCpuState = testutil.TestCpuState;

// IUT = Instruction Under Test

test "Load register (register)" {
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

            const name = try std.fmt.allocPrint(std.testing.allocator, "Load register (from={b} to={b})", .{ from, to });
            defer std.testing.allocator.free(name);
            try run_test_case(
                name,
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
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .reg(@intCast(from), test_value),
                    TestCpuState.init() // execute nop | load iut(PC) from ram
                        .rPC(0x0002)
                        .reg(@intCast(from), test_value),
                    TestCpuState.init() // execute iut | read (PC) from ram
                        .rPC(0x0003)
                        .reg(@intCast(from), test_value)
                        .reg(@intCast(to), test_value),
                },
            );
        }
    }
}

test "Load register (immediate)" {
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

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load register (immediate) (to={b})", .{to});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
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
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002),
                TestCpuState.init() // execute iut: read immediate(PC) from ram
                    .rPC(0x0003),
                TestCpuState.init() // read (PC) from ram | write immediate to reg
                    .rPC(0x0004)
                    .reg(@intCast(to), test_value),
            },
        );
    }
}

test "Load register (indirect HL)" {
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

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load register (indirect HL) (to={b})", .{to});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
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
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute iut: read data(HL) from ram
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram | write data to reg
                    .rPC(0x0003)
                    .rHL(test_addr)
                    .reg(@intCast(to), test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load from register (indirect HL)" {
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

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD from register (indirect HL) (from={b})", .{from});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
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
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rHL(test_addr)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // execute iut: write reg(HL) to ram
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value)
                    .reg(@intCast(from), test_value),
                TestCpuState.init() // read (PC) from ram
                    .rPC(0x0003)
                    .rHL(test_addr)
                    .ram(test_addr, test_value)
                    .reg(@intCast(from), test_value),
            },
        );
    }
}

test "Load from immediate data (indirect HL)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b00_110_110;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD0D0;

    try run_test_case(
        "Load from immediate data (indirect HL)",
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
            TestCpuState.init() // read nop(PC) from ram
                .rPC(0x0001)
                .rHL(test_addr),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .rHL(test_addr),
            TestCpuState.init() // execute iut: read immediate(PC) from ram
                .rPC(0x0003)
                .rHL(test_addr),
            TestCpuState.init() // execute iut: write immediate(HL) to ram
                .rPC(0x0003)
                .rHL(test_addr)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPC(0x0004)
                .rHL(test_addr)
                .ram(test_addr, test_value),
        },
    );
}

test "Load accumulator (indirect)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..2) |from| {
        // Constants
        const instr: u8 = @intCast(0b000_0_1010 | (from << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD00D;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load accumulator (indirect) (from={b})", .{from});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
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
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .reg16(@intCast(from), test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute iut: read data(reg) from ram
                    .rPC(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram | write data to A
                    .rPC(0x0003)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load from accumulator (indirect)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..2) |from| {
        // Constants
        const instr: u8 = @intCast(0b000_0_0010 | (from << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xD00D;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load from accumulator (indirect) (from={b})", .{from});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
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
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute iut: write A(reg) to ram
                    .rPC(0x0002)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram
                    .rPC(0x0003)
                    .reg16(@intCast(from), test_addr)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load accumulator (direct)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11111010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD00D;

    try run_test_case(
        "Load accumulator (direct)",
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
            TestCpuState.init() // read nop(PC) from ram
                .rPC(0x0001)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read address_lsb(PC) from ram
                .rPC(0x0003)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read address_msb(PC) from ram
                .rPC(0x0004)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read data(address) from ram
                .rPC(0x0004)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram | write data to A
                .rPC(0x0005)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load from accumulator (direct)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11101010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xD00D;

    try run_test_case(
        "Load from accumulator (direct)",
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
            TestCpuState.init() // read nop(PC) from ram
                .rPC(0x0001)
                .rA(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .rA(test_value),
            TestCpuState.init() // execute iut: read address_lsb(PC) from ram
                .rPC(0x0003)
                .rA(test_value),
            TestCpuState.init() // execute iut: read address_msb(PC) from ram
                .rPC(0x0004)
                .rA(test_value),
            TestCpuState.init() // execute iut: write A(address) to ram
                .rPC(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPC(0x0005)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load accumulator (indirect 0xFF00 + C)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11110010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try run_test_case(
        "Load accumulator (indirect 0xFF00 + C)",
        rom,
        exram,
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
                .rPC(0x0001)
                .rC(test_addr & 0xFF)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .rC(test_addr & 0xFF)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read data(0xFF00+C) from ram
                .rPC(0x0002)
                .rC(test_addr & 0xFF)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram | write data to A
                .rPC(0x0003)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load from accumulator (indirect 0xFF00 + C)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11100010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try run_test_case(
        "Load from accumulator (indirect 0xFF00 + C)",
        rom,
        exram,
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
                .rPC(0x0001)
                .rC(test_addr & 0xFF)
                .rA(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .rC(test_addr & 0xFF)
                .rA(test_value),
            TestCpuState.init() // execute iut: write A(0xFF00+C) to ram
                .rPC(0x0002)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPC(0x0003)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load accumulator (direct 0xFF00 + n)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11110000;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try run_test_case(
        "Load accumulator (direct 0xFF00 + n)",
        rom,
        exram,
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
                .rPC(0x0001)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read immediate(PC) from ram
                .rPC(0x0003)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute iut: read data(0xFF00+immediate) from ram
                .rPC(0x0003)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPC(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load from accumulator (direct 0xFF00 + n)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11100000;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try run_test_case(
        "Load from accumulator (direct 0xFF00 + n)",
        rom,
        exram,
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
                .rPC(0x0001)
                .rA(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .rA(test_value),
            TestCpuState.init() // execute iut: read immediate(PC) from ram
                .rPC(0x0003)
                .rA(test_value),
            TestCpuState.init() // execute iut: write A(0xFF00+immediate) to ram
                .rPC(0x0003)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // read (PC) from ram
                .rPC(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "Load accumulator (indirect HL)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
        try run_test_case(
            name,
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
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop | load iut(PC) from ram
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute iut: read data(HL) from ram | inc/dec HL
                    .rPC(0x0002)
                    .rHL(test_addr_next)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram | write data to A
                    .rPC(0x0003)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load from accumulator (indirect HL)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
        try run_test_case(
            name,
            rom,
            exram,
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
                    .rPC(0x0001)
                    .rHL(test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute iut: write A(HL) to ram
                    .rPC(0x0002)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // read (PC) from ram
                    .rPC(0x0003)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "Load 16-bit register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..(0b11 + 1)) |reg| {
        // Constants
        const instr: u8 = @intCast(0b00000001 | (reg << 4));
        const test_value: u16 = 0xABCD;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load 16-bit register (reg={b})", .{reg});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
            rom,
            exram,
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
                    .rPC(0x0001),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002),
                TestCpuState.init() // execute iut: read data_lsb(PC) from ram
                    .rPC(0x0003),
                TestCpuState.init() // execute iut: read data_msb(PC) from ram
                    .rPC(0x0004),
                TestCpuState.init() // read (PC) | write data to reg
                    .rPC(0x0005)
                    .reg16(reg, test_value),
            },
        );
    }
}

test "Load from stack pointer (direct)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr = 0b00001000;
    const test_addr = 0xD00D;
    const test_value = 0xFFAA;

    try run_test_case(
        "Load from stack pointer (direct)",
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
            .rSP(test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPC(0x0001)
                .rSP(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .rSP(test_value),
            TestCpuState.init() // execute iut: read address_lsb(PC) from ram
                .rPC(0x0003)
                .rSP(test_value),
            TestCpuState.init() // execute iut: read address_msb(PC) from ram
                .rPC(0x0004)
                .rSP(test_value),
            TestCpuState.init() // execute iut: write SP_lsb(address) to ram
                .rPC(0x0004)
                .rSP(test_value)
                .ram(test_addr, test_value & 0xFF),
            TestCpuState.init() // execute iut: write SP_msb(address) to ram
                .rPC(0x0004)
                .rSP(test_value)
                .ram(test_addr, test_value & 0xFF)
                .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
            TestCpuState.init() // read (PC) from ram
                .rPC(0x0005)
                .rSP(test_value)
                .ram(test_addr, test_value & 0xFF)
                .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
        },
    );
}

test "Load stack pointer from HL" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr = 0b11111001;
    const test_value = 0xFFAA;

    try run_test_case(
        "Load stack pointer from HL",
        rom,
        exram,
        &[_]u8{
            0x00,
            instr,
            0xFD,
        },
        TestCpuState.init()
            .rHL(test_value),
        &[_]*TestCpuState{
            TestCpuState.init() // read nop(PC) from ram
                .rPC(0x0001)
                .rHL(test_value),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .rHL(test_value),
            TestCpuState.init() // execute iut: write HL to SP
                .rPC(0x0002)
                .rHL(test_value)
                .rSP(test_value),
            TestCpuState.init() // read (PC) from ram
                .rPC(0x0003)
                .rHL(test_value)
                .rSP(test_value),
        },
    );
}

test "Push to stack" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..(0b11 + 1)) |reg| {
        // Constants
        const instr: u8 = @intCast(0b11000101 | (reg << 4));
        const test_value: u16 = 0xABCD;
        const test_addr: u16 = 0xFFAA;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Push to stack (reg={b})", .{reg});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
            rom,
            exram,
            &[_]u8{
                0x00,
                instr,
                0xFD,
            },
            TestCpuState.init()
                .rSP(test_addr)
                .reg16p(reg, test_value),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rSP(test_addr)
                    .reg16p(reg, test_value),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rSP(test_addr)
                    .reg16p(reg, test_value),
                TestCpuState.init() // execute iut: decrease SP
                    .rPC(0x0002)
                    .rSP(test_addr - 1)
                    .reg16p(reg, test_value),
                TestCpuState.init() // execute iut: write reg_msb[SP] to ram | decrease SP
                    .rPC(0x0002)
                    .rSP(test_addr - 2)
                    .reg16p(reg, test_value)
                    .ram(test_addr - 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // execute iut: write reg_lsb[SP] to ram
                    .rPC(0x0002)
                    .rSP(test_addr - 2)
                    .reg16p(reg, test_value)
                    .ram(test_addr - 1, (test_value & 0xFF00) >> 8)
                    .ram(test_addr - 2, (test_value & 0xFF)),
                TestCpuState.init() // read (PC)
                    .rPC(0x0003)
                    .rSP(test_addr - 2)
                    .reg16p(reg, test_value)
                    .ram(test_addr - 1, (test_value & 0xFF00) >> 8)
                    .ram(test_addr - 2, (test_value & 0xFF)),
            },
        );
    }
}
