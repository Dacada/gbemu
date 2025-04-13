const std = @import("std");
const testutil = @import("testutil.zig");
const run_test_case = testutil.run_test_case;
const TestCpuState = testutil.TestCpuState;
const alu = @import("lib").alu;

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

test "Pop from stack" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..(0b11 + 1)) |reg| {
        // Constants
        const instr: u8 = @intCast(0b11000001 | (reg << 4));
        const test_value: u16 = 0xABCD;
        const test_addr: u16 = 0xFFAA;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Pop from stack (reg={b})", .{reg});
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
                .ram(test_addr, test_value & 0xFF)
                .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rSP(test_addr)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rSP(test_addr)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // execute iut: read data_lsb[SP] | increase SP
                    .rPC(0x0002)
                    .rSP(test_addr + 1)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // execute iut: read data_msb[SP] | increase SP
                    .rPC(0x0002)
                    .rSP(test_addr + 2)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
                TestCpuState.init() // read (PC) | write data to reg
                    .rPC(0x0003)
                    .rSP(test_addr + 2)
                    .reg16p(reg, test_value)
                    .ram(test_addr, test_value & 0xFF)
                    .ram(test_addr + 1, (test_value & 0xFF00) >> 8),
            },
        );
    }
}

test "Load HL from adjusted SP" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0x01, 0x04, 0x0F, 0xF4, 0xFF }) |e| {
        // Constants
        const instr: u8 = 0b11111000;
        const test_value: u16 = 0x100D;

        const unsigned_e: u8 = @intCast(e);

        var reg = alu.AluRegister{
            .Hi = 0x00,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        const expected_lo = reg.add_return(@intCast(test_value & 0xFF), unsigned_e, 0, 0);
        const expected_hi = reg.add_adj(@intCast((test_value & 0xFF00) >> 8), unsigned_e);
        const expected = (@as(u16, expected_hi) << 8) | expected_lo;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Load HL from adjusted SP (e={d})", .{e});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
            rom,
            exram,
            &[_]u8{
                0x00,
                instr,
                unsigned_e,
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
                TestCpuState.init() // execute iut: read e(PC) from ram
                    .rPC(0x0003)
                    .rSP(test_value),
                TestCpuState.init() // execute iut: add e to SP_lsb
                    .rPC(0x0003)
                    .fC(reg.Lo.C)
                    .fH(reg.Lo.H)
                    .rSP(test_value)
                    .rL(expected_lo),
                TestCpuState.init() // read (PC) | execute iut: add carry to SP_msb
                    .rPC(0x0004)
                    .fC(reg.Lo.C)
                    .fH(reg.Lo.H)
                    .rSP(test_value)
                    .rHL(expected),
            },
        );
    }
}

test "Add (register)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
                    .Hi = test_val,
                    .Lo = alu.RegisterFlags{
                        .C = carry_flag,
                        .H = 0,
                        .N = 0,
                        .Z = 0,
                        .rest = 0,
                    },
                };
                res.add(val_u8, carry_instr);

                const name = try std.fmt.allocPrint(std.testing.allocator, "Add (reg={b}) (val={x}) (with_carry={b})", .{ reg, val, with_carry });
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
                        .rA(test_val)
                        .fC(carry_flag)
                        .reg(reg_u3, val_u8),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPC(0x0001)
                            .rA(test_val)
                            .fC(carry_flag)
                            .reg(reg_u3, val_u8),
                        TestCpuState.init() // execute nop | read iut(PC) from ram
                            .rPC(0x0002)
                            .rA(test_val)
                            .fC(carry_flag)
                            .reg(reg_u3, val_u8),
                        TestCpuState.init() // execute iut: add reg to a | read (PC)
                            .rPC(0x0003)
                            .reg(reg_u3, val_u8)
                            .rAF(res.all()),
                    },
                );
            }
        }
    }
}

test "Add (register A)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const instr: u8 = 0b10000111 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .Hi = val_u8,
                .Lo = alu.RegisterFlags{
                    .C = carry_flag,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.add(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Add (reg=111) (val={x}) (with_carry={b})", .{ val, with_carry });
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
                    .rA(val_u8)
                    .fC(carry_flag),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .rA(val_u8)
                        .fC(carry_flag),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .rA(val_u8)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: add A to itself | read (PC)
                        .rPC(0x0003)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "Add (indirect HL)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
                .Hi = test_val,
                .Lo = alu.RegisterFlags{
                    .C = carry_flag,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.add(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Add (indirect HL) (val={x}) (with_carry={b})", .{ val, with_carry });
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
                    .rA(test_val)
                    .fC(carry_flag)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute iut: read val(HL) from ram
                        .rPC(0x0002)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute iut: add val to A | read (PC)
                        .rPC(0x0003)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "Add (immediate)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const test_val: u8 = 0xAA;
            const instr: u8 = 0b11000110 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .Hi = test_val,
                .Lo = alu.RegisterFlags{
                    .C = carry_flag,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.add(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Add (immediate) (val={x}) (with_carry={b})", .{ val, with_carry });
            defer std.testing.allocator.free(name);
            try run_test_case(
                name,
                rom,
                exram,
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
                        .rPC(0x0001)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: read val(PC) from ram
                        .rPC(0x0003)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: add val to A | read (PC)
                        .rPC(0x0004)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "Sub (register)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
                    .Hi = test_val,
                    .Lo = alu.RegisterFlags{
                        .C = carry_flag,
                        .H = 0,
                        .N = 0,
                        .Z = 0,
                        .rest = 0,
                    },
                };
                res.sub(val_u8, carry_instr);

                const name = try std.fmt.allocPrint(std.testing.allocator, "Sub (reg={b}) (val={x}) (with_carry={b})", .{ reg, val, with_carry });
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
                        .rA(test_val)
                        .fC(carry_flag)
                        .reg(reg_u3, val_u8),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPC(0x0001)
                            .rA(test_val)
                            .fC(carry_flag)
                            .reg(reg_u3, val_u8),
                        TestCpuState.init() // execute nop | read iut(PC) from ram
                            .rPC(0x0002)
                            .rA(test_val)
                            .fC(carry_flag)
                            .reg(reg_u3, val_u8),
                        TestCpuState.init() // execute iut: sub reg from a | read (PC)
                            .rPC(0x0003)
                            .reg(reg_u3, val_u8)
                            .rAF(res.all()),
                    },
                );
            }
        }
    }
}

test "Sub (register A)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0xC, 0xF, 0xCC, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const instr: u8 = 0b10010111 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .Hi = val_u8,
                .Lo = alu.RegisterFlags{
                    .C = carry_flag,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.sub(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Sub (reg=111) (val={x}) (with_carry={b})", .{ val, with_carry });
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
                    .rA(val_u8)
                    .fC(carry_flag),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .rA(val_u8)
                        .fC(carry_flag),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .rA(val_u8)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: sub A from itself | read (PC)
                        .rPC(0x0003)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "Sub (indirect HL)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
                .Hi = test_val,
                .Lo = alu.RegisterFlags{
                    .C = carry_flag,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.sub(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Sub (indirect HL) (val={x}) (with_carry={b})", .{ val, with_carry });
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
                    .rA(test_val)
                    .fC(carry_flag)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute iut: read val(HL) from ram
                        .rPC(0x0002)
                        .rA(test_val)
                        .fC(carry_flag)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr),
                    TestCpuState.init() // execute iut: sub val from A | read (PC)
                        .rPC(0x0003)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "Sub (immediate)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    for (0..4) |with_carry| {
        inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
            // Constants
            const val_u8: u8 = @intCast(val);

            const carry_flag: u1 = @intCast(with_carry & 0b01);
            const carry_instr: u1 = @intCast((with_carry & 0b10) >> 1);

            const test_val: u8 = 0xAA;
            const instr: u8 = 0b11010110 | (@as(u8, carry_instr) << 3);

            var res = alu.AluRegister{
                .Hi = test_val,
                .Lo = alu.RegisterFlags{
                    .C = carry_flag,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.sub(val_u8, carry_instr);

            const name = try std.fmt.allocPrint(std.testing.allocator, "Sub (immediate) (val={x}) (with_carry={b})", .{ val, with_carry });
            defer std.testing.allocator.free(name);
            try run_test_case(
                name,
                rom,
                exram,
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
                        .rPC(0x0001)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: read val(PC) from ram
                        .rPC(0x0003)
                        .rA(test_val)
                        .fC(carry_flag),
                    TestCpuState.init() // execute iut: sub val from A | read (PC)
                        .rPC(0x0004)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "CP (register)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
                .Hi = test_val,
                .Lo = alu.RegisterFlags{
                    .C = 0,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.sub(val_u8, 0);

            const name = try std.fmt.allocPrint(std.testing.allocator, "CP (reg={b}) (val={x})", .{ reg, val });
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
                    .rA(test_val)
                    .reg(reg_u3, val_u8),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .rA(test_val)
                        .reg(reg_u3, val_u8),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .rA(test_val)
                        .reg(reg_u3, val_u8),
                    TestCpuState.init() // execute iut: sub reg from a, store flags only | read (PC)
                        .rPC(0x0003)
                        .reg(reg_u3, val_u8)
                        .rA(test_val)
                        .rF(res.Lo.all()),
                },
            );
        }
    }
}

test "CP (register A)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0x01, 0xC, 0xF, 0xCC, 0xFF }) |val| {
        // Constants
        const val_u8: u8 = @intCast(val);

        const instr: u8 = 0b10111111;

        var res = alu.AluRegister{
            .Hi = val_u8,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.sub(val_u8, 0);

        const name = try std.fmt.allocPrint(std.testing.allocator, "CP (reg=111) (val={x})", .{val});
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
                .rA(val_u8),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rA(val_u8),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(val_u8),
                TestCpuState.init() // execute iut: sub A from itself, store flags only | read (PC)
                    .rPC(0x0003)
                    .rA(val_u8)
                    .rF(res.Lo.all()),
            },
        );
    }
}

test "CP (indirect HL)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
        // Constants
        const val_u8: u8 = @intCast(val);

        const test_val: u8 = 0xAA;
        const test_addr: u16 = 0xD00D;
        const instr: u8 = 0b10111110;

        var res = alu.AluRegister{
            .Hi = test_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.sub(val_u8, 0);

        const name = try std.fmt.allocPrint(std.testing.allocator, "CP (indirect HL) (val={x})", .{
            val,
        });
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
                .rA(test_val)
                .ram(test_addr, val_u8)
                .rHL(test_addr),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rA(test_val)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(test_val)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                TestCpuState.init() // execute iut: read val(HL) from ram
                    .rPC(0x0002)
                    .rA(test_val)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr),
                TestCpuState.init() // execute iut: sub val from A, store flags only | read (PC)
                    .rPC(0x0003)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr)
                    .rA(test_val)
                    .rF(res.Lo.all()),
            },
        );
    }
}

test "CP (immediate)" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0x01, 0x4, 0xF, 0x44, 0xFF }) |val| {
        // Constants
        const val_u8: u8 = @intCast(val);

        const test_val: u8 = 0xAA;
        const instr: u8 = 0b11111110;

        var res = alu.AluRegister{
            .Hi = test_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.sub(val_u8, 0);

        const name = try std.fmt.allocPrint(std.testing.allocator, "CP (immediate) (val={x})", .{val});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
            rom,
            exram,
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
                    .rPC(0x0001)
                    .rA(test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(test_val),
                TestCpuState.init() // execute iut: read val(PC) from ram
                    .rPC(0x0003)
                    .rA(test_val),
                TestCpuState.init() // execute iut: sub val from A, store flags only | read (PC)
                    .rPC(0x0004)
                    .rA(test_val)
                    .rF(res.Lo.all()),
            },
        );
    }
}

test "INC/DEC Register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0, 1 }) |incdec| {
        inline for (.{ 0x00, 0xFF, 0xF0, 0x0F, 0xAA }) |test_val| {
            inline for (0..(0b111 + 1)) |reg| {
                if (reg == 0b110) {
                    continue;
                }

                // Constants
                const instr: u8 = 0b00_000_10_0 | (reg << 3) | incdec;

                var rreg = alu.AluRegister{
                    .Hi = test_val,
                    .Lo = alu.RegisterFlags{
                        .C = 0,
                        .H = 0,
                        .N = 0,
                        .Z = 0,
                        .rest = 0,
                    },
                };
                const res = if (incdec == 0)
                    rreg.inc(test_val)
                else
                    rreg.dec(test_val);

                const name = try std.fmt.allocPrint(std.testing.allocator, "INC/DEC (inc/dec={b}) (test_val={x}) (reg={b})", .{ incdec, test_val, reg });
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
                        .reg(reg, test_val),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPC(0x0001)
                            .reg(reg, test_val),
                        TestCpuState.init() // execute nop | read iut(PC) from ram
                            .rPC(0x0002)
                            .reg(reg, test_val),
                        TestCpuState.init() // execute iut: inc/dec reg | read (PC)
                            .rPC(0x0003)
                            .reg(reg, res)
                            .rF(rreg.Lo.all()),
                    },
                );
            }
        }
    }
}

test "INC/DEC Indirect" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0, 1 }) |incdec| {
        inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
            // Constants
            const instr: u8 = 0b00_110_10_0 | incdec;
            const test_addr = 0xD00D;

            var reg = alu.AluRegister{
                .Hi = test_val,
                .Lo = alu.RegisterFlags{
                    .C = 0,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            const res = if (incdec == 0)
                reg.inc(test_val)
            else
                reg.dec(test_val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "INC/DEC Indirect (inc/dec={b}) (test_val={x})", .{ incdec, test_val });
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
                    .ram(test_addr, test_val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .rHL(test_addr)
                        .ram(test_addr, test_val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .rHL(test_addr)
                        .ram(test_addr, test_val),
                    TestCpuState.init() // execute iut: read val(HL) from ram
                        .rPC(0x0002)
                        .rHL(test_addr)
                        .ram(test_addr, test_val),
                    TestCpuState.init() // execute iut: inc/dec val | write val(HL) to ram
                        .rPC(0x0002)
                        .rHL(test_addr)
                        .ram(test_addr, res)
                        .rF(reg.Lo.all()),
                    TestCpuState.init() // read (PC)
                        .rPC(0x0003)
                        .rHL(test_addr)
                        .ram(test_addr, res)
                        .rF(reg.Lo.all()),
                },
            );
        }
    }
}

test "AND register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..(0b111 + 1)) |reg| {
        if (reg == 0b110 or reg == 0b111) {
            continue;
        }
        inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
            // Constants
            const instr: u8 = 0b10100_000 | reg;

            const reg_val = 0xAA;
            var res = alu.AluRegister{
                .Hi = reg_val,
                .Lo = alu.RegisterFlags{
                    .C = 0,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.and_(test_val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "AND register (reg={b}) (test_val={x})", .{ reg, test_val });
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
                    .reg(reg, reg_val)
                    .rA(test_val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute iut: and reg to A | read (PC) from ram
                        .rPC(0x0003)
                        .reg(reg, reg_val)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "AND register A" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10100_111;

        var res = alu.AluRegister{
            .Hi = test_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.and_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "AND register A (test_val={x})", .{test_val});
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
                .rA(test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rA(test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(test_val),
                TestCpuState.init() // execute iut: and reg to A | read (PC) from ram
                    .rPC(0x0003)
                    .rAF(res.all()),
            },
        );
    }
}

test "AND indirect HL" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10100_110;

        const test_addr = 0xD00D;
        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .Hi = reg_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.and_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "AND indirect HL (test_val={x})", .{test_val});
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
                .rA(reg_val)
                .rHL(test_addr)
                .ram(test_addr, test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: read val from ram(HL)
                    .rPC(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: and val to A | read (PC) from ram
                    .rPC(0x0003)
                    .rAF(res.all())
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
            },
        );
    }
}

test "AND immediate" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b11100110;

        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .Hi = reg_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.and_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "AND immediate (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
            rom,
            exram,
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
                    .rPC(0x0001)
                    .rA(reg_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: read val from ram(PC)
                    .rPC(0x0003)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: and val to A | read (PC) from ram
                    .rPC(0x0004)
                    .rAF(res.all()),
            },
        );
    }
}

test "OR register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..(0b111 + 1)) |reg| {
        if (reg == 0b110 or reg == 0b111) {
            continue;
        }
        inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
            // Constants
            const instr: u8 = 0b10110_000 | reg;

            const reg_val = 0xAA;
            var res = alu.AluRegister{
                .Hi = reg_val,
                .Lo = alu.RegisterFlags{
                    .C = 0,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.or_(test_val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "OR register (reg={b}) (test_val={x})", .{ reg, test_val });
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
                    .reg(reg, reg_val)
                    .rA(test_val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute iut: or reg to A | read (PC) from ram
                        .rPC(0x0003)
                        .reg(reg, reg_val)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "OR register A" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10110_111;

        var res = alu.AluRegister{
            .Hi = test_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.or_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "OR register A (test_val={x})", .{test_val});
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
                .rA(test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rA(test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(test_val),
                TestCpuState.init() // execute iut: or reg to A | read (PC) from ram
                    .rPC(0x0003)
                    .rAF(res.all()),
            },
        );
    }
}

test "OR indirect HL" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10110_110;

        const test_addr = 0xD00D;
        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .Hi = reg_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.or_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "OR indirect HL (test_val={x})", .{test_val});
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
                .rA(reg_val)
                .rHL(test_addr)
                .ram(test_addr, test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: read val from ram(HL)
                    .rPC(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: or val to A | read (PC) from ram
                    .rPC(0x0003)
                    .rAF(res.all())
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
            },
        );
    }
}

test "OR immediate" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b11110110;

        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .Hi = reg_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.or_(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "OR immediate (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
            rom,
            exram,
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
                    .rPC(0x0001)
                    .rA(reg_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: read val from ram(PC)
                    .rPC(0x0003)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: or val to A | read (PC) from ram
                    .rPC(0x0004)
                    .rAF(res.all()),
            },
        );
    }
}

test "XOR register" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..(0b111 + 1)) |reg| {
        if (reg == 0b110 or reg == 0b111) {
            continue;
        }
        inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
            // Constants
            const instr: u8 = 0b10101_000 | reg;

            const reg_val = 0xAA;
            var res = alu.AluRegister{
                .Hi = reg_val,
                .Lo = alu.RegisterFlags{
                    .C = 0,
                    .H = 0,
                    .N = 0,
                    .Z = 0,
                    .rest = 0,
                },
            };
            res.xor(test_val);

            const name = try std.fmt.allocPrint(std.testing.allocator, "XOR register (reg={b}) (test_val={x})", .{ reg, test_val });
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
                    .reg(reg, reg_val)
                    .rA(test_val),
                &[_]*TestCpuState{
                    TestCpuState.init() // read nop(PC) from ram
                        .rPC(0x0001)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute nop | read iut(PC) from ram
                        .rPC(0x0002)
                        .reg(reg, reg_val)
                        .rA(test_val),
                    TestCpuState.init() // execute iut: xor reg to A | read (PC) from ram
                        .rPC(0x0003)
                        .reg(reg, reg_val)
                        .rAF(res.all()),
                },
            );
        }
    }
}

test "XOR register A" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10101_111;

        var res = alu.AluRegister{
            .Hi = test_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.xor(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "XOR register A (test_val={x})", .{test_val});
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
                .rA(test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rA(test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(test_val),
                TestCpuState.init() // execute iut: xor reg to A | read (PC) from ram
                    .rPC(0x0003)
                    .rAF(res.all()),
            },
        );
    }
}

test "XOR indirect HL" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b10101_110;

        const test_addr = 0xD00D;
        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .Hi = reg_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.xor(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "XOR indirect HL (test_val={x})", .{test_val});
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
                .rA(reg_val)
                .rHL(test_addr)
                .ram(test_addr, test_val),
            &[_]*TestCpuState{
                TestCpuState.init() // read nop(PC) from ram
                    .rPC(0x0001)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: read val from ram(HL)
                    .rPC(0x0002)
                    .rA(reg_val)
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
                TestCpuState.init() // execute iut: xor val to A | read (PC) from ram
                    .rPC(0x0003)
                    .rAF(res.all())
                    .rHL(test_addr)
                    .ram(test_addr, test_val),
            },
        );
    }
}

test "XOR immediate" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0xFF, 0x0F, 0xF0, 0xAA }) |test_val| {
        // Constants
        const instr: u8 = 0b11101110;

        const reg_val = 0xAA;
        var res = alu.AluRegister{
            .Hi = reg_val,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        res.xor(test_val);

        const name = try std.fmt.allocPrint(std.testing.allocator, "XOR immediate (test_val={x})", .{test_val});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
            rom,
            exram,
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
                    .rPC(0x0001)
                    .rA(reg_val),
                TestCpuState.init() // execute nop | read iut(PC) from ram
                    .rPC(0x0002)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: read val from ram(PC)
                    .rPC(0x0003)
                    .rA(reg_val),
                TestCpuState.init() // execute iut: xor val to A | read (PC) from ram
                    .rPC(0x0004)
                    .rAF(res.all()),
            },
        );
    }
}

test "Complement carry flag" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..2) |carry| {
        inline for (0..2) |halfcarry| {
            inline for (0..2) |subtract| {
                inline for (0..2) |zero| {
                    // Constants
                    const instr: u8 = 0b00111111;

                    const name = try std.fmt.allocPrint(std.testing.allocator, "Complement carry flag (C={b}) (H={b}) (N={b})", .{ carry, halfcarry, subtract });
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
                            .fC(carry)
                            .fH(halfcarry)
                            .fN(subtract)
                            .fZ(zero),
                        &[_]*TestCpuState{
                            TestCpuState.init() // read nop(PC) from ram
                                .rPC(0x0001)
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero),
                            TestCpuState.init() // execute nop | read iut(PC) from ram
                                .rPC(0x0002)
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero),
                            TestCpuState.init() // execute iut: set flags | read (PC) from ram
                                .rPC(0x0003)
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
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..2) |carry| {
        inline for (0..2) |halfcarry| {
            inline for (0..2) |subtract| {
                inline for (0..2) |zero| {
                    // Constants
                    const instr: u8 = 0b00110111;

                    const name = try std.fmt.allocPrint(std.testing.allocator, "Set carry flag (C={b}) (H={b}) (N={b})", .{ carry, halfcarry, subtract });
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
                            .fC(carry)
                            .fH(halfcarry)
                            .fN(subtract)
                            .fZ(zero),
                        &[_]*TestCpuState{
                            TestCpuState.init() // read nop(PC) from ram
                                .rPC(0x0001)
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero),
                            TestCpuState.init() // execute nop | read iut(PC) from ram
                                .rPC(0x0002)
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero),
                            TestCpuState.init() // execute iut: set flags | read (PC) from ram
                                .rPC(0x0003)
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
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b00100111;

    const test_value: u8 = 0x2F;
    const test_carry: u1 = 0;
    const test_halfcarry: u1 = 1;
    const test_subtraction: u1 = 1;

    var res = alu.AluRegister{
        .Hi = test_value,
        .Lo = alu.RegisterFlags{
            .C = test_carry,
            .H = test_halfcarry,
            .N = test_subtraction,
            .Z = 0,
            .rest = 0,
        },
    };
    res.daa();

    try run_test_case(
        "Decimal adjust accumulator",
        rom,
        exram,
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
                .rPC(0x0001)
                .rA(test_value)
                .fC(test_carry)
                .fH(test_halfcarry)
                .fN(test_subtraction),
            TestCpuState.init() // execute nop | read iut(PC) from ram
                .rPC(0x0002)
                .rA(test_value)
                .fC(test_carry)
                .fH(test_halfcarry)
                .fN(test_subtraction),
            TestCpuState.init() // execute iut: set flags | read (PC) from ram
                .rPC(0x0003)
                .rAF(res.all()),
        },
    );
}

test "Complement accumulator" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b00101111;
    const test_value = 0xAA;

    var res = alu.AluRegister{
        .Hi = test_value,
        .Lo = alu.RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    res.cpl();

    try run_test_case(
        "Complement accumulator",
        rom,
        exram,
        &[_]u8{
            0x00,
            instr,
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
            TestCpuState.init() // execute iut: complement A | read (PC) from ram
                .rPC(0x0003)
                .rAF(res.all()),
        },
    );
}

test "Inc/Dec register 16" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
                        .reg16(@intCast(reg), value),
                    &[_]*TestCpuState{
                        TestCpuState.init() // read nop(PC) from ram
                            .rPC(0x0001)
                            .reg16(@intCast(reg), value),
                        TestCpuState.init() // execute nop | read iut(PC) from ram
                            .rPC(0x0002)
                            .reg16(@intCast(reg), value),
                        TestCpuState.init() // execute iut: inc/dec register
                            .rPC(0x0002)
                            .reg16(@intCast(reg), expected),
                        TestCpuState.init() // read (PC) from ram
                            .rPC(0x0003)
                            .reg16(@intCast(reg), expected),
                    },
                );
            }
        }
    }
}

test "Add register 16" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
                                .Hi = 0,
                                .Lo = alu.RegisterFlags{
                                    .C = carry,
                                    .H = halfcarry,
                                    .N = subtract,
                                    .Z = zero,
                                    .rest = 0,
                                },
                            };
                            const expected_lo = res_1.add_return(base_lo, value_lo, 0, zero);
                            var res_2 = res_1;
                            const expected_hi = res_2.add_return(base_hi, value_hi, 1, zero);

                            const expected: u16 = (@as(u16, expected_hi) << 8) | expected_lo;

                            const name = try std.fmt.allocPrint(std.testing.allocator, "Add register 16 (val={x}) (C={b}) (Z={b}) (H={b}) (N={b})", .{ val, carry, zero, halfcarry, subtract });
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
                                    .fC(carry)
                                    .fH(halfcarry)
                                    .fN(subtract)
                                    .fZ(zero)
                                    .rHL(base)
                                    .reg16(@intCast(reg), value),
                                &[_]*TestCpuState{
                                    TestCpuState.init() // read nop(PC) from ram
                                        .rPC(0x0001)
                                        .fC(carry)
                                        .fH(halfcarry)
                                        .fN(subtract)
                                        .fZ(zero)
                                        .rHL(base)
                                        .reg16(@intCast(reg), value),
                                    TestCpuState.init() // execute nop | read iut(PC) from ram
                                        .rPC(0x0002)
                                        .fC(carry)
                                        .fH(halfcarry)
                                        .fN(subtract)
                                        .fZ(zero)
                                        .rHL(base)
                                        .reg16(@intCast(reg), value),
                                    TestCpuState.init() // execute iut: add lsb
                                        .rPC(0x0002)
                                        .fC(res_1.Lo.C)
                                        .fH(res_1.Lo.H)
                                        .fN(res_1.Lo.N)
                                        .fZ(res_1.Lo.Z)
                                        .rH(base_hi)
                                        .rL(expected_lo)
                                        .reg16(@intCast(reg), value),
                                    TestCpuState.init() // execute iut: add msb | read (PC) from ram
                                        .rPC(0x0003)
                                        .fC(res_2.Lo.C)
                                        .fH(res_2.Lo.H)
                                        .fN(res_2.Lo.N)
                                        .fZ(res_2.Lo.Z)
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
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

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
                            .Hi = 0,
                            .Lo = alu.RegisterFlags{
                                .C = carry,
                                .H = halfcarry,
                                .N = subtract,
                                .Z = zero,
                                .rest = 0,
                            },
                        };
                        const expected_lo = res_1.add_return(value_lo, value_lo, 0, zero);
                        var res_2 = res_1;
                        const expected_hi = res_2.add_return(value_hi, value_hi, 1, zero);

                        const expected: u16 = (@as(u16, expected_hi) << 8) | expected_lo;

                        const name = try std.fmt.allocPrint(std.testing.allocator, "Add register HL (val={x}) (C={b}) (Z={b}) (H={b}) (N={b})", .{ val, carry, zero, halfcarry, subtract });
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
                                .fC(carry)
                                .fH(halfcarry)
                                .fN(subtract)
                                .fZ(zero)
                                .rHL(value),
                            &[_]*TestCpuState{
                                TestCpuState.init() // read nop(PC) from ram
                                    .rPC(0x0001)
                                    .fC(carry)
                                    .fH(halfcarry)
                                    .fN(subtract)
                                    .fZ(zero)
                                    .rHL(value),
                                TestCpuState.init() // execute nop | read iut(PC) from ram
                                    .rPC(0x0002)
                                    .fC(carry)
                                    .fH(halfcarry)
                                    .fN(subtract)
                                    .fZ(zero)
                                    .rHL(value),
                                TestCpuState.init() // execute iut: add lsb
                                    .rPC(0x0002)
                                    .fC(res_1.Lo.C)
                                    .fH(res_1.Lo.H)
                                    .fN(res_1.Lo.N)
                                    .fZ(res_1.Lo.Z)
                                    .rH(value_hi)
                                    .rL(expected_lo),
                                TestCpuState.init() // execute iut: add msb | read (PC) from ram
                                    .rPC(0x0003)
                                    .fC(res_2.Lo.C)
                                    .fH(res_2.Lo.H)
                                    .fN(res_2.Lo.N)
                                    .fZ(res_2.Lo.Z)
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
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (.{ 0x00, 0x01, 0x04, 0x0F, 0xF4, 0xFF }) |e| {
        // Constants
        const instr: u8 = 0b11101000;
        const test_value: u16 = 0x100D;

        const unsigned_e: u8 = @intCast(e);

        var reg = alu.AluRegister{
            .Hi = 0x00,
            .Lo = alu.RegisterFlags{
                .C = 0,
                .H = 0,
                .N = 0,
                .Z = 0,
                .rest = 0,
            },
        };
        const expected_lo = reg.add_return(@intCast(test_value & 0xFF), unsigned_e, 0, 0);
        const expected_hi = reg.add_adj(@intCast((test_value & 0xFF00) >> 8), unsigned_e);
        const expected = (@as(u16, expected_hi) << 8) | expected_lo;

        const name = try std.fmt.allocPrint(std.testing.allocator, "Add SP relative (e={d})", .{e});
        defer std.testing.allocator.free(name);
        try run_test_case(
            name,
            rom,
            exram,
            &[_]u8{
                0x00,
                instr,
                unsigned_e,
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
                TestCpuState.init() // execute iut: read e(PC) from ram
                    .rPC(0x0003)
                    .rSP(test_value),
                TestCpuState.init() // execute iut: add e to SP_lsb
                    .rPC(0x0003)
                    .fC(reg.Lo.C)
                    .fH(reg.Lo.H)
                    .rSP(test_value),
                TestCpuState.init() // execute iut: add carry/adj to SP_msb
                    .rPC(0x0003)
                    .fC(reg.Lo.C)
                    .fH(reg.Lo.H)
                    .rSP(test_value),
                TestCpuState.init() // execute iut: assign WZ to SP | read (PC) from ram
                    .rPC(0x0004)
                    .fC(reg.Lo.C)
                    .fH(reg.Lo.H)
                    .rSP(expected),
            },
        );
    }
}
