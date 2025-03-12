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
        const signed_e: i8 = @bitCast(unsigned_e);
        const test_value_after_second_add_signed, _ = @addWithOverflow(@as(i16, test_value), signed_e);
        const test_value_after_second_add: u16 = @bitCast(test_value_after_second_add_signed);
        const test_value_after_first_add: u16 = test_value_after_second_add & 0x00FF;
        _, const halfcarry_flag = @addWithOverflow(@as(u4, test_value & 0x000F), @as(u4, @intCast(unsigned_e & 0x0F)));
        _, const carry_flag = @addWithOverflow(@as(u8, test_value & 0x00FF), @as(u8, unsigned_e));

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
                    .fC(carry_flag)
                    .fH(halfcarry_flag)
                    .rSP(test_value)
                    .rHL(test_value_after_first_add),
                TestCpuState.init() // read (PC) | execute iut: add carry to SP_msb
                    .rPC(0x0004)
                    .fC(carry_flag)
                    .fH(halfcarry_flag)
                    .rSP(test_value)
                    .rHL(test_value_after_second_add),
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
                const carry_for_test_add: u1 = carry_flag & carry_instr;

                const instr: u8 = 0b10000000 | reg_u8 | (@as(u8, carry_instr) << 3);
                const test_val: u8 = 0xBB;

                const res = alu.AluOp8Bit.add(test_val, val_u8, carry_for_test_add);

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
                            .rA(res.result)
                            .reg(reg_u3, val_u8)
                            .fC(res.carry)
                            .fH(res.halfcarry)
                            .fN(res.subtraction)
                            .fZ(res.zero),
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
            const carry_for_test_add: u1 = carry_flag & carry_instr;

            const instr: u8 = 0b10000111 | (@as(u8, carry_instr) << 3);

            const res = alu.AluOp8Bit.add(val_u8, val_u8, carry_for_test_add);

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
                        .rA(res.result)
                        .fC(res.carry)
                        .fH(res.halfcarry)
                        .fN(res.subtraction)
                        .fZ(res.zero),
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
            const carry_for_test_add: u1 = carry_flag & carry_instr;

            const test_val: u8 = 0xAA;
            const test_addr: u16 = 0xD00D;
            const instr: u8 = 0b10000110 | (@as(u8, carry_instr) << 3);

            const res = alu.AluOp8Bit.add(test_val, val_u8, carry_for_test_add);

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
                        .rA(res.result)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr)
                        .fC(res.carry)
                        .fH(res.halfcarry)
                        .fN(res.subtraction)
                        .fZ(res.zero),
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
            const carry_for_test_add: u1 = carry_flag & carry_instr;

            const test_val: u8 = 0xAA;
            const instr: u8 = 0b11000110 | (@as(u8, carry_instr) << 3);

            const res = alu.AluOp8Bit.add(test_val, val_u8, carry_for_test_add);

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
                        .rA(res.result)
                        .fC(res.carry)
                        .fH(res.halfcarry)
                        .fN(res.subtraction)
                        .fZ(res.zero),
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
                const carry_for_test_sub: u1 = carry_flag & carry_instr;

                const instr: u8 = 0b10010000 | reg_u8 | (@as(u8, carry_instr) << 3);
                const test_val: u8 = 0xBB;

                const res = alu.AluOp8Bit.sub(test_val, val_u8, carry_for_test_sub);

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
                            .rA(res.result)
                            .reg(reg_u3, val_u8)
                            .fC(res.carry)
                            .fH(res.halfcarry)
                            .fN(res.subtraction)
                            .fZ(res.zero),
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
            const carry_for_test_sub: u1 = carry_flag & carry_instr;

            const instr: u8 = 0b10010111 | (@as(u8, carry_instr) << 3);

            const res = alu.AluOp8Bit.sub(val_u8, val_u8, carry_for_test_sub);

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
                        .rA(res.result)
                        .fC(res.carry)
                        .fH(res.halfcarry)
                        .fN(res.subtraction)
                        .fZ(res.zero),
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
            const carry_for_test_sub: u1 = carry_flag & carry_instr;

            const test_val: u8 = 0xAA;
            const test_addr: u16 = 0xD00D;
            const instr: u8 = 0b10010110 | (@as(u8, carry_instr) << 3);

            const res = alu.AluOp8Bit.sub(test_val, val_u8, carry_for_test_sub);

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
                        .rA(res.result)
                        .ram(test_addr, val_u8)
                        .rHL(test_addr)
                        .fC(res.carry)
                        .fH(res.halfcarry)
                        .fN(res.subtraction)
                        .fZ(res.zero),
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
            const carry_for_test_sub: u1 = carry_flag & carry_instr;

            const test_val: u8 = 0xAA;
            const instr: u8 = 0b11010110 | (@as(u8, carry_instr) << 3);

            const res = alu.AluOp8Bit.sub(test_val, val_u8, carry_for_test_sub);

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
                        .rA(res.result)
                        .fC(res.carry)
                        .fH(res.halfcarry)
                        .fN(res.subtraction)
                        .fZ(res.zero),
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

            const res = alu.AluOp8Bit.sub(test_val, val_u8, 0);

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
                        .rA(test_val)
                        .reg(reg_u3, val_u8)
                        .fC(res.carry)
                        .fH(res.halfcarry)
                        .fN(res.subtraction)
                        .fZ(res.zero),
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

        const res = alu.AluOp8Bit.sub(val_u8, val_u8, 0);

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
                    .fC(res.carry)
                    .fH(res.halfcarry)
                    .fN(res.subtraction)
                    .fZ(res.zero),
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

        const res = alu.AluOp8Bit.sub(test_val, val_u8, 0);

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
                    .rA(test_val)
                    .ram(test_addr, val_u8)
                    .rHL(test_addr)
                    .fC(res.carry)
                    .fH(res.halfcarry)
                    .fN(res.subtraction)
                    .fZ(res.zero),
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

        const res = alu.AluOp8Bit.sub(test_val, val_u8, 0);

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
                    .fC(res.carry)
                    .fH(res.halfcarry)
                    .fN(res.subtraction)
                    .fZ(res.zero),
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

                const res = if (incdec == 0)
                    alu.AluOp8Bit.add(test_val, 1, 0)
                else
                    alu.AluOp8Bit.sub(test_val, 1, 0);

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
                            .reg(reg, res.result)
                            .fC(0)
                            .fN(incdec)
                            .fH(res.halfcarry)
                            .fZ(res.zero),
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

            const res = if (incdec == 0)
                alu.AluOp8Bit.add(test_val, 1, 0)
            else
                alu.AluOp8Bit.sub(test_val, 1, 0);

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
                        .ram(test_addr, res.result)
                        .fC(0)
                        .fH(res.halfcarry)
                        .fN(incdec)
                        .fZ(res.zero),
                    TestCpuState.init() // read (PC)
                        .rPC(0x0003)
                        .rHL(test_addr)
                        .ram(test_addr, res.result)
                        .fC(0)
                        .fH(res.halfcarry)
                        .fN(incdec)
                        .fZ(res.zero),
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
            const res = alu.AluOp8Bit.and_(reg_val, test_val);

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
                        .rA(res.result)
                        .fC(0)
                        .fH(1)
                        .fN(0)
                        .fZ(res.zero),
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

        const res = alu.AluOp8Bit.and_(test_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(1)
                    .fN(0)
                    .fZ(res.zero),
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
        const res = alu.AluOp8Bit.and_(reg_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(1)
                    .fN(0)
                    .fZ(res.zero)
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
        const res = alu.AluOp8Bit.and_(reg_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(1)
                    .fN(0)
                    .fZ(res.zero),
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
            const res = alu.AluOp8Bit.or_(reg_val, test_val);

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
                        .rA(res.result)
                        .fC(0)
                        .fH(0)
                        .fN(0)
                        .fZ(res.zero),
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

        const res = alu.AluOp8Bit.or_(test_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(0)
                    .fN(0)
                    .fZ(res.zero),
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
        const res = alu.AluOp8Bit.or_(reg_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(0)
                    .fN(0)
                    .fZ(res.zero)
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
        const res = alu.AluOp8Bit.or_(reg_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(0)
                    .fN(0)
                    .fZ(res.zero),
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
            const res = alu.AluOp8Bit.xor_(reg_val, test_val);

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
                        .rA(res.result)
                        .fC(0)
                        .fH(0)
                        .fN(0)
                        .fZ(res.zero),
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

        const res = alu.AluOp8Bit.xor_(test_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(0)
                    .fN(0)
                    .fZ(res.zero),
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
        const res = alu.AluOp8Bit.xor_(reg_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(0)
                    .fN(0)
                    .fZ(res.zero)
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
        const res = alu.AluOp8Bit.xor_(reg_val, test_val);

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
                    .rA(res.result)
                    .fC(0)
                    .fH(0)
                    .fN(0)
                    .fZ(res.zero),
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

    const res = alu.AluOp8Bit.daa(test_value, test_carry, test_halfcarry, test_subtraction);

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
                .rA(res.result)
                .fC(res.carry)
                .fH(res.halfcarry)
                .fN(res.subtraction)
                .fZ(res.zero),
        },
    );
}
