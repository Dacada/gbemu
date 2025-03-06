const std = @import("std");
const testutil = @import("testutil.zig");
const run_test_case = testutil.run_test_case;
const TestCpuState = testutil.TestCpuState;

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

            const name = try std.fmt.allocPrint(std.testing.allocator, "LD register from={b} to={b}", .{ from, to });
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

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD register indirect from={b}", .{from});
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

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD register indirect to={b}", .{to});
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

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD immediate to={b}", .{to});
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
        "LD immediate indirect",
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

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD accumulator indirect from={b}", .{from});
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

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD accumulator indirect from={b}", .{from});
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
        "LD to accumulator direct",
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
        "LD from accumulator direct",
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

test "ldh to accumulator indirect" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11110010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try run_test_case(
        "LDH to accumulator indirect",
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
            TestCpuState.init() // load nop
                .rPC(0x0001)
                .rC(test_addr & 0xFF)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop and load instruction under test
                .rPC(0x0002)
                .rC(test_addr & 0xFF)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute instruction under test: load from ram
                .rPC(0x0002)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // fetch illegal instruction
                .rPC(0x0003)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "ldh from accumulator indirect" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11100010;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try run_test_case(
        "LDH from accumulator indirect",
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
            TestCpuState.init() // load nop
                .rPC(0x0001)
                .rC(test_addr & 0xFF)
                .rA(test_value),
            TestCpuState.init() // execute nop and load instruction under test
                .rPC(0x0002)
                .rC(test_addr & 0xFF)
                .rA(test_value),
            TestCpuState.init() // execute instruction under test: load from ram
                .rPC(0x0002)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // fetch illegal instruction
                .rPC(0x0003)
                .rC(test_addr & 0xFF)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "ldh to accumulator direct" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11110000;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try run_test_case(
        "LDH to accumulator direct",
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
            TestCpuState.init() // load nop
                .rPC(0x0001)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute nop and load instruction under test
                .rPC(0x0002)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute instruction under test: load address from ram
                .rPC(0x0003)
                .ram(test_addr, test_value),
            TestCpuState.init() // execute instruction under test: load value from ram
                .rPC(0x0003)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // fetch illegal instruction
                .rPC(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "ldh from accumulator direct" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    // Constants
    const instr: u8 = 0b11100000;
    const test_value: u8 = 0xFF;
    const test_addr: u16 = 0xFFAA;

    try run_test_case(
        "LDH from accumulator direct",
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
            TestCpuState.init() // load nop
                .rPC(0x0001)
                .rA(test_value),
            TestCpuState.init() // execute nop and load instruction under test
                .rPC(0x0002)
                .rA(test_value),
            TestCpuState.init() // execute instruction under test: load address from ram
                .rPC(0x0003)
                .rA(test_value),
            TestCpuState.init() // execute instruction under test: load value to ram
                .rPC(0x0003)
                .rA(test_value)
                .ram(test_addr, test_value),
            TestCpuState.init() // fetch illegal instruction
                .rPC(0x0004)
                .rA(test_value)
                .ram(test_addr, test_value),
        },
    );
}

test "ld to accumulator indirect HL" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..2) |decinc| {
        // Constants
        const instr: u8 = @intCast(0b00101010 | (decinc << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xFFAA;
        const test_addr_next = switch (decinc) {
            1 => test_addr - 1,
            0 => test_addr + 1,
            else => unreachable,
        };

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD to accumulator indirect dec/inc={b}", .{decinc});
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
                TestCpuState.init() // load nop
                    .rPC(0x0001)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute nop and load instruction under test
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .ram(test_addr, test_value),
                TestCpuState.init() // execute instruction under test: load value from ram
                    .rPC(0x0002)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // fetch illegal instruction
                    .rPC(0x0003)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "ld from accumulator indirect HL" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..2) |decinc| {
        // Constants
        const instr: u8 = @intCast(0b00100010 | (decinc << 4));
        const test_value: u8 = 0xFF;
        const test_addr: u16 = 0xFFAA;
        const test_addr_next = switch (decinc) {
            1 => test_addr - 1,
            0 => test_addr + 1,
            else => unreachable,
        };

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD from accumulator indirect dec/inc={b}", .{decinc});
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
                TestCpuState.init() // load nop
                    .rPC(0x0001)
                    .rHL(test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute nop and load instruction under test
                    .rPC(0x0002)
                    .rHL(test_addr)
                    .rA(test_value),
                TestCpuState.init() // execute instruction under test: load value to ram
                    .rPC(0x0002)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
                TestCpuState.init() // fetch illegal instruction
                    .rPC(0x0003)
                    .rHL(test_addr_next)
                    .rA(test_value)
                    .ram(test_addr, test_value),
            },
        );
    }
}

test "ld immediate 16bit" {
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);

    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);

    inline for (0..(0b11 + 1)) |reg| {
        // Constants
        const instr: u8 = @intCast(0b00000001 | (reg << 4));
        const test_value: u16 = 0xABCD;

        const name = try std.fmt.allocPrint(std.testing.allocator, "LD immediate 16bit reg={b}", .{reg});
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
                TestCpuState.init() // load nop
                    .rPC(0x0001),
                TestCpuState.init() // execute nop and load instruction under test
                    .rPC(0x0002),
                TestCpuState.init() // execute instruction under test: load lsb from program
                    .rPC(0x0003),
                TestCpuState.init() // execute instruction under test: load msb from program
                    .rPC(0x0004),
                TestCpuState.init() // load illegal instruction and store value to register pair
                    .rPC(0x0005)
                    .reg16(reg, test_value),
            },
        );
    }
}
