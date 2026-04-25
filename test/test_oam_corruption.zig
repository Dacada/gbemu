const std = @import("std");
const lib = @import("lib");

const Container = lib.dependency_container.Container(.{
    .cpu = .real,
    .ppu = .real,

    .mmu = .real,
    .interrupt = .mock,

    .apu = .mock,
    .timer = .dummy,
    .scheduler = .mock,
    .debugger = .mock,
    .video_backend = .mock_nil,
    .cartridge = .mock,
    .mmio = .dummy,
});
const Ppu = Container.Ppu;
const Cpu = Container.Cpu;
const Mmu = Container.Mmu;
const Emulator = Container.Emulator;

// LLM generated from documentation as a blackboxish kind of test
fn testCorruptOam(arr: []u8, row: u8, comptime operation: enum { write, read }) void {
    const row_size = 8; // 4 words * 2 bytes
    const row_index: usize = row;

    // First row (row 0) is not corrupted
    if (row_index == 0) return;

    const cur_base = row_index * row_size;
    const prev_base = (row_index - 1) * row_size;

    // Safety (only for tests, but still useful)
    if (cur_base + row_size > arr.len) return;
    if (prev_base + row_size > arr.len) return;

    // --- Load words (little endian) ---
    const a_lo = arr[cur_base + 0];
    const a_hi = arr[cur_base + 1];
    const a: u16 = (@as(u16, a_hi) << 8) | a_lo;

    const b_lo = arr[prev_base + 0];
    const b_hi = arr[prev_base + 1];
    const b: u16 = (@as(u16, b_hi) << 8) | b_lo;

    const c_lo = arr[prev_base + 4];
    const c_hi = arr[prev_base + 5];
    const c: u16 = (@as(u16, c_hi) << 8) | c_lo;

    // --- Apply corruption formula to first word ---
    const result: u16 = switch (operation) {
        .write => ((a ^ c) & (b ^ c)) ^ c,
        .read => b | (a & c),
    };

    arr[cur_base + 0] = @intCast(result & 0xFF);
    arr[cur_base + 1] = @intCast((result >> 8) & 0xFF);

    // --- Copy last three words from previous row ---
    // words 1–3 → bytes 2..8
    for (0..3) |i| {
        const src = prev_base + 2 + i * 2;
        const dst = cur_base + 2 + i * 2;

        arr[dst + 0] = arr[src + 0];
        arr[dst + 1] = arr[src + 1];
    }
}

fn setup_for_test(cpu: *Cpu, ppu: *Ppu, mmu: *Mmu, code: []const u8, oam_row: u5, oam_buffer: []u8) !void {
    // write a random pattern for oam
    var prng = std.Random.DefaultPrng.init(0x4242);
    var rand = prng.random();
    rand.bytes(oam_buffer);

    // copy it into oam
    for (0..0xA0) |addr| {
        Ppu.Oam.poke(ppu, @intCast(addr), oam_buffer[addr]);
    }

    // set the ppu as if it was in mode 2 reading from oam
    ppu.mode = .Mode2;
    ppu.current_oam_row = oam_row;

    // assemble and write the program to memory
    const program = try lib.assembler.translate(code, std.testing.allocator, 0x100);
    defer std.testing.allocator.free(program);
    for (program, 0..) |b, i| {
        mmu.write(@intCast(i + 0x100), b);
    }

    // initialize the cpu
    lib.emulator.initializeCpu(Cpu, cpu, 0);
}

fn run_until_breakpoint(cpu: *Cpu, ppu: *Ppu, emulator: *Emulator, dumped_oam: []u8) !void {
    // get the emulator and run it until we reach a breakpoint
    var count: usize = 0;
    while (!cpu.getFlags().breakpoint) {
        try std.testing.expect(!try emulator.tick());

        // limit cycles
        count += 1;
        try std.testing.expect(count < 1000);
    }

    // dump oam
    for (0..0xA0) |addr| {
        dumped_oam[addr] = Ppu.Oam.peek(ppu, @intCast(addr));
    }
}

fn normalMemoryAccess(which: enum { read, write }) !void {
    // a program that executes a read/write into OAM
    const code = switch (which) {
        .write =>
        \\ LD A, 42
        \\ LD (0xFE50), A
        \\ LD B, B  ;; breakpoint
        ,
        .read =>
        \\ LD A, (0xFE50)
        \\ LD B, B  ;; breakpoint
        ,
    };

    var expected_oam: [0xA0]u8 = undefined;
    var result_oam: [0xA0]u8 = undefined;

    for (0..20) |oam_row_iter| {
        var container = Container.init(.{
            .breakpoint_instruction = 0x40,
        });

        const cpu = try container.get_cpu();
        const ppu = try container.get_ppu();
        const mmu = try container.get_mmu();
        const emulator = try container.get_emulator();

        const oam_row: u5 = @intCast(oam_row_iter);

        try setup_for_test(cpu, ppu, mmu, code, oam_row, &expected_oam);

        // alter the expected oam so it holds our expected pattern
        switch (which) {
            .read => testCorruptOam(&expected_oam, oam_row, .read),
            .write => testCorruptOam(&expected_oam, oam_row, .write),
        }

        try run_until_breakpoint(cpu, ppu, emulator, &result_oam);

        // compare dumped oam with expected oam
        try std.testing.expectEqualSlices(u8, &expected_oam, &result_oam);
    }
}

test "normal write" {
    try normalMemoryAccess(.write);
}

test "normal read" {
    try normalMemoryAccess(.read);
}
