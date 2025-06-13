const std = @import("std");
const lib = @import("lib");

fn print_test_result(comptime result: []const u8, comptime color: std.io.tty.Color, msg: ?[]const u8, writer: anytype, cfg: std.io.tty.Config) !void {
    try writer.writeAll("  > ");
    try cfg.setColor(writer, color);
    try writer.writeAll(result);
    try cfg.setColor(writer, std.io.tty.Color.reset);
    if (msg) |m| {
        try writer.print(" - {s}\n", .{m});
    } else {
        try writer.writeAll("\n");
    }
}

fn fail_test(msg: []const u8, writer: anytype, cfg: std.io.tty.Config) !void {
    return print_test_result("FAILURE", .red, msg, writer, cfg);
}

fn succeed_test(writer: anytype, cfg: std.io.tty.Config) !void {
    return print_test_result("SUCCESS", .green, null, writer, cfg);
}

fn skip_test(msg: []const u8, writer: anytype, cfg: std.io.tty.Config) !void {
    return print_test_result("SKIP", .cyan, msg, writer, cfg);
}

const TestResult = enum {
    success,
    failure,
    skip,
};

fn run_test(file: std.fs.File, writer: anytype, cfg: std.io.tty.Config) !TestResult {
    var cartridge = lib.cartridge.Cartridge.fromFile(file) catch |e| {
        switch (e) {
            lib.cartridge.CartridgeHeaderParseError.NoHeader,
            lib.cartridge.CartridgeHeaderParseError.NoRom,
            lib.cartridge.CartridgeHeaderParseError.UnsupportedCartridgeType,
            => {
                try fail_test("Invalid cartridge", writer, cfg);
                return .failure;
            },
            else => {
                return e;
            },
        }
    };

    var ppu = lib.ppu.Ppu{};
    var mmio = lib.mmio.Mmio{};
    var mmu = lib.mmu.Mmu.init(&ppu, &mmio);
    var cpu = lib.cpu.Cpu.init(&mmu, 0x40);
    lib.emulator.initialize_cpu(&cpu, cartridge.checksum);
    lib.emulator.initialize_mmu(&mmu);
    mmu.setCartridge(&cartridge);

    var count: u32 = 0;
    while (true) {
        cpu.tick();
        count += 1;

        var failure: ?[]const u8 = null;
        if (cpu.breakpointHappened()) {
            if (cpu.reg.BC.Hi == 3 and cpu.reg.BC.Lo == 5 and cpu.reg.DE.Hi == 8 and cpu.reg.DE.Lo == 13 and cpu.reg.HL.Hi == 21 and cpu.reg.HL.Lo == 34) {
                try succeed_test(writer, cfg);
                return .success;
            } else {
                failure = "Breakpoint reached with invalid register values";
                std.debug.print("B/C/D/E/H/L = {d}/{d}/{d}/{d}/{d}/{d}\n", .{ cpu.reg.BC.Hi, cpu.reg.BC.Lo, cpu.reg.DE.Hi, cpu.reg.DE.Lo, cpu.reg.HL.Hi, cpu.reg.HL.Lo });
            }
        }
        if (cpu.illegalInstructionExecuted()) {
            failure = "Illegal instruction decoded";
        }
        if (count > 1_000_000) {
            failure = "Timeout";
        }

        if (failure) |text| {
            try fail_test(text, writer, cfg);
            return .failure;
        }
    }
}

fn list_files(dir: std.fs.Dir, writer: anytype, cfg: std.io.tty.Config) !struct { usize, usize, usize } {
    var successful: usize = 0;
    var failed: usize = 0;
    var skipped: usize = 0;

    var it = dir.iterate();
    while (try it.next()) |item| {
        switch (item.kind) {
            .file => {
                if (std.mem.endsWith(u8, item.name, ".gb")) {
                    try writer.writeAll("> Testing ");
                    try cfg.setColor(writer, std.io.tty.Color.blue);
                    try writer.print("{s}", .{item.name});
                    try cfg.setColor(writer, std.io.tty.Color.reset);
                    try writer.writeAll(" ...\n");
                    const file = try dir.openFile(item.name, .{});
                    defer file.close();
                    switch (try run_test(file, writer, cfg)) {
                        .success => successful += 1,
                        .failure => failed += 1,
                        .skip => skipped += 1,
                    }
                }
            },
            .directory => {
                var next_dir = try dir.openDir(item.name, .{ .iterate = true });
                defer next_dir.close();
                const next_successful, const next_failed, const next_skipped = try list_files(next_dir, writer, cfg);
                successful += next_successful;
                failed += next_failed;
                skipped += next_skipped;
            },
            else => {},
        }
    }

    return .{ successful, failed, skipped };
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    const ttyConfig = std.io.tty.detectConfig(stdout);

    var dir = try std.fs.cwd().openDir("testroms", .{ .iterate = true });
    defer dir.close();
    const successful, const failed, const skipped = try list_files(dir, writer, ttyConfig);

    try writer.print("Result:\n  Pass: {d}\n  Fail: {d}\n  Skip: {d}\nTotal: {d}\n", .{ successful, failed, skipped, successful + failed + skipped });
}
