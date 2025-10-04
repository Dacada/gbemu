const std = @import("std");
const lib = @import("lib");

const AudioBackend = lib.backend.NullAudioBackend;
const Scheduler = lib.scheduler.Scheduler;
const Cartridge = lib.cartridge.Cartridge;
const Interrupt = lib.interrupt.Interrupt;
const Joypad = lib.joypad.Joypad(Interrupt);
const Serial = lib.serial.Serial(Scheduler, Interrupt);
const Apu = lib.apu.Apu(AudioBackend);
const Timer = lib.timer.Timer(Apu, Interrupt);
const Lcd = lib.mmio.Dummy;
const BootRom = lib.mmio.Dummy;
const Mmio = lib.mmio.Mmio(Joypad, Serial, Timer, Interrupt, Apu, Lcd, BootRom);
const Ppu = lib.ppu.Ppu;
const Mmu = lib.mmu.Mmu(Cartridge, Ppu, Mmio);
const Cpu = lib.cpu.Cpu(Mmu, Interrupt);

fn print_test_result(comptime result: []const u8, comptime color: std.io.tty.Color, msg: ?[]const u8, writer: *std.Io.Writer, cfg: std.io.tty.Config) !void {
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

fn fail_test(msg: []const u8, writer: *std.Io.Writer, cfg: std.io.tty.Config) !void {
    return print_test_result("FAILURE", .red, msg, writer, cfg);
}

fn succeed_test(writer: *std.Io.Writer, cfg: std.io.tty.Config) !void {
    return print_test_result("SUCCESS", .green, null, writer, cfg);
}

fn skip_test(msg: []const u8, writer: *std.Io.Writer, cfg: std.io.tty.Config) !void {
    return print_test_result("SKIP", .cyan, msg, writer, cfg);
}

const TestResult = enum {
    success,
    failure,
    skip,
};

fn run_test(file: std.fs.File, writer: *std.Io.Writer, cfg: std.io.tty.Config) !TestResult {
    var audio_backend = AudioBackend.init();

    var cart = lib.cartridge.Cartridge.fromFile(file) catch |e| {
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

    var sched = Scheduler.init();
    var intr = Interrupt.init();
    var joypad = Joypad.init(&intr);
    var serial = Serial.init(&sched, &intr);
    var apu = Apu.init(&audio_backend);
    var timer = Timer.init(&apu, &intr);
    var lcd = Lcd{};
    var boot_rom = BootRom{};
    var ppu = Ppu.init();
    var mmio = Mmio.init(&joypad, &serial, &timer, &intr, &apu, &lcd, &boot_rom);
    var mmu = Mmu.init(&cart, &ppu, &mmio);
    var cpu = Cpu.init(&mmu, &intr, 0x40);
    lib.emulator.initializeCpu(Cpu, &cpu, cart.checksum);

    var count: u32 = 0;
    while (true) {
        cpu.tick();
        count += 1;

        var failure: ?[]const u8 = null;
        if (cpu.flags.breakpoint) {
            if (cpu.reg.bc.hi == 3 and cpu.reg.bc.lo == 5 and cpu.reg.de.hi == 8 and cpu.reg.de.lo == 13 and cpu.reg.hl.hi == 21 and cpu.reg.hl.lo == 34) {
                try succeed_test(writer, cfg);
                return .success;
            } else {
                failure = "Breakpoint reached with invalid register values";
                std.debug.print("B/C/D/E/H/L = {d}/{d}/{d}/{d}/{d}/{d}\n", .{ cpu.reg.bc.hi, cpu.reg.bc.lo, cpu.reg.de.hi, cpu.reg.de.lo, cpu.reg.hl.hi, cpu.reg.hl.lo });
            }
        }
        if (cpu.flags.illegal) {
            failure = "Illegal instruction decoded";
        }
        if (cpu.flags.double_halt) {
            failure = "Double halt bug triggered";
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

fn listFiles(dir: std.fs.Dir, writer: *std.Io.Writer, cfg: std.io.tty.Config) !struct { usize, usize, usize } {
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
                const next_successful, const next_failed, const next_skipped = try listFiles(next_dir, writer, cfg);
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
    var stdout_buffer: [1024]u8 = undefined;
    const stdout_fileno = std.fs.File.stdout();
    var stdout_file_writer = stdout_fileno.writer(&stdout_buffer);
    const tty_config = std.Io.tty.detectConfig(stdout_fileno);
    const writer = &stdout_file_writer.interface;

    var dir = try std.fs.cwd().openDir("testroms", .{ .iterate = true });
    defer dir.close();
    const successful, const failed, const skipped = try listFiles(dir, writer, tty_config);

    try writer.print("Result:\n  Pass: {d}\n  Fail: {d}\n  Skip: {d}\nTotal: {d}\n", .{ successful, failed, skipped, successful + failed + skipped });
    try writer.flush();
}
