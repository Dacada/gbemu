const std = @import("std");
const lib = @import("lib");
const cli = @import("cli.zig");

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
const Debugger = lib.debugger.Debugger(Cpu, Mmu, std.fs.File.Writer);
const Emulator = lib.emulator.Emulator(Cpu, Apu, Ppu, Timer, Scheduler, Debugger);

var array = [_]u8{0x00} ** 0x100;

fn makeCart() !Cartridge {
    var dir = try std.fs.openDirAbsolute("/home/dacada/Downloads/testroms/mooneye-test-suite/acceptance", .{});
    defer dir.close();
    const file = try dir.openFile("call_timing.gb", .{});
    defer file.close();
    return Cartridge.fromFile(file);
}

pub fn main() !void {
    const parser = cli.ArgParser(.{
        .error_exit_code = 1,
        .optional = &.{
            cli.ArgParserParamDefinition.init(
                "breakpoint-instruction",
                "This opcode will be treated like a software breakpoint.",
                ?u8,
                null,
            ),
        },
    });

    var argiter = std.process.args();
    const args = parser.parse(&argiter);

    const stdout = std.io.getStdOut();
    const writer = stdout.writer();

    var audioBackend = AudioBackend.init();

    var cart = try makeCart();

    var sched = Scheduler.init();

    var intr = Interrupt.init();
    var joypad = Joypad.init(&intr);
    var serial = Serial.init(&sched, &intr);
    var apu = Apu.init(&audioBackend);
    var timer = Timer.init(&apu, &intr);
    var lcd = Lcd{};
    var boot_rom = BootRom{};
    var ppu = Ppu.init();

    var mmio = Mmio.init(
        &joypad,
        &serial,
        &timer,
        &intr,
        &apu,
        &lcd,
        &boot_rom,
    );

    var mmu = Mmu.init(&cart, &ppu, &mmio);
    lib.emulator.initialize_memory(Mmu, &mmu);

    var cpu = Cpu.init(&mmu, &intr, args.@"breakpoint-instruction");
    lib.emulator.initialize_cpu(Cpu, &cpu, cart.checksum);

    // This executes a nop and fetches the first instruction of the ROM
    cpu.tick();

    var dbg = Debugger.init(&cpu, &mmu, writer);

    var emu = Emulator.init(&cpu, &apu, &ppu, &timer, &sched, &dbg);
    try emu.run(true);
}
