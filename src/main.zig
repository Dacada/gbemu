const std = @import("std");
const lib = @import("lib");
const cli = @import("cli.zig");

var array = [_]u8{0x00} ** 0x100;

fn makeRom() !lib.cartridge.Cartridge {
    var dir = try std.fs.openDirAbsolute("/home/dacada/Downloads/testroms/mooneye-test-suite/acceptance", .{});
    defer dir.close();
    const file = try dir.openFile("call_timing.gb", .{});
    defer file.close();
    return lib.cartridge.Cartridge.fromFile(file);
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

    const cartridge = try makeRom();
    const ppu = lib.ppu.Ppu.init();
    var mmio = lib.mmio.Mmio{
        .joypad = lib.memory.SimpleMemory(false, &array, null).memory(),
        .serial = lib.memory.SimpleMemory(false, &array, null).memory(),
        .timer = lib.memory.SimpleMemory(false, &array, null).memory(),
        .interrupts = lib.memory.SimpleMemory(false, &array, null).memory(),
        .audio = lib.memory.SimpleMemory(false, &array, null).memory(),
        .wave = lib.memory.SimpleMemory(false, &array, null).memory(),
        .lcd = lib.memory.SimpleMemory(false, &array, null).memory(),
        .boot_rom = lib.memory.SimpleMemory(false, &array, null).memory(),
    };
    var mmu = lib.mmu.Mmu{
        .cartRom = cartridge.rom,
        .cartRam = cartridge.ram,
        .vram = ppu.vram,
        .oam = ppu.oam,
        .forbidden = ppu.forbidden,
        .mmio = mmio.memory(),
    };
    lib.emulator.initialize_memory(mmu.memory());
    var mem = mmu.memory();
    var cpu = lib.cpu.Cpu.init(&mem, args.@"breakpoint-instruction");
    lib.emulator.initialize_cpu(&cpu, cartridge.checksum);
    var dbg = lib.debugger.Debugger(lib.cpu.Cpu, @TypeOf(writer)).init(&cpu, writer);
    var emu = lib.emulator.Emulator(@TypeOf(dbg)).init(&cpu, &dbg);

    try emu.run(true);
}
