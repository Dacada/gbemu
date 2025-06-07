const std = @import("std");
const lib = @import("lib");
const cli = @import("cli.zig");

fn makeRom() !lib.rom.Rom {
    var dir = try std.fs.openDirAbsolute("/home/dacada/Downloads/testroms/mooneye-test-suite/acceptance", .{});
    defer dir.close();
    const file = try dir.openFile("call_timing.gb", .{});
    defer file.close();
    return lib.rom.Rom.fromFile(file);
}

pub fn main() !void {
    const parser = cli.ArgParser(.{
        .error_exit_code = 1,
        .optional = &.{
            cli.ArgParserParamDefinition.init(
                "breakpoint_instruction",
                "This opcode will be treated like a software breakpoint.",
                ?u8,
                null,
            ),
        },
    });

    var argiter = std.process.args();
    const args = parser.parse(&argiter);

    var mmu = lib.mmu.Mmu.init();
    var cpu = lib.cpu.Cpu.init(&mmu, args.breakpoint_instruction);
    var dbg = lib.debugger.Debugger.init(&cpu, std.io.getStdOut());
    var emu = lib.emulator.Emulator.init(&mmu, &cpu, &dbg);

    const rom = try makeRom();
    emu.mapRom(&rom);

    try emu.run(true);
}
