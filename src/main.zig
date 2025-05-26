const std = @import("std");
const lib = @import("lib");
const cli = @import("cli.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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

    var emulator = lib.emulator.Emulator.init(.{
        .breakpoint_instruction = args.breakpoint_instruction,
    });

    const code =
        \\ LD A, 2
        \\ LD B, 2
        \\ ADD B
        \\ LD B, B
    ;
    const program = try lib.assembler.translate(code, allocator);

    //const rom = lib.rom.Rom.fromFile("/home/dacada/Downloads/testroms/mooneye-test-suite/acceptance/boot_regs-dmgABC.gb");
    const rom = try lib.rom.Rom.fromBinary(allocator, program, "test", 0x0100);
    defer rom.deinit(allocator);
    emulator.mapRom(&rom);
    emulator.run() catch {};

    const stdout = std.io.getStdOut().writer();
    try stdout.print("2+2={d}\n", .{emulator.cpu.reg.AF.Hi});
}
