const std = @import("std");
const lib = @import("lib");
const cli = @import("cli.zig");

var array = [_]u8{0x00} ** 0x100;

const Container = lib.dependency_container.Container(.{});
const Mmu = Container.Mmu;
const Cpu = Container.Cpu;

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

    var stdout_buffer: [1024]u8 = undefined;
    const stdout_fileno = std.fs.File.stdout();
    var stdout_file_writer = stdout_fileno.writer(&stdout_buffer);
    const writer = &stdout_file_writer.interface;

    var container = Container.init(.{
        .debugger_writer = writer,
        .breakpoint_instruction = args.@"breakpoint-instruction",
        .cartridge = .{
            .by_path = .{
                .absolute_path = "/home/dacada/Downloads/testroms/mooneye-test-suite/acceptance",
                .filename = "call_timing.gb",
            },
        },
    });

    const cart = try container.get_cartridge();

    const mmu = try container.get_mmu();
    lib.emulator.initializeMemory(Mmu, mmu);

    var cpu = try container.get_cpu();
    lib.emulator.initializeCpu(Cpu, cpu, cart.checksum);

    // This executes a nop and fetches the first instruction of the ROM
    cpu.tick();

    var emu = try container.get_emulator();
    try emu.run(true);
}
