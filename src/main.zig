const std = @import("std");
const lib = @import("lib");

var array = [_]u8{0x00} ** 0x100;

const Container = lib.dependency_container.Container(.{});
const Mmu = Container.Mmu;
const Cpu = Container.Cpu;

pub fn main(init: std.process.Init) !void {
    const breakpoint_instruction: ?u8 = null;

    var stdout_buffer: [1024]u8 = undefined;
    const stdout_fileno = std.Io.File.stdout();
    var stdout_file_writer = stdout_fileno.writer(init.io, &stdout_buffer);
    const writer = &stdout_file_writer.interface;

    const romDir = try std.Io.Dir.openDirAbsolute(init.io, "/home/dacada/Downloads/testroms/mooneye-test-suite/acceptance", .{});
    defer romDir.close(init.io);
    const romFile = try romDir.openFile(init.io, "call_timing.gb", .{});
    defer romFile.close(init.io);
    var romBuffer: [32 * 1024]u8 = undefined;
    const read = try romFile.readPositionalAll(init.io, &romBuffer, 0);
    if (read != romBuffer.len) {
        @panic("invalid rom?");
    }

    var container = Container.init();
    var debugger = container.get_debugger();
    debugger.setWriter(writer);

    const cart = container.get_cartridge();
    try cart.loadFromBuffer(&romBuffer);

    const mmu = container.get_mmu();
    lib.emulator.initializeMemory(Mmu, mmu);

    var cpu = container.get_cpu();
    cpu.setBreakpointInstruction(breakpoint_instruction);
    lib.emulator.initializeCpu(Cpu, cpu, cart.checksum);

    // This executes a nop and fetches the first instruction of the ROM
    cpu.tick();

    var emu = container.get_emulator();
    try emu.run(true);
}
