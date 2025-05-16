const std = @import("std");
const lib = @import("lib");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var emulator = lib.emulator.Emulator.init(.{
        .breakpoint_instruction = 0x40,
    });

    const code =
        \\ LD A, 2
        \\ LD B, 2
        \\ ADD B
        \\ LD B, B
    ;
    const program = try lib.assembler.translate(code, allocator);
    emulator.mmu.mapRom(program);

    emulator.run() catch {};

    const stdout = std.io.getStdOut().writer();
    try stdout.print("2+2={d}\n", .{emulator.cpu.reg.AF.Hi});
}
