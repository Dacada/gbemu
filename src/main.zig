const std = @import("std");
const lib = @import("lib");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var mmu = try lib.mmu.Mmu.init(allocator);
    mmu.zeroize();
    var cpu = lib.cpu.Cpu.init(mmu);
    cpu.zeroize_regs();

    const code =
        \\ LD A, 2
        \\ LD B, 2
        \\ ADD B
    ;

    const program = try lib.assembler.translate(code, allocator);
    for (program, 0..) |instr, idx| {
        mmu.memory[idx] = instr;
    }
    mmu.memory[program.len] = 0xFD;

    while (true) {
        cpu.tick();
        if (cpu.illegalInstructionExecuted) {
            break;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("2+2={d}\n", .{cpu.reg.AF.Hi});
}
