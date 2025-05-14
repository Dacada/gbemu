const std = @import("std");
const mmu = @import("mmu.zig");
const cpu = @import("cpu.zig");

pub const EmulatorError = error{
    IllegalMemoryAccess,
    IllegalInstruction,
};

pub const Emulator = struct {
    mmu: mmu.Mmu,
    cpu: cpu.Cpu,

    pub fn init() Emulator {
        const m = mmu.Mmu.init();
        const c = cpu.Cpu.init(m);
        return Emulator{
            .mmu = m,
            .cpu = c,
        };
    }

    pub fn run(self: *Emulator) EmulatorError {
        while (true) {
            self.cpu.tick();
            if (self.cpu.illegalInstructionExecuted()) {
                return EmulatorError.IllegalInstruction;
            }
            if (self.mmu.illegalMemoryOperationHappened()) {
                return EmulatorError.IllegalMemoryAccess;
            }
        }
    }
};
