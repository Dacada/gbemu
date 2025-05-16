const std = @import("std");
const mmu = @import("mmu.zig");
const cpu = @import("cpu.zig");

pub const EmulatorError = error{
    IllegalMemoryAccess,
    IllegalInstruction,
    Breakpoint,
};

pub const EmulatorParamters = struct {
    breakpoint_instruction: ?u8,
};

pub const Emulator = struct {
    mmu: mmu.Mmu,
    cpu: cpu.Cpu,

    pub fn init(params: EmulatorParamters) Emulator {
        const m = mmu.Mmu.init();
        const c = cpu.Cpu.init(m, params.breakpoint_instruction);
        return Emulator{
            .mmu = m,
            .cpu = c,
        };
    }

    pub fn run(self: *Emulator) EmulatorError {
        while (true) {
            self.cpu.tick();
            if (self.cpu.instructionBoundary() and self.cpu.breakpointHappened()) {
                return EmulatorError.Breakpoint;
            }
            if (self.cpu.illegalInstructionExecuted()) {
                return EmulatorError.IllegalInstruction;
            }
            if (self.mmu.illegalMemoryOperationHappened()) {
                return EmulatorError.IllegalMemoryAccess;
            }
        }
    }
};
