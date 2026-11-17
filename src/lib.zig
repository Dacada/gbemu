pub const backends = struct {
    pub const audio = struct {
        pub const Null = @import("backends/audio/Null.zig");
        pub const Wav = @import("backends/audio/Wav.zig");
    };

    pub const joypad = struct {
        pub const Null = @import("backends/joypad/Null.zig");
    };

    pub const video = struct {
        pub const Null = @import("backends/video/Null.zig");
    };
};

const dependency_container = @import("dependency_container.zig");
pub const Container = dependency_container.Container;
pub const ContainerConfig = dependency_container.ContainerConfig;

pub const tools = struct {
    pub const assembler = @import("tools/assembler.zig");
    pub const tracker = @import("tools/tracker.zig");
};

pub const core = struct {
    pub const Alu = @import("core/cpu/Alu.zig");
    pub const Apu = @import("core/apu.zig").Apu;
    pub const ApuChannel = @import("core/apu/channel.zig").Channel;
    pub const Cartridge = @import("core/cartridge.zig").Cartridge;
    pub const Cpu = @import("core/cpu.zig").Cpu;
    pub const Debugger = @import("core/debugger.zig").Debugger;
    pub const Emulator = @import("core/emulator.zig").Emulator;
    pub const Interrupt = @import("core/interrupt.zig").Interrupt;
    pub const InterruptKind = @import("core/interrupt.zig").InterruptKind;
    pub const Joypad = @import("core/joypad.zig").Joypad;
    pub const Mmu = @import("core/mmu.zig").Mmu;
    pub const Ppu = @import("core/ppu.zig").Ppu;
    pub const Serial = @import("core/serial.zig").Serial;
    pub const Timer = @import("core/timer.zig").Timer;
};

test {
    @import("std").testing.refAllDecls(@This());
}
