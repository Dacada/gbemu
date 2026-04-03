const std = @import("std");

const RuntimeCfg = struct {
    debugger_writer: ?*std.io.Writer = null,
    breakpoint_instruction: ?u8 = null,
    cartridge: ?union(enum) {
        by_path: struct {
            absolute_path: []const u8,
            filename: []const u8,
        },
        by_file: struct {
            file: std.fs.File,
        },
    } = null,
    audio_wav_filename: ?[]const u8 = null,
    allocator: ?std.mem.Allocator = null,
};

pub const ContainerConfig = struct {
    audio_backend: enum {
        mock_wav,
        mock_nil,
    } = .mock_nil,
    video_backend: enum {
        mock_nil,
    } = .mock_nil,
    scheduler: enum {
        real,
        mock,
    } = .real,
    cartridge: enum {
        real,
        dummy,
    } = .real,
    joypad: enum {
        real,
        dummy,
    } = .real,
    serial: enum {
        real,
        dummy,
    } = .real,
    timer: enum {
        real,
        dummy,
    } = .real,
    interrupt: enum {
        real,
        mock,
        dummy,
    } = .real,
    apu: enum {
        real,
        mock,
        dummy,
    } = .real,
    ppu: enum {
        real,
        dummy,
    } = .real,
    boot_rom: enum {
        dummy,
    } = .dummy,
    mmio: enum {
        real,
        dummy,
    } = .real,
    mmu: enum {
        real,
        mock,
        dummy,
    } = .real,
    cpu: enum {
        real,
        mock,
    } = .real,
    debugger: enum {
        real,
        mock,
    } = .real,
};

pub fn Container(comptime cfg: ContainerConfig) type {
    const backend = @import("backend.zig");
    const cartridge = @import("cartridge.zig");
    const scheduler = @import("scheduler.zig");
    const interrupt = @import("interrupt.zig");
    const mmio = @import("mmio.zig");
    const mmu = @import("mmu.zig");
    const cpu = @import("cpu.zig");
    const apu = @import("apu.zig");
    const timer = @import("timer.zig");
    const ppu = @import("ppu.zig");
    const debugger = @import("debugger.zig");
    const Dummy = mmu.Dummy;

    const c_AudioBackend = switch (cfg.audio_backend) {
        .mock_wav => backend.WavAudioBackend,
        .mock_nil => backend.NullAudioBackend,
    };

    const c_VideoBackend = switch (cfg.video_backend) {
        .mock_nil => backend.NullVideoBackend,
    };

    const c_Scheduler = switch (cfg.scheduler) {
        .mock => scheduler.MockScheduler,
        .real => scheduler.Scheduler,
    };

    const c_Cartridge = switch (cfg.cartridge) {
        .dummy => Dummy,
        .real => cartridge.Cartridge,
    };

    const c_Interrupt = switch (cfg.interrupt) {
        .real => interrupt.Interrupt,
        .mock => interrupt.MockInterrupt,
        .dummy => Dummy,
    };

    const c_Joypad = switch (cfg.joypad) {
        .real => @import("joypad.zig").Joypad(c_Interrupt),
        .dummy => Dummy,
    };

    const c_Serial = switch (cfg.serial) {
        .real => @import("serial.zig").Serial(c_Scheduler, c_Interrupt),
        .dummy => Dummy,
    };

    const c_Apu = switch (cfg.apu) {
        .real => apu.Apu(c_AudioBackend),
        .mock => apu.MockApu(c_AudioBackend),
        .dummy => Dummy,
    };

    const c_Timer = switch (cfg.timer) {
        .real => timer.Timer(c_Apu, c_Interrupt),
        .dummy => Dummy,
    };

    const c_BootRom = switch (cfg.boot_rom) {
        .dummy => Dummy,
    };

    const c_Ppu = switch (cfg.ppu) {
        .real => ppu.Ppu(c_VideoBackend),
        .dummy => Dummy,
    };

    const c_Mmio = switch (cfg.mmio) {
        .real => mmio.Mmio(c_Joypad, c_Serial, c_Timer, c_Interrupt, c_Apu, c_Ppu, c_BootRom),
        .dummy => Dummy,
    };

    const c_Mmu = switch (cfg.mmu) {
        .real => mmu.Mmu(c_Cartridge, c_Ppu, c_Mmio),
        .mock => mmu.MockMmu,
        .dummy => Dummy,
    };

    const c_Cpu = switch (cfg.cpu) {
        .real => cpu.Cpu(c_Mmu, c_Interrupt),
        .mock => cpu.MockCpu(c_Mmu),
    };

    const c_Debugger = switch (cfg.debugger) {
        .real => debugger.Debugger(c_Cpu, c_Mmu),
        .mock => debugger.MockDebugger,
    };

    const c_Emulator = @import("emulator.zig").Emulator(c_Cpu, c_Apu, c_Ppu, c_Timer, c_Scheduler, c_Debugger);

    return struct {
        const This = @This();

        pub const AudioBackend = c_AudioBackend;
        pub const VideoBackend = c_VideoBackend;
        pub const Scheduler = c_Scheduler;
        pub const Cartridge = c_Cartridge;
        pub const Interrupt = c_Interrupt;
        pub const Joypad = c_Joypad;
        pub const Serial = c_Serial;
        pub const Apu = c_Apu;
        pub const Timer = c_Timer;
        pub const BootRom = c_BootRom;
        pub const Ppu = c_Ppu;
        pub const Mmio = c_Mmio;
        pub const Mmu = c_Mmu;
        pub const Cpu = c_Cpu;
        pub const Debugger = c_Debugger;
        pub const Emulator = c_Emulator;

        audio_backend: ?AudioBackend = null,
        video_backend: ?VideoBackend = null,
        scheduler: ?Scheduler = null,
        cartridge: ?Cartridge = null,
        interrupt: ?Interrupt = null,
        joypad: ?Joypad = null,
        serial: ?Serial = null,
        apu: ?Apu = null,
        timer: ?Timer = null,
        boot_rom: ?BootRom = null,
        ppu: ?Ppu = null,
        mmio: ?Mmio = null,
        mmu: ?Mmu = null,
        cpu: ?Cpu = null,
        debugger: ?Debugger = null,
        emulator: ?Emulator = null,

        runtime_cfg: RuntimeCfg,

        /// The lifetimes of all created objects depend on the lifetime of this one.
        pub fn init(runtime_cfg: RuntimeCfg) This {
            return This{
                .runtime_cfg = runtime_cfg,
            };
        }

        fn get_or_make(T: type, comptime what: []const u8, inst: *This) !*T {
            if (@field(inst, what) == null) {
                @field(inst, what) = try @field(This, std.fmt.comptimePrint("make_{s}", .{what}))(inst);
            }
            return &@field(inst, what).?;
        }

        fn make_audio_backend(self: *This) !AudioBackend {
            return switch (cfg.audio_backend) {
                .mock_nil => AudioBackend.init(),
                .mock_wav => AudioBackend.init(
                    self.runtime_cfg.audio_wav_filename.?,
                    self.runtime_cfg.allocator.?,
                ),
            };
        }

        fn make_video_backend(_: *This) !VideoBackend {
            return switch (cfg.video_backend) {
                .mock_nil => VideoBackend.init(),
            };
        }

        fn make_scheduler(_: *This) !Scheduler {
            return switch (cfg.scheduler) {
                .real => Scheduler.init(),
                .mock => Scheduler{},
            };
        }

        fn make_cartridge(self: *This) !Cartridge {
            return switch (cfg.cartridge) {
                .real => switch (self.runtime_cfg.cartridge.?) {
                    .by_path => |p| blk: {
                        var dir = try std.fs.openDirAbsolute(p.absolute_path, .{});
                        defer dir.close();
                        const file = try dir.openFile(p.filename, .{});
                        defer file.close();
                        break :blk Cartridge.fromFile(file);
                    },
                    .by_file => |f| Cartridge.fromFile(f.file),
                },
                .dummy => Cartridge{},
            };
        }

        fn make_interrupt(_: *This) !Interrupt {
            return switch (cfg.interrupt) {
                .real => Interrupt.init(),
                .mock => Interrupt.init(),
                .dummy => Dummy{},
            };
        }

        fn make_joypad(self: *This) !Joypad {
            return switch (cfg.joypad) {
                .real => Joypad.init(try self.get_interrupt()),
                .dummy => Dummy{},
            };
        }

        fn make_serial(self: *This) !Serial {
            return switch (cfg.serial) {
                .real => Serial.init(try self.get_scheduler(), try self.get_interrupt()),
                .dummy => Dummy{},
            };
        }

        fn make_apu(self: *This) !Apu {
            return switch (cfg.apu) {
                .real => Apu.init(try self.get_audio_backend()),
                .mock => Apu.init(try self.get_audio_backend()),
                .dummy => Dummy{},
            };
        }

        fn make_timer(self: *This) !Timer {
            return switch (cfg.timer) {
                .real => Timer.init(try self.get_apu(), try self.get_interrupt()),
                .dummy => Dummy{},
            };
        }

        fn make_boot_rom(_: *This) !BootRom {
            return switch (cfg.boot_rom) {
                .dummy => Dummy{},
            };
        }

        fn make_ppu(self: *This) !Ppu {
            return switch (cfg.ppu) {
                .real => Ppu.init(try self.get_video_backend()),
                .dummy => Dummy{},
            };
        }

        fn make_mmio(self: *This) !Mmio {
            return switch (cfg.mmio) {
                .real => Mmio.init(
                    try self.get_joypad(),
                    try self.get_serial(),
                    try self.get_timer(),
                    try self.get_interrupt(),
                    try self.get_apu(),
                    try self.get_ppu(),
                    try self.get_boot_rom(),
                ),
                .dummy => Dummy{},
            };
        }

        fn make_mmu(self: *This) !Mmu {
            return switch (cfg.mmu) {
                .real => Mmu.init(
                    try self.get_cartridge(),
                    try self.get_ppu(),
                    try self.get_mmio(),
                ),
                .mock => Mmu{},
                .dummy => Dummy{},
            };
        }

        fn make_cpu(self: *This) !Cpu {
            return switch (cfg.cpu) {
                .real => Cpu.init(try self.get_mmu(), try self.get_interrupt(), self.runtime_cfg.breakpoint_instruction),
                .mock => Cpu.init(try self.get_mmu()),
            };
        }

        fn make_debugger(self: *This) !Debugger {
            return switch (cfg.debugger) {
                .real => Debugger.init(try self.get_cpu(), try self.get_mmu(), self.runtime_cfg.debugger_writer.?),
                .mock => Debugger{},
            };
        }

        fn make_emulator(self: *This) !Emulator {
            return Emulator.init(try self.get_cpu(), try self.get_apu(), try self.get_ppu(), try self.get_timer(), try self.get_scheduler(), try self.get_debugger());
        }

        pub fn get_audio_backend(self: *This) !*AudioBackend {
            return get_or_make(AudioBackend, "audio_backend", self);
        }

        pub fn get_video_backend(self: *This) !*VideoBackend {
            return get_or_make(VideoBackend, "video_backend", self);
        }

        pub fn get_scheduler(self: *This) !*Scheduler {
            return get_or_make(Scheduler, "scheduler", self);
        }

        pub fn get_cartridge(self: *This) !*Cartridge {
            return get_or_make(Cartridge, "cartridge", self);
        }

        pub fn get_interrupt(self: *This) !*Interrupt {
            return get_or_make(Interrupt, "interrupt", self);
        }

        pub fn get_joypad(self: *This) !*Joypad {
            return get_or_make(Joypad, "joypad", self);
        }

        pub fn get_serial(self: *This) !*Serial {
            return get_or_make(Serial, "serial", self);
        }

        pub fn get_apu(self: *This) !*Apu {
            return get_or_make(Apu, "apu", self);
        }

        pub fn get_timer(self: *This) !*Timer {
            return get_or_make(Timer, "timer", self);
        }

        pub fn get_boot_rom(self: *This) !*BootRom {
            return get_or_make(BootRom, "boot_rom", self);
        }

        pub fn get_ppu(self: *This) !*Ppu {
            return get_or_make(Ppu, "ppu", self);
        }

        pub fn get_mmio(self: *This) !*Mmio {
            return get_or_make(Mmio, "mmio", self);
        }

        pub fn get_mmu(self: *This) !*Mmu {
            return get_or_make(Mmu, "mmu", self);
        }

        pub fn get_cpu(self: *This) !*Cpu {
            return get_or_make(Cpu, "cpu", self);
        }

        pub fn get_debugger(self: *This) !*Debugger {
            return get_or_make(Debugger, "debugger", self);
        }

        pub fn get_emulator(self: *This) !*Emulator {
            return get_or_make(Emulator, "emulator", self);
        }
    };
}
