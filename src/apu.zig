const std = @import("std");

const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const router = @import("router.zig");
const GenericRouter = router.Router;
const GenericRange = router.Range;
const GenericTargetField = router.TargetField;
const TrackedValue = @import("tracked_value.zig").TrackedValue;
const Channel = @import("channel.zig").Channel;

const mmio_register = @import("mmio_register.zig");
const NR50 = mmio_register.NR50;
const NR51 = mmio_register.NR51;
const NR52 = mmio_register.NR52;

// TODO: The mysterious fifth channel from the cartridge itself? (absolutely not worth it)

// DMG ONLY -- CGB models include registers that allow inspecting the emitted sample

const MockChannel = struct {
    const wave_memory_interface = struct {
        pub fn read(_: *MockChannel, _: u16) struct { MemoryFlag, u8 } {
            return .{ .{}, 0xFF };
        }
        pub fn write(_: *MockChannel, _: u16, _: u8) MemoryFlag {
            return .{};
        }
        pub fn peek(_: *MockChannel, _: u16) u8 {
            return 0xFF;
        }
        pub fn poke(_: *MockChannel, _: u16, _: u8) void {}
    };
    fn init() MockChannel {
        return MockChannel{};
    }
    fn isActive(_: *const MockChannel) bool {
        return false;
    }
    fn poweroff(_: *MockChannel) void {}
    fn tick(_: *MockChannel) f32 {
        return 0;
    }
    fn divtick(_: *MockChannel) void {}

    pub fn read(_: *MockChannel, _: u16) struct { MemoryFlag, u8 } {
        return .{ .{}, 0xFF };
    }
    pub fn write(_: *MockChannel, _: u16, _: u8) MemoryFlag {
        return .{};
    }
    pub fn peek(_: *MockChannel, _: u16) u8 {
        return 0xFF;
    }
    pub fn poke(_: *MockChannel, _: u16, _: u8) void {}
};

pub fn Apu(AudioBackend: type) type {
    return ApuGeneric(Channel(1), Channel(2), Channel(3), Channel(4), AudioBackend);
}

pub fn MockApu(AudioBackend: type) type {
    return ApuGeneric(MockChannel, MockChannel, MockChannel, MockChannel, AudioBackend);
}

fn ApuGeneric(Channel1: type, Channel2: type, Channel3: type, Channel4: type, AudioBackend: type) type {
    return struct {
        const This = @This();

        const ApuTickRate = 2_097_152;

        const Accumulator = struct {
            sum: f32 = 0,
            count: u64 = 0,
            prev: f32 = 0,
        };

        channel1: Channel1,
        channel2: Channel2,
        channel3: Channel3,
        channel4: Channel4,

        backend: *AudioBackend,

        vin_left: TrackedValue(bool),
        vin_right: TrackedValue(bool),
        left_vol: TrackedValue(u3),
        right_vol: TrackedValue(u3),

        ch1_left: TrackedValue(bool),
        ch2_left: TrackedValue(bool),
        ch3_left: TrackedValue(bool),
        ch4_left: TrackedValue(bool),
        ch1_right: TrackedValue(bool),
        ch2_right: TrackedValue(bool),
        ch3_right: TrackedValue(bool),
        ch4_right: TrackedValue(bool),

        audio_on: TrackedValue(bool),

        hpf_left: f32,
        hpf_right: f32,

        ch1_acc_left: Accumulator,
        ch2_acc_left: Accumulator,
        ch3_acc_left: Accumulator,
        ch4_acc_left: Accumulator,
        ch1_acc_right: Accumulator,
        ch2_acc_right: Accumulator,
        ch3_acc_right: Accumulator,
        ch4_acc_right: Accumulator,

        sampling_error: u64,

        const Invalid = struct {
            pub fn read(_: *This, _: u16) struct { MemoryFlag, u8 } {
                return .{ .{ .illegal = true }, 0xFF };
            }

            pub fn write(_: *This, _: u16, _: u8) MemoryFlag {
                return .{ .illegal = true };
            }

            pub fn peek(_: *This, _: u16) u8 {
                return 0xFF;
            }
            pub fn poke(_: *This, _: u16, _: u8) void {}
        };

        const Control = struct {
            fn inner_read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
                var flag: MemoryFlag = .{};
                const val: u8 = switch (addr) {
                    0 => @bitCast(NR50{
                        .vin_left = self.vin_left.resolve(&flag),
                        .vin_right = self.vin_right.resolve(&flag),
                        .left_volume = self.left_vol.resolve(&flag),
                        .right_volume = self.right_vol.resolve(&flag),
                    }),
                    1 => @bitCast(NR51{
                        .ch1_left = self.ch1_left.resolve(&flag),
                        .ch2_left = self.ch2_left.resolve(&flag),
                        .ch3_left = self.ch3_left.resolve(&flag),
                        .ch4_left = self.ch4_left.resolve(&flag),
                        .ch1_right = self.ch1_right.resolve(&flag),
                        .ch2_right = self.ch2_right.resolve(&flag),
                        .ch3_right = self.ch3_right.resolve(&flag),
                        .ch4_right = self.ch4_right.resolve(&flag),
                    }),
                    2 => @bitCast(NR52{
                        .audio_on = self.audio_on.resolve(&flag),
                        .ch1_on = self.channel1.isActive(),
                        .ch2_on = self.channel2.isActive(),
                        .ch3_on = self.channel3.isActive(),
                        .ch4_on = self.channel4.isActive(),
                    }),
                    else => unreachable,
                };

                return .{ flag, val };
            }

            fn inner_write(self: *This, addr: u16, val: u8) void {
                switch (addr) {
                    0 => {
                        const reg: NR50 = @bitCast(val);
                        self.vin_left.set(reg.vin_left);
                        self.vin_right.set(reg.vin_right);
                        self.left_vol.set(reg.left_volume);
                        self.right_vol.set(reg.right_volume);
                    },
                    1 => {
                        const reg: NR51 = @bitCast(val);
                        self.ch4_left.set(reg.ch4_left);
                        self.ch3_left.set(reg.ch3_left);
                        self.ch2_left.set(reg.ch2_left);
                        self.ch1_left.set(reg.ch1_left);
                        self.ch4_right.set(reg.ch4_right);
                        self.ch3_right.set(reg.ch3_right);
                        self.ch2_right.set(reg.ch2_right);
                        self.ch1_right.set(reg.ch1_right);
                    },
                    2 => {
                        const reg: NR52 = @bitCast(val);
                        self.audio_on.set(reg.audio_on);
                    },
                    else => unreachable,
                }
            }

            pub fn peek(self: *This, addr: u16) u8 {
                return Control.inner_read(self, addr).@"1";
            }

            pub fn poke(self: *This, addr: u16, val: u8) void {
                Control.inner_write(self, addr, val);
            }

            pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
                return Control.inner_read(self, addr);
            }

            pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
                // The register to turn audio on ALWAYS works, the rest depend on whether audio is on
                // But we can't check if audio is on or off yet because it starts UNDEFINED
                // So if we're on addr 2 we set it immediately
                // THEN we can check if audio is off and bail out
                // And otherwise we do the write
                var flags: MemoryFlag = .{};

                if (addr == 2) {
                    Control.inner_write(self, addr, val);
                } else if (!self.audio_on.resolve(&flags)) {
                    return flags;
                } else {
                    Control.inner_write(self, addr, val);
                }

                // If we are setting the audio on bit to 0, zero all registers
                // If it was already 0, then we zero them again, which should be fine

                if (addr == 2 and val & 0b1000_0000 == 0) {
                    Control.inner_write(self, 0, 0);
                    Control.inner_write(self, 1, 0);
                    self.channel1.poweroff();
                    self.channel2.poweroff();
                    self.channel3.poweroff();
                    self.channel4.poweroff();
                }

                return flags;
            }
        };

        const Target = enum {
            channel1,
            channel2,
            channel3,
            channel4,
            control,
            wave,
            invalid,
        };

        const Range = GenericRange(Target);
        const TargetField = GenericTargetField(Target);
        const Router = GenericRouter(
            Target,
            &[_]Range{
                .{
                    .start = 0x00,
                    .end = 0x04,
                    .target = .channel1,
                },
                .{
                    .start = 0x05,
                    .end = 0x09,
                    .target = .channel2,
                },
                .{
                    .start = 0x0A,
                    .end = 0x0E,
                    .target = .channel3,
                },
                .{
                    .start = 0xF,
                    .end = 0x13,
                    .target = .channel4,
                },
                .{
                    .start = 0x14,
                    .end = 0x16,
                    .target = .control,
                },
                .{
                    .start = 0x20,
                    .end = 0x2f,
                    .target = .wave,
                },
            },
            .{
                .addr = undefined,
                .target = .invalid,
            },
            &[_]TargetField{
                .{
                    .target = .channel1,
                    .field = "channel1",
                    .namespace = Channel1,
                },
                .{
                    .target = .channel2,
                    .field = "channel2",
                    .namespace = Channel2,
                },
                .{
                    .target = .channel3,
                    .field = "channel3",
                    .namespace = Channel3,
                },
                .{
                    .target = .channel4,
                    .field = "channel4",
                    .namespace = Channel4,
                },
                .{
                    .target = .control,
                    .field = null,
                    .namespace = Control,
                },
                .{
                    .target = .wave,
                    .field = "channel3",
                    .namespace = Channel3.wave_memory_interface,
                },
                .{
                    .target = .invalid,
                    .field = null,
                    .namespace = Invalid,
                },
            },
        );

        pub inline fn init(backend: *AudioBackend) This {
            return This{
                .channel1 = Channel1.init(),
                .channel2 = Channel2.init(),
                .channel3 = Channel3.init(),
                .channel4 = Channel4.init(),
                .backend = backend,
                .vin_left = .init(true),
                .vin_right = .init(false),
                .left_vol = .init(0b101),
                .right_vol = .init(0b101),
                .ch1_left = .init(true),
                .ch2_left = .init(false),
                .ch3_left = .init(true),
                .ch4_left = .init(false),
                .ch1_right = .init(true),
                .ch2_right = .init(false),
                .ch3_right = .init(true),
                .ch4_right = .init(false),
                .audio_on = .init(false),
                .hpf_left = 0.0,
                .hpf_right = 0.0,
                .ch1_acc_left = .{},
                .ch2_acc_left = .{},
                .ch3_acc_left = .{},
                .ch4_acc_left = .{},
                .ch1_acc_right = .{},
                .ch2_acc_right = .{},
                .ch3_acc_right = .{},
                .ch4_acc_right = .{},
                .sampling_error = 0,
            };
        }

        /// This is intended to be called every APU tick
        pub fn tick(self: *This) void {
            if (!self.audio_on.resolve(null)) {
                return;
            }

            inline for (1..5) |n| {
                const channel_name = std.fmt.comptimePrint("channel{d}", .{n});

                // Each channel knows its DAC output value as a normalized floating point sample between 1 and -1. A
                // disabled DAC is expected to output 0.
                const sample: f32 = @field(self, channel_name).tick();

                inline for (.{ "left", "right" }) |side| {
                    const channel_select_name = std.fmt.comptimePrint("ch{d}_{s}", .{ n, side });
                    const accumulator_channel_name = std.fmt.comptimePrint("ch{d}_acc_{s}", .{ n, side });
                    const volume_name = std.fmt.comptimePrint("{s}_vol", .{side});

                    // Select audio channels, attenuate, and accumulate for sampling.
                    if (@field(self, channel_select_name).resolve(null)) {
                        // Attenuate the selected channel if needed
                        var attenuation: f32 = @floatFromInt(@field(self, volume_name).resolve(null));
                        attenuation += 1.0;
                        attenuation /= 8.0;

                        const value = sample * attenuation;
                        @field(self, accumulator_channel_name).sum += value;
                    }

                    // If a channel is deselected the result is that the DAC's contribution
                    // disappears, which should cause an audio pop due to the DC component changing level, which we
                    // want. Therefore we always add that sample even if the channel is not selected
                    @field(self, accumulator_channel_name).count += 1;
                }
            }

            // Resampling logic, take the accumulated samples and contruct a new sample from them, or keep the previous
            // sample
            self.sampling_error += AudioBackend.SamplingRate;
            if (self.sampling_error >= This.ApuTickRate) {
                self.sampling_error -= This.ApuTickRate;

                var results: struct { left: f32 = 0, right: f32 = 0 } = .{};
                inline for (.{ "left", "right" }) |side| {
                    const hpf_name = std.fmt.comptimePrint("hpf_{s}", .{side});
                    var mixed_total: f32 = 0;
                    inline for (1..5) |n| {
                        const accumulator_channel_name = std.fmt.comptimePrint("ch{d}_acc_{s}", .{ n, side });
                        var acc = &@field(self, accumulator_channel_name);
                        if (acc.count != 0) {
                            // We keep track of the last sample and keep it if there have been no new samples in the interval.
                            acc.prev = acc.sum / @as(f32, @floatFromInt(acc.count));
                        }
                        mixed_total += acc.prev;
                        acc.count = 0;
                        acc.sum = 0;
                    }

                    // Renormalize first and then apply the high pass filter.
                    mixed_total /= 4;
                    @field(results, side) = applyHpf(&@field(self, hpf_name), mixed_total);
                }

                self.backend.submit(results.left, results.right);
            }
        }

        fn applyHpf(capacitor: *f32, value: f32) f32 {
            // This magic constant comes from the Pan Docs documentation
            const capacitor_factor = comptime std.math.pow(f32, 0.999958, 4194304.0 / AudioBackend.SamplingRate);

            var out: f32 = 0;
            out = value - capacitor.*;
            capacitor.* = value - out * capacitor_factor;
            return out;
        }

        /// This is intended to be called every DIV-APU tick
        pub fn divtick(self: *This) void {
            inline for (1..5) |n| {
                const channel_name = std.fmt.comptimePrint("channel{d}", .{n});
                @field(self, channel_name).divtick();
            }
        }

        pub inline fn peek(self: *This, addr: u16) u8 {
            _, const ret = Router.dispatch(self, addr, .peek, undefined);
            return ret;
        }

        pub inline fn poke(self: *This, addr: u16, val: u8) void {
            _ = Router.dispatch(self, addr, .poke, val);
        }

        pub inline fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            return Router.dispatch(self, addr, .read, undefined);
        }

        pub inline fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            const ret, _ = Router.dispatch(self, addr, .write, val);
            return ret;
        }

        pub fn shouldPanic(self: *This) void {
            self.channel1.shouldPanic();
            self.channel2.shouldPanic();
            self.channel3.shouldPanic();
            self.channel4.shouldPanic();
        }
    };
}
