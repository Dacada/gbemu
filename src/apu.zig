const std = @import("std");

const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;
const router = @import("router.zig");
const GenericRouter = router.Router;
const GenericRange = router.Range;
const GenericTargetField = router.TargetField;

// TODO: The mysterious fifth channel from the cartridge itself? (absolutely not worth it)

// DMG ONLY -- CGB models include registers that allow inspecting the emitted sample

const MockChannel = struct {
    const Wave = struct {
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
    return ApuGeneric(MockChannel, MockChannel, MockChannel, MockChannel, AudioBackend);
}

fn ApuGeneric(Channel1: type, Channel2: type, Channel3: type, Channel4: type, AudioBackend: type) type {
    return struct {
        const This = @This();

        const ApuTickRate = 1_048_576;

        const Accumulator = packed struct {
            sum: f32 = 0,
            count: u64 = 0,
            prev: f32 = 0,
        };

        channel1: Channel1,
        channel2: Channel2,
        channel3: Channel3,
        channel4: Channel4,

        backend: *AudioBackend,

        vin_left: u1,
        vin_right: u1,
        left_vol: u3,
        right_vol: u3,

        ch1_left: u1,
        ch2_left: u1,
        ch3_left: u1,
        ch4_left: u1,
        ch1_right: u1,
        ch2_right: u1,
        ch3_right: u1,
        ch4_right: u1,

        audio_on: u1,

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

        samplingError: u64,

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
            pub fn peek(self: *This, addr: u16) u8 {
                switch (addr) {
                    0 => {
                        var res: u8 = 0;
                        res |= self.vin_left;
                        res <<= 3;
                        res |= self.left_vol;
                        res <<= 1;
                        res |= self.vin_right;
                        res <<= 3;
                        res |= self.right_vol;
                        return res;
                    },

                    1 => {
                        var res: u8 = 0;
                        res |= self.ch4_left;
                        res <<= 1;
                        res |= self.ch3_left;
                        res <<= 1;
                        res |= self.ch2_left;
                        res <<= 1;
                        res |= self.ch1_left;
                        res <<= 1;
                        res |= self.ch4_right;
                        res <<= 1;
                        res |= self.ch3_right;
                        res <<= 1;
                        res |= self.ch2_right;
                        res <<= 1;
                        res |= self.ch1_right;
                        return res;
                    },

                    2 => {
                        var res: u8 = 0;
                        res |= self.audio_on;
                        res <<= 4;
                        res |= if (self.channel1.isActive()) 1 else 0;
                        res <<= 1;
                        res |= if (self.channel2.isActive()) 1 else 0;
                        res <<= 1;
                        res |= if (self.channel3.isActive()) 1 else 0;
                        res <<= 1;
                        res |= if (self.channel4.isActive()) 1 else 0;
                        return res;
                    },

                    else => unreachable,
                }
            }

            pub fn poke(self: *This, addr: u16, val: u8) void {
                switch (addr) {
                    0 => {
                        self.vin_left = @intCast((val & 0b1000_0000) >> 7);
                        self.vin_right = @intCast((val & 0b0000_1000) >> 3);
                        self.left_vol = @intCast((val & 0b0111_0000) >> 4);
                        self.right_vol = @intCast(val & 0b0000_0111);
                    },
                    1 => {
                        self.ch4_left = @intCast((val & 0b1000_0000) >> 7);
                        self.ch3_left = @intCast((val & 0b0100_0000) >> 6);
                        self.ch2_left = @intCast((val & 0b0010_0000) >> 5);
                        self.ch1_left = @intCast((val & 0b0001_0000) >> 4);
                        self.ch4_right = @intCast((val & 0b0000_1000) >> 3);
                        self.ch3_right = @intCast((val & 0b0000_0100) >> 2);
                        self.ch2_right = @intCast((val & 0b0000_0010) >> 1);
                        self.ch1_right = @intCast(val & 0b0000_0001);
                    },
                    2 => {
                        self.audio_on = @intCast((val & 0b1000_0000) >> 7);
                    },
                    else => unreachable,
                }
            }

            pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
                return .{ .{}, self.peek(addr) };
            }

            pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
                if (self.audio_on == 0 and addr != 2) {
                    // If audio is off, registers are read-only (except the one to turn it back on)
                    return .{};
                }

                const prev_audio_on = self.audio_on;
                self.poke(addr, val);
                if (addr == 2 and (prev_audio_on == 1 and self.audio_on == 0)) {
                    // Turning the apu off zeroes all registers (except this one)
                    self.poke(0, 0);
                    self.poke(1, 0);
                    self.channel1.poweroff();
                    self.channel2.poweroff();
                    self.channel3.poweroff();
                    self.channel4.poweroff();
                }

                return .{};
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
                    .end = 0x0D,
                    .target = .channel3,
                },
                .{
                    .start = 0x10,
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
                    .namespace = Channel3.Wave,
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
                .vin_left = undefined,
                .vin_right = undefined,
                .left_vol = undefined,
                .right_vol = undefined,
                .ch1_left = undefined,
                .ch2_left = undefined,
                .ch3_left = undefined,
                .ch4_left = undefined,
                .ch1_right = undefined,
                .ch2_right = undefined,
                .ch3_right = undefined,
                .ch4_right = undefined,
                .audio_on = 1,
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
                .samplingError = 0,
            };
        }

        /// This is intended to be called every APU tick
        pub fn tick(self: *This) void {
            if (self.audio_on == 0) {
                return;
            }

            inline for (1..5) |n| {
                const channelName = std.fmt.comptimePrint("channel{d}", .{n});

                // Each channel knows its DAC output value as a normalized floating point sample between 1 and -1. A
                // disabled DAC is expected to output 0.
                const sample: f32 = @field(self, channelName).tick();

                inline for (.{ "left", "right" }) |side| {
                    const channelSelectName = std.fmt.comptimePrint("ch{d}_{s}", .{ n, side });
                    const accumulatorChannelName = std.fmt.comptimePrint("ch{d}_acc_{s}", .{ n, side });
                    const volume_name = std.fmt.comptimePrint("{s}_vol", .{side});

                    // Select audio channels, attenuate, and accumulate for sampling.
                    if (@field(self, channelSelectName) == 1) {
                        // Attenuate the selected channel if needed
                        var attenuation: f32 = @floatFromInt(@field(self, volume_name));
                        attenuation += 1.0;
                        attenuation /= 8.0;

                        const value = sample * attenuation;
                        @field(self, accumulatorChannelName).sum += value;
                    }

                    // If a channel is deselected the result is that the DAC's contribution
                    // disappears, which should cause an audio pop due to the DC component changing level, which we
                    // want. Therefore we always add that sample even if the channel is not selected
                    @field(self, accumulatorChannelName).count += 1;
                }
            }

            // Resampling logic, take the accumulated samples and contruct a new sample from them, or keep the previous
            // sample
            self.samplingError += AudioBackend.SamplingRate;
            if (self.samplingError >= This.ApuTickRate) {
                self.samplingError -= AudioBackend.SamplingRate;

                var results: struct { left: f32 = 0, right: f32 = 0 } = .{};
                inline for (.{ "left", "right" }) |side| {
                    const hpfName = std.fmt.comptimePrint("hpf_{s}", .{side});
                    var mixed_total: f32 = 0;
                    inline for (1..5) |n| {
                        const accumulatorChannelName = std.fmt.comptimePrint("ch{d}_acc_{s}", .{ n, side });
                        var acc = &@field(self, accumulatorChannelName);
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
                    @field(results, side) = apply_hpf(&@field(self, hpfName), mixed_total);
                }

                self.backend.submit(results.left, results.right);
            }
        }

        fn apply_hpf(capacitor: *f32, value: f32) f32 {
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
                const channelName = std.fmt.comptimePrint("channel{d}", .{n});
                @field(self, channelName).divtick();
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
    };
}
