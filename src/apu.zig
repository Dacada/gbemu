const std = @import("std");

const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;
const GenericRouter = @import("router.zig").Router;
const GenericRange = @import("router.zig").Range;
const GenericTargetField = @import("router.zig").TargetField;

// TODO: The mysterious fifth channel? (absolutely not worth it)

// DMG ONLY -- CGB models include registers that allow inspecting the emitted sample

pub fn Apu(AudioBackend: type) type {
    return ApuGeneric(MockChannel, MockChannel, MockChannel, MockChannel, AudioBackend);
}

fn ApuGeneric(Channel1: type, Channel2: type, Channel3: type, Channel4: type, AudioBackend: type) type {
    return struct {
        const This = @This();

        channel1: Channel1,
        channel2: Channel2,
        channel3: Channel3,
        channel4: Channel4,

        dac1: AudioBackend.Resampler,
        dac2: AudioBackend.Resampler,
        dac3: AudioBackend.Resampler,
        dac4: AudioBackend.Resampler,

        backend: *AudioBackend,

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
            pub fn read(_: *This, _: u16) struct { MemoryFlag, u8 } {
                return .{ .{}, 0xFF };
            }

            pub fn write(_: *This, _: u16, _: u8) MemoryFlag {
                return .{};
            }

            pub fn peek(_: *This, _: u16) u8 {
                return 0xFF;
            }
            pub fn poke(_: *This, _: u16, _: u8) void {}
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
                .backend = backend,
                .channel1 = Channel1.init(),
                .channel2 = Channel2.init(),
                .channel3 = Channel3.init(),
                .channel4 = Channel4.init(),
                .dac1 = AudioBackend.Resampler.init(),
                .dac2 = AudioBackend.Resampler.init(),
                .dac3 = AudioBackend.Resampler.init(),
                .dac4 = AudioBackend.Resampler.init(),
            };
        }

        pub fn tick(self: *This) void {
            inline for (1..5) |n| {
                const channelName = std.fmt.comptimePrint("channel{d}", .{n});
                const dacName = std.fmt.comptimePrint("dac{d}", .{n});
                const sample = @field(self, channelName).tick();
                if (sample) |s| {
                    const resampled = @field(self, dacName).resample(s);
                    if (resampled) |r| {
                        self.backend.submit(r);
                    }
                }
            }
        }

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

const MockChannel = struct {
    ticks: usize = 0,
    divticks: usize = 0,
    last_read_addr: ?u16 = null,
    last_write_addr: ?u16 = null,
    last_write_val: ?u8 = null,

    const Wave = struct {
        pub fn read(self: *MockChannel, addr: u16) struct { MemoryFlag, u8 } {
            self.last_read_addr = addr;
            return .{ .{}, 0x42 };
        }

        pub fn write(self: *MockChannel, addr: u16, val: u8) MemoryFlag {
            self.last_write_addr = addr;
            self.last_write_val = val;
            return .{};
        }

        pub fn peek(_: *MockChannel, _: u16) u8 {
            return 0x42;
        }

        pub fn poke(_: *MockChannel, _: u16, _: u8) void {}
    };

    pub fn init() MockChannel {
        return .{};
    }

    pub fn tick(self: *MockChannel) ?u4 {
        self.ticks += 1;
        return @as(u4, 0b1010); // arbitrary non-null sample
    }

    pub fn divtick(self: *MockChannel) void {
        self.divticks += 1;
    }

    pub fn read(self: *MockChannel, addr: u16) struct { MemoryFlag, u8 } {
        self.last_read_addr = addr;
        return .{ .{}, 0x42 };
    }

    pub fn write(self: *MockChannel, addr: u16, val: u8) MemoryFlag {
        self.last_write_addr = addr;
        self.last_write_val = val;
        return .{};
    }

    pub fn peek(_: *MockChannel, _: u16) u8 {
        return 0x42;
    }

    pub fn poke(_: *MockChannel, _: u16, _: u8) void {}
};

const MockBackend = struct {
    const Resampler = struct {
        resample_count: usize = 0,

        pub fn init() Resampler {
            return .{};
        }

        pub fn resample(self: *Resampler, sample: u4) ?u8 {
            self.resample_count += 1;
            return sample;
        }
    };

    submissions: usize = 0,

    pub fn submit(self: *MockBackend, _: u8) void {
        self.submissions += 1;
    }
};

const MockedApu = ApuGeneric(MockChannel, MockChannel, MockChannel, MockChannel, MockBackend);

test "tick increases counters and submits samples" {
    var backend = MockBackend{};
    var apu = MockedApu.init(&backend);

    apu.tick();

    try std.testing.expectEqual(4, backend.submissions);
    try std.testing.expectEqual(1, apu.channel1.ticks);
    try std.testing.expectEqual(1, apu.channel2.ticks);
    try std.testing.expectEqual(1, apu.channel3.ticks);
    try std.testing.expectEqual(1, apu.channel4.ticks);
    try std.testing.expectEqual(1, apu.dac1.resample_count);
    try std.testing.expectEqual(1, apu.dac2.resample_count);
    try std.testing.expectEqual(1, apu.dac3.resample_count);
    try std.testing.expectEqual(1, apu.dac4.resample_count);
}

test "divtick increases divtick counters" {
    var backend = MockBackend{};
    var apu = MockedApu.init(&backend);

    apu.divtick();

    try std.testing.expectEqual(1, apu.channel1.divticks);
    try std.testing.expectEqual(1, apu.channel2.divticks);
    try std.testing.expectEqual(1, apu.channel3.divticks);
    try std.testing.expectEqual(1, apu.channel4.divticks);
}

test "read and write are routed to correct channel" {
    var backend = MockBackend{};
    var apu = MockedApu.init(&backend);

    const addr1: u16 = 0x01; // Channel 1 range
    const addr2: u16 = 0x07; // Channel 2 range
    const val: u8 = 0x99;

    _ = apu.write(addr1, val);
    _ = apu.read(addr2);

    try std.testing.expectEqual(null, apu.channel1.last_read_addr);
    try std.testing.expectEqual(addr1, apu.channel1.last_write_addr);
    try std.testing.expectEqual(val, apu.channel1.last_write_val);
    try std.testing.expectEqual(addr2 - 0x05, apu.channel2.last_read_addr);
    try std.testing.expectEqual(null, apu.channel2.last_write_addr);
    try std.testing.expectEqual(null, apu.channel2.last_write_val);
}
