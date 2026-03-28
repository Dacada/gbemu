const std = @import("std");

const SongPlayError = error{
    NullChannel,
    NonNullChannel,
    InvalidChannel,
    InvalidValue,
    ImpossibleMerge,
    InvalidMergeParameters,
    InvalidSetValue,
};

pub const SongMetadata = struct {
    bpm: usize,
    tpb: usize,

    pub fn beat(comptime self: SongMetadata, comptime num: usize, comptime den: usize) usize {
        if (self.tpb % den != 0) {
            @panic("beat denominator must be able to subdivide song's tpb");
        }
        return self.tpb / den * num;
    }
};

pub const SongWaveform = struct {
    waveform: [32]u4,
    id: usize,
};

pub const Direction = enum {
    increase,
    decrease,
};

pub const Channel = enum(u2) {
    ch1 = 0,
    ch2 = 1,
    ch3 = 2,
    ch4 = 3,
};

const WaveDuty = enum {
    one_in_eight,
    one_in_four,
    one_in_two,
    three_in_four,
};

const Side = enum {
    left,
    right,
};

const OutputLevel = enum {
    silent,
    full,
    half,
    quart,
};

const LfsrWidth = enum {
    wide,
    narrow,
};

const SongEventInternal = union(enum) {
    set_apu: struct {
        state: bool,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel != null) {
                return SongPlayError.NonNullChannel;
            }

            var res = HardwareEvent.init(0x16);
            if (self.state) {
                res.setBit(7);
            } else {
                res.clearBit(7);
            }

            return singleElementArray(res, allocator, null);
        }
    },
    set_vin: struct {
        left: bool,
        right: bool,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel != null) {
                return SongPlayError.NonNullChannel;
            }

            var res = HardwareEvent.init(0x14);
            if (self.left) {
                res.setBit(7);
            } else {
                res.clearBit(7);
            }
            if (self.right) {
                res.setBit(3);
            } else {
                res.clearBit(3);
            }

            return singleElementArray(res, allocator, null);
        }
    },
    pan: struct {
        left: bool,
        right: bool,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            var res = HardwareEvent.init(0x15);
            if (self.left) {
                res.setBit(@as(u3, @intCast(@intFromEnum(channel.?))) + 4);
            } else {
                res.clearBit(@as(u3, @intCast(@intFromEnum(channel.?))) + 4);
            }
            if (self.right) {
                res.setBit(@intCast(@intFromEnum(channel.?)));
            } else {
                res.clearBit(@intCast(@intFromEnum(channel.?)));
            }

            return singleElementArray(res, allocator, null);
        }
    },
    master_volume: struct {
        side: Side,
        value: u3,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel != null) {
                return SongPlayError.NonNullChannel;
            }

            var res = HardwareEvent.init(0x14);
            if (self.side == .left) {
                try res.setValue(4, self.value);
            } else {
                try res.setValue(0, self.value);
            }
            res.clearBit(7);
            res.clearBit(3);

            return singleElementArray(res, allocator, null);
        }
    },
    period_sweep_disable: struct {
        fn intoHardwareEvents(_: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel != .ch1) {
                return SongPlayError.InvalidChannel;
            }

            var res = HardwareEvent.init(0x00);
            try res.setValue(4, @as(u3, 0));

            return singleElementArray(res, allocator, channel);
        }
    },
    period_sweep_enable: struct {
        pace: u3,
        direction: Direction,
        individual_step: u3,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel != .ch1) {
                return SongPlayError.InvalidChannel;
            }

            if (self.pace == 0) {
                // This will disable period sweep, should use that event instead
                return SongPlayError.InvalidValue;
            }

            var res = HardwareEvent.init(0x00);
            try res.setValue(0, self.individual_step);
            if (self.direction == .increase) {
                res.setBit(3);
            } else {
                res.clearBit(3);
            }
            try res.setValue(4, self.pace);

            return singleElementArray(res, allocator, channel);
        }
    },
    wave_duty: struct {
        duty: WaveDuty,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel != .ch1 and channel != .ch2) {
                return SongPlayError.InvalidChannel;
            }

            const address: u16 = if (channel == .ch1) 0x01 else 0x06;
            const val: u2 = switch (self.duty) {
                .one_in_eight => 0b00,
                .one_in_four => 0b01,
                .one_in_two => 0b10,
                .three_in_four => 0b11,
            };

            var res = HardwareEvent.init(address);
            try res.setValue(6, val);

            return singleElementArray(res, allocator, channel);
        }
    },
    length_timer_disable: struct {
        fn intoHardwareEvents(_: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            const address: u16 = switch (channel.?) {
                .ch1 => 0x04,
                .ch2 => 0x09,
                .ch3 => 0x0E,
                .ch4 => 0x13,
            };

            var res = HardwareEvent.init(address);
            res.clearBit(6);

            return singleElementArray(res, allocator, channel);
        }
    },
    length_timer_enable: struct {
        length: union {
            long: u8,
            short: u6,
        },

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            const address_ctrl: u16, const address_val: u16 = switch (channel.?) {
                .ch1 => .{ 0x04, 0x01 },
                .ch2 => .{ 0x09, 0x06 },
                .ch3 => .{ 0x0E, 0x0B },
                .ch4 => .{ 0x13, 0x10 },
            };

            var res_ctrl = HardwareEvent.init(address_ctrl);
            res_ctrl.setBit(6);

            var res_val = HardwareEvent.init(address_val);
            if (channel == .ch3) {
                try res_val.setValue(0, self.length.long);
            } else {
                try res_val.setValue(0, self.length.short);
            }

            var res = try allocator.alloc(HardwareEvent, 3);
            res[0] = res_ctrl;
            res[1] = res_val;
            res[2] = triggerEvent(channel.?);
            return res;
        }
    },
    dac_disable: struct {
        fn intoHardwareEvents(_: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            const address: u16 = switch (channel.?) {
                .ch1 => 0x02,
                .ch2 => 0x07,
                .ch3 => 0x0A,
                .ch4 => 0x11,
            };

            var res = HardwareEvent.init(address);
            if (channel == .ch3) {
                res.clearBit(7);
            } else {
                try res.setValue(3, @as(u5, 0));
            }

            return singleElementArray(res, allocator, null);
        }
    },
    dac_enable: struct {
        fn intoHardwareEvents(_: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel != .ch3) {
                return SongPlayError.InvalidChannel;
            }

            var res = HardwareEvent.init(0x0A);
            res.setBit(7);

            return singleElementArray(res, allocator, null);
        }
    },
    volume: struct {
        value: u4,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel == .ch3) {
                return SongPlayError.InvalidChannel;
            }

            const address: u16 = switch (channel.?) {
                .ch1 => 0x02,
                .ch2 => 0x07,
                .ch3 => unreachable,
                .ch4 => 0x11,
            };

            var res = HardwareEvent.init(address);
            try res.setValue(4, self.value);

            return singleElementArray(res, allocator, channel);
        }
    },
    envelope_disable: struct {
        fn intoHardwareEvents(_: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel == .ch3) {
                return SongPlayError.InvalidChannel;
            }

            const address: u16 = switch (channel.?) {
                .ch1 => 0x02,
                .ch2 => 0x07,
                .ch3 => unreachable,
                .ch4 => 0x11,
            };

            var res = HardwareEvent.init(address);
            try res.setValue(0, @as(u3, 0));

            return singleElementArray(res, allocator, channel);
        }
    },
    envelope_enable: struct {
        direction: Direction,
        pace: u3,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel == .ch3) {
                return SongPlayError.InvalidChannel;
            }

            const address: u16 = switch (channel.?) {
                .ch1 => 0x02,
                .ch2 => 0x07,
                .ch3 => unreachable,
                .ch4 => 0x11,
            };

            var res = HardwareEvent.init(address);
            try res.setValue(0, self.pace);
            if (self.direction == .decrease) {
                res.clearBit(3);
            } else {
                res.setBit(3);
            }

            return singleElementArray(res, allocator, channel);
        }
    },
    period: struct {
        value: u11,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }
            if (channel == .ch4) {
                return SongPlayError.InvalidChannel;
            }

            const addr_high: u16, const addr_low: u16 = switch (channel.?) {
                .ch1 => .{ 0x04, 0x03 },
                .ch2 => .{ 0x09, 0x08 },
                .ch3 => .{ 0x0E, 0x0D },
                .ch4 => unreachable,
            };

            const value_high: u3 = @intCast((self.value & 0x700) >> 8);
            const value_low: u8 = @intCast(self.value & 0x0FF);

            var res_high = HardwareEvent.init(addr_high);
            try res_high.setValue(0, value_high);

            var res_low = HardwareEvent.init(addr_low);
            try res_low.setValue(0, value_low);

            var res = try allocator.alloc(HardwareEvent, 3);
            res[0] = res_high;
            res[1] = res_low;
            res[2] = triggerEvent(channel.?);
            return res;
        }
    },
    waveform: struct {
        id: usize,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator, song: Song) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }
            if (channel != .ch3) {
                return SongPlayError.InvalidChannel;
            }

            var res = try allocator.alloc(HardwareEvent, 16);
            for (song.waveforms.items) |waveform| {
                if (waveform.id == self.id) {
                    for (waveform.waveform, 0..) |sample, i| {
                        if (i % 2 == 0) {
                            var ev = HardwareEvent.init(0x20 + @as(u16, @intCast(i)) / 2);
                            try ev.setValue(4, sample);
                            res[i / 2] = ev;
                        } else {
                            try res[i / 2].setValue(0, sample);
                        }
                    }
                    break;
                }
            }
            return res;
        }
    },
    output_level: struct {
        level: OutputLevel,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel != .ch3) {
                return SongPlayError.InvalidChannel;
            }

            const val: u2 = switch (self.level) {
                .silent => 0b00,
                .full => 0b01,
                .half => 0b10,
                .quart => 0b11,
            };

            var res = HardwareEvent.init(0x0C);
            try res.setValue(5, val);

            return singleElementArray(res, allocator, null);
        }
    },
    lfsr: struct {
        clock_shift: u4,
        width: LfsrWidth,
        clock_divider: u3,

        fn intoHardwareEvents(self: @This(), channel: ?Channel, allocator: std.mem.Allocator) ![]HardwareEvent {
            if (channel == null) {
                return SongPlayError.NullChannel;
            }

            if (channel != .ch4) {
                return SongPlayError.InvalidChannel;
            }

            var res = HardwareEvent.init(0x12);
            try res.setValue(4, self.clock_shift);
            if (self.width == .wide) {
                res.clearBit(3);
            } else {
                res.setBit(3);
            }
            try res.setValue(0, self.clock_divider);

            return singleElementArray(res, allocator, channel);
        }
    },
};

pub const SongEvent = struct {
    beat: usize,
    tick: usize,
    channel: ?Channel,
    event: SongEventInternal,

    fn intoHardwareEvents(self: SongEvent, allocator: std.mem.Allocator, song: Song) ![]HardwareEvent {
        return switch (self.event) {
            .set_apu => |event| event.intoHardwareEvents(self.channel, allocator),
            .set_vin => |event| event.intoHardwareEvents(self.channel, allocator),
            .pan => |event| event.intoHardwareEvents(self.channel, allocator),
            .master_volume => |event| event.intoHardwareEvents(self.channel, allocator),
            .period_sweep_disable => |event| event.intoHardwareEvents(self.channel, allocator),
            .period_sweep_enable => |event| event.intoHardwareEvents(self.channel, allocator),
            .wave_duty => |event| event.intoHardwareEvents(self.channel, allocator),
            .length_timer_disable => |event| event.intoHardwareEvents(self.channel, allocator),
            .length_timer_enable => |event| event.intoHardwareEvents(self.channel, allocator),
            .dac_disable => |event| event.intoHardwareEvents(self.channel, allocator),
            .dac_enable => |event| event.intoHardwareEvents(self.channel, allocator),
            .volume => |event| event.intoHardwareEvents(self.channel, allocator),
            .envelope_disable => |event| event.intoHardwareEvents(self.channel, allocator),
            .envelope_enable => |event| event.intoHardwareEvents(self.channel, allocator),
            .period => |event| event.intoHardwareEvents(self.channel, allocator),
            .waveform => |event| event.intoHardwareEvents(self.channel, allocator, song),
            .output_level => |event| event.intoHardwareEvents(self.channel, allocator),
            .lfsr => |event| event.intoHardwareEvents(self.channel, allocator),
        };
    }
};

test "SongEvent pan no channel" {
    var song = try Song.init(std.testing.allocator, .{ .bpm = 120, .tpb = 4 });
    defer song.deinit(std.testing.allocator);

    const song_event = SongEvent{
        .beat = 0,
        .tick = 0,
        .channel = null,
        .event = .{
            .pan = .{
                .left = false,
                .right = false,
            },
        },
    };

    const expected_result = SongPlayError.NullChannel;

    const actual_result = song_event.intoHardwareEvents(std.testing.allocator, song);

    try std.testing.expectError(expected_result, actual_result);
}

test "SongEvent pan" {
    var song = try Song.init(std.testing.allocator, .{ .bpm = 120, .tpb = 4 });
    defer song.deinit(std.testing.allocator);

    // test all channels here

    const song_event = SongEvent{
        .beat = 0,
        .tick = 0,
        .channel = .ch1,
        .event = .{
            .pan = .{
                .left = true,
                .right = false,
            },
        },
    };

    const expected_hardware_events = [_]HardwareEvent{
        .{
            .address = 0x15,
            .set_mask = 0b0001_0000,
            .clear_mask = 0b0000_0001,
        },
    };

    const actual_hardware_events = try song_event.intoHardwareEvents(std.testing.allocator, song);
    defer std.testing.allocator.free(actual_hardware_events);

    try std.testing.expectEqualSlices(HardwareEvent, &expected_hardware_events, actual_hardware_events);
}

pub const SongComposer = struct {
    allocator: std.mem.Allocator,
    events: *std.ArrayList(SongEvent),
    tpb: usize,

    cursors: [4]usize,
    globalCursor: usize,

    volumes: [4]u4,

    selectedChannel: ?Channel,

    pub fn init(allocator: std.mem.Allocator, events: *std.ArrayList(SongEvent), tpb: usize) SongComposer {
        return SongComposer{
            .allocator = allocator,
            .events = events,
            .tpb = tpb,
            .cursors = .{ 0, 0, 0, 0 },
            .globalCursor = 0,
            .volumes = .{ 15, 15, 15, 15 },
            .selectedChannel = null,
        };
    }

    fn addEvent(self: SongComposer, event: SongEventInternal) void {
        const cursor = if (self.selectedChannel == null)
            self.globalCursor
        else
            self.cursors[@intFromEnum(self.selectedChannel.?)];
        const beat = cursor / self.tpb;
        const tick = cursor % self.tpb;

        self.events.append(self.allocator, .{
            .beat = beat,
            .tick = tick,
            .channel = self.selectedChannel,
            .event = event,
        }) catch {
            @panic("memory error during song construction");
        };
    }

    pub fn syncTo(self: SongComposer, whom: ?Channel) SongComposer {
        const value = if (whom == null)
            self.globalCursor
        else
            self.cursors[@intFromEnum(whom.?)];

        const ref = if (self.selectedChannel == null)
            *self.globalCursor
        else
            *self.cursors[@intFromEnum(self.selectedChannel.?)];

        ref.* = value;

        return self;
    }

    pub fn enable(self: SongComposer) SongComposer {
        if (self.selectedChannel == null) {
            return self.apu(true);
        } else {
            return self.dac(true);
        }
    }

    pub fn disable(self: SongComposer) SongComposer {
        if (self.selectedChannel == null) {
            return self.apu(false);
        } else {
            return self.dac(false);
        }
    }

    pub fn defaults(self: SongComposer) SongComposer {
        if (self.selectedChannel == null) {
            return self
                .vin(false, false)
                .masterVolume(.left, 7)
                .masterVolume(.right, 7);
        }
        return switch (self.selectedChannel.?) {
            .ch1 => self
                .pan(true, true)
                .periodSweep(null)
                .lengthTimer(null)
                .duty(.one_in_two)
                .envelope(null)
                .volume(15),
            .ch2 => self
                .pan(true, true)
                .lengthTimer(null)
                .duty(.one_in_two)
                .envelope(null)
                .volume(15),
            .ch3 => self
                .pan(true, true)
                .lengthTimer(null)
                .outputLevel(.full),
            .ch4 => self
                .pan(true, true)
                .lengthTimer(null)
                .envelope(null)
                .volume(15),
        };
    }

    pub fn apu(self: SongComposer, state: bool) SongComposer {
        self.addEvent(SongEventInternal{
            .set_apu = .{
                .state = state,
            },
        });
        return self;
    }

    pub fn dac(self: SongComposer, state: bool) SongComposer {
        if (state) {
            if (self.selectedChannel == .ch3) {
                self.addEvent(SongEventInternal{
                    .dac_enable = .{},
                });
            } else if (self.selectedChannel != null) {
                // other channels don't need a specific dac enable, just reset their volume to reenable their dac
                self.addEvent(SongEventInternal{
                    .volume = .{
                        .value = self.volumes[@intFromEnum(self.selectedChannel.?)],
                    },
                });
            } else {
                @panic("cannot set dac globally, you probably want .apu");
            }
        } else {
            self.addEvent(SongEventInternal{
                .dac_disable = .{},
            });
        }
        return self;
    }

    pub fn vin(self: SongComposer, left: bool, right: bool) SongComposer {
        self.addEvent(SongEventInternal{
            .set_vin = .{
                .left = left,
                .right = right,
            },
        });
        return self;
    }

    pub fn masterVolume(self: SongComposer, side: Side, value: u3) SongComposer {
        self.addEvent(SongEventInternal{
            .master_volume = .{
                .side = side,
                .value = value,
            },
        });
        return self;
    }

    pub fn pan(self: SongComposer, left: bool, right: bool) SongComposer {
        self.addEvent(SongEventInternal{
            .pan = .{
                .left = left,
                .right = right,
            },
        });
        return self;
    }

    pub fn periodSweep(self: SongComposer, someParameterIHaventDecidedYet: ?u8) SongComposer {
        if (someParameterIHaventDecidedYet == null) {
            self.addEvent(SongEventInternal{
                .period_sweep_disable = .{},
            });
            return self;
        } else {
            // TODO
        }
        return self;
    }

    pub fn lengthTimer(self: SongComposer, someParameterIHaventDecidedYet: ?u8) SongComposer {
        if (someParameterIHaventDecidedYet == null) {
            self.addEvent(SongEventInternal{
                .length_timer_disable = .{},
            });
        } else {
            // TODO
        }
        return self;
    }

    pub fn duty(self: SongComposer, value: WaveDuty) SongComposer {
        self.addEvent(SongEventInternal{
            .wave_duty = .{
                .duty = value,
            },
        });
        return self;
    }

    pub fn envelope(self: SongComposer, someParameterIHaventDecidedYet: ?u8) SongComposer {
        if (someParameterIHaventDecidedYet == null) {
            self.addEvent(SongEventInternal{
                .envelope_disable = .{},
            });
        } else {
            // TODO
        }
        return self;
    }

    pub fn volume(self: SongComposer, value: u4) SongComposer {
        var next = self;
        if (self.selectedChannel != null and value > 0) {
            // remember last nonzero value for when we want to reset the volume (.apu or .note)
            next.volumes[@intFromEnum(self.selectedChannel.?)] = value;
        }
        next.addEvent(SongEventInternal{
            .volume = .{
                .value = value,
            },
        });
        return next;
    }

    pub fn outputLevel(self: SongComposer, value: OutputLevel) SongComposer {
        self.addEvent(SongEventInternal{
            .output_level = .{
                .level = value,
            },
        });
        return self;
    }

    pub fn channel(self: SongComposer, ch: ?Channel) SongComposer {
        var next = self;
        next.selectedChannel = ch;
        return next;
    }

    pub fn phrase(
        self: SongComposer,
        comptime data: []const struct { ?[]const u8, usize },
    ) SongComposer {
        var comp = self;
        inline for (data) |n| {
            comp = comp.note(n);
        }
        return comp;
    }

    pub fn note(
        self: SongComposer,
        comptime data: struct { ?[]const u8, usize },
    ) SongComposer {
        if (self.selectedChannel == null) {
            @panic("attempt to call .note without a selected channel");
        }
        return (if (data.@"0" == null)
            self.rest(data.@"1")
        else
            self
                // set volume to latest in case previously we did a rest which sets it to 0
                .volume(self.volumes[@intFromEnum(self.selectedChannel.?)])
                .pitch(data.@"0".?))
            .advance(data.@"1");
    }

    pub fn rest(self: SongComposer, amount: usize) SongComposer {
        return self.volume(0).advance(amount);
    }

    pub fn pitch(self: SongComposer, comptime data: []const u8) SongComposer {
        if (self.selectedChannel == null) {
            @panic("attempt to set pitch without a selected channel");
        }
        self.addEvent(SongEventInternal{
            .period = .{
                .value = freqToPeriodValue(noteToFreq(data), self.selectedChannel.?) catch @panic("invalid note"),
            },
        });
        return self;
    }

    pub fn advance(self: SongComposer, amount: usize) SongComposer {
        if (self.selectedChannel == null) {
            @panic("attempt to advance global cursor (use .syncTo instead)");
        }
        var next = self;
        next.cursors[@intFromEnum(self.selectedChannel.?)] += amount;
        return next;
    }

    /// this does nothing, it just helps keep the syntax neater
    pub fn finish(_: SongComposer) void {}
};

pub const Song = struct {
    metadata: SongMetadata,
    waveforms: std.ArrayList(SongWaveform),
    events: std.ArrayList(SongEvent),

    pub fn init(allocator: std.mem.Allocator, metadata: SongMetadata) !Song {
        return Song{
            .metadata = metadata,
            .waveforms = try std.ArrayList(SongWaveform).initCapacity(allocator, 16),
            .events = try std.ArrayList(SongEvent).initCapacity(allocator, 128),
        };
    }

    pub fn deinit(self: *Song, allocator: std.mem.Allocator) void {
        self.waveforms.deinit(allocator);
        self.events.deinit(allocator);
    }

    pub fn compose(self: *Song, allocator: std.mem.Allocator) SongComposer {
        return SongComposer.init(allocator, &self.events, self.metadata.tpb);
    }
};

pub const HardwareEvent = struct {
    address: u16,
    set_mask: u8,
    clear_mask: u8,

    pub fn init(address: u16) HardwareEvent {
        return HardwareEvent{
            .address = address,
            .set_mask = 0,
            .clear_mask = 0,
        };
    }

    pub fn set(self: *HardwareEvent, mask: u8) void {
        self.set_mask |= mask;
        self.clear_mask &= ~mask;
    }

    pub fn setBit(self: *HardwareEvent, bit: u3) void {
        self.set(@as(u8, 1) << bit);
    }

    pub fn clear(self: *HardwareEvent, mask: u8) void {
        self.clear_mask |= mask;
        self.set_mask &= ~mask;
    }

    pub fn clearBit(self: *HardwareEvent, bit: u3) void {
        self.clear(@as(u8, 1) << bit);
    }

    pub fn setValue(self: *HardwareEvent, comptime shift: u3, value: anytype) SongPlayError!void {
        const value_type = @typeInfo(@TypeOf(value));
        if (value_type != .int or value_type.int.signedness != .unsigned or value_type.int.bits + shift > 8) {
            @compileError(std.fmt.comptimePrint("invalid value passed to setValue: {} (shift={})", .{ value_type, shift }));
        }
        const width = value_type.int.bits;

        const mask: u8 = if (width == 8) 0xFF else (@as(u8, 1) << width) - 1;
        const shifted_mask = mask << shift;

        const shifted_value = @as(u8, @intCast(value)) << shift;

        var tmp = HardwareEvent.init(self.address);
        tmp.set_mask = shifted_value & shifted_mask;
        tmp.clear_mask = ~tmp.set_mask & shifted_mask;

        self.* = try self.merge(tmp);
    }

    pub fn apply(self: HardwareEvent, val: u8) u8 {
        return (val | self.set_mask) & ~self.clear_mask;
    }

    pub fn merge(self: HardwareEvent, other: HardwareEvent) SongPlayError!HardwareEvent {
        if (self.address != other.address) {
            return SongPlayError.InvalidMergeParameters;
        }

        const conflict = (self.set_mask & other.clear_mask) | (other.set_mask & self.clear_mask);
        if (conflict != 0) {
            return SongPlayError.ImpossibleMerge;
        }

        return HardwareEvent{
            .address = self.address,
            .set_mask = self.set_mask | other.set_mask,
            .clear_mask = self.clear_mask | other.clear_mask,
        };
    }
};

/// We need to make sure that events with addresses that could be a trigger event happen last. Luckily, the trigger address is always last.
fn sortTriggerResigsterWritesLast(_: void, a: HardwareEvent, b: HardwareEvent) bool {
    // Ensure the audio master control is ALWAYS FIRST
    if (a.address == 0x16) {
        return true;
    } else if (b.address == 0x16) {
        return false;
    }

    return a.address < b.address;
}

pub fn play(song: Song, allocator: std.mem.Allocator) ![][]HardwareEvent {
    var hardware_events = try std.ArrayList(std.ArrayList(HardwareEvent)).initCapacity(allocator, 256);

    defer {
        for (hardware_events.items) |*event| {
            event.deinit(allocator);
        }
        hardware_events.deinit(allocator);
    }

    for (song.events.items) |song_event| {
        const location = song_event.beat * song.metadata.tpb + song_event.tick;
        {
            const currlen = hardware_events.items.len;
            if (currlen <= location) {
                try hardware_events.resize(allocator, location + 1);
                for (currlen..(hardware_events.items.len)) |i| {
                    hardware_events.items[i] = try std.ArrayList(HardwareEvent).initCapacity(allocator, 32);
                }
            }
        }

        const new_hardware_events = try song_event.intoHardwareEvents(allocator, song);
        defer allocator.free(new_hardware_events);

        for (new_hardware_events) |hardware_event| {
            var added = false;
            for (hardware_events.items[location].items, 0..) |other_event, i| {
                if (other_event.address == hardware_event.address) {
                    hardware_events.items[location].items[i] = hardware_event.merge(other_event) catch |e| {
                        std.debug.print("Failed to merge hardware events: {any}\n", .{e});
                        std.debug.print("Current hardware event: {any}\n", .{hardware_event});
                        std.debug.print("Other hardware event: {any}\n", .{other_event});
                        return e;
                    };
                    added = true;
                }
            }
            if (!added) {
                try hardware_events.items[location].append(allocator, hardware_event);
            }
        }
    }

    var intermediate = try std.ArrayList([]HardwareEvent).initCapacity(allocator, hardware_events.items.len);
    defer intermediate.deinit(allocator);
    errdefer for (intermediate.items) |arr| {
        allocator.free(arr);
    };
    for (hardware_events.items) |*eventlist| {
        const writes = try eventlist.toOwnedSlice(allocator);
        std.sort.pdq(HardwareEvent, writes, {}, sortTriggerResigsterWritesLast);
        try intermediate.append(allocator, writes);
    }
    return intermediate.toOwnedSlice(allocator);
}

fn triggerEvent(ch: Channel) HardwareEvent {
    const addr: u16 = switch (ch) {
        .ch1 => 0x04,
        .ch2 => 0x09,
        .ch3 => 0x0E,
        .ch4 => 0x13,
    };
    var ev = HardwareEvent.init(addr);
    ev.setBit(7);
    return ev;
}

fn singleElementArray(ev: HardwareEvent, allocator: std.mem.Allocator, retrigger: ?Channel) ![]HardwareEvent {
    var res = try allocator.alloc(HardwareEvent, if (retrigger == null) 1 else 2);
    res[0] = ev;
    if (retrigger) |ch| {
        res[1] = triggerEvent(ch);
    }
    return res;
}

fn noteToFreq(comptime note: []const u8) f64 {
    if (note.len < 2 or note.len > 3) {
        @compileError(std.fmt.comptimePrint("incorrect note notation: {s}", .{note}));
    }

    const letter = note[0];
    if (letter < 'A' or letter > 'G') {
        @compileError("incorrect semitone");
    }

    comptime var isSharp = false;
    if (note.len == 3) {
        if (note[1] == '#') {
            isSharp = true;
        } else {
            @compileError("invalid accent, only sharp is supported");
        }
    }

    const number = note[note.len - 1];
    if (number < '2' or number > '7') {
        @compileError("out of range octave");
    }

    const octave: comptime_int = number - '0';
    const semitone_from_C = switch (letter) {
        'C' => if (isSharp) 1 else 0,
        'D' => if (isSharp) 3 else 2,
        'E' => if (isSharp) @compileError("invalid sharp") else 4,
        'F' => if (isSharp) 6 else 5,
        'G' => if (isSharp) 8 else 7,
        'A' => if (isSharp) 10 else 9,
        'B' => if (isSharp) @compileError("invalid sharp") else 11,
        else => @compileError("invalid note"),
    };

    const semitone_from_A4 =
        (octave - 4) * 12 +
        (semitone_from_C - 9); // because A = 9 in C-based scale

    return 440.0 * @exp2(@as(f64, @floatFromInt(semitone_from_A4)) / 12.0);
}

fn freqToPeriodValue(freq: f64, ch: Channel) SongPlayError!u11 {
    const numerator: f64 = @floatFromInt(if (ch == .ch3) @as(u32, 1) << 16 else @as(u32, 1) << 17);
    if (freq <= 0) {
        return SongPlayError.InvalidValue;
    }

    const period = 2048.0 - numerator / freq;
    const result: u32 = @intFromFloat(@round(period));
    if (result < 0 or result > 0x7FF) {
        return SongPlayError.InvalidValue;
    }
    return @intCast(result);
}
