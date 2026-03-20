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

const SongMetadata = struct {
    bpm: usize,
    tpb: usize,
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

pub const SongEvent = struct {
    beat: usize,
    tick: usize,
    channel: ?Channel,
    event: union(enum) {
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
            side: enum {
                left,
                right,
            },
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
            level: enum {
                silent,
                full,
                half,
                quart,
            },

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
            width: enum {
                wide,
                narrow,
            },
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
    },

    fn intoHardwareEvents(self: SongEvent, allocator: std.mem.Allocator, song: Song) ![]HardwareEvent {
        return switch (self.event) {
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

    pub fn addNotesCh2(
        self: *Song,
        allocator: std.mem.Allocator,
        start_beat: usize,
        start_tick: usize,
        wave_duty: WaveDuty,
        volume: u4,
        comptime notes: []const struct {
            ?[]const u8,
            usize,
        },
    ) !void {
        var current_beat = start_beat;
        var current_tick = start_tick;
        inline for (notes) |note| {
            if (note[0]) |actualNote| {
                try self.addNoteCh2(allocator, actualNote, current_beat, current_tick, wave_duty, volume);
            } else {
                try self.addRestCh2(allocator, current_beat, current_tick);
            }
            current_tick += note[1];
            current_beat += current_tick / self.metadata.tpb;
            current_tick %= self.metadata.tpb;
        }
    }

    pub fn addNoteCh2(
        self: *Song,
        allocator: std.mem.Allocator,
        comptime note: []const u8,
        start_beat: usize,
        start_tick: usize,
        wave_duty: WaveDuty,
        volume: u4,
    ) !void {
        // Set up the channel

        // We don't use the hardware length timer for music
        try self.events.append(allocator, SongEvent{
            .beat = start_beat,
            .tick = start_tick,
            .channel = .ch2,
            .event = .{
                .length_timer_disable = .{},
            },
        });

        // We don't use the hardware envelope for melody
        try self.events.append(allocator, SongEvent{
            .beat = start_beat,
            .tick = start_tick,
            .channel = .ch2,
            .event = .{
                .envelope_disable = .{},
            },
        });

        // Set up the duty cycle
        try self.events.append(allocator, SongEvent{
            .beat = start_beat,
            .tick = start_tick,
            .channel = .ch2,
            .event = .{
                .wave_duty = .{
                    .duty = wave_duty,
                },
            },
        });

        // Set the note's frequency
        try self.events.append(allocator, SongEvent{
            .beat = start_beat,
            .tick = start_tick,
            .channel = .ch2,
            .event = .{
                .period = .{
                    .value = try freqToPeriodValue(noteToFreq(note), .ch2),
                },
            },
        });

        // Set the note's volume
        try self.events.append(allocator, SongEvent{
            .beat = start_beat,
            .tick = start_tick,
            .channel = .ch2,
            .event = .{
                .volume = .{
                    .value = volume,
                },
            },
        });
    }

    pub fn addRestCh2(
        self: *Song,
        allocator: std.mem.Allocator,
        start_beat: usize,
        start_tick: usize,
    ) !void {
        // Just mute the channel
        try self.events.append(allocator, SongEvent{
            .beat = start_beat,
            .tick = start_tick,
            .channel = .ch2,
            .event = .{
                .volume = .{
                    .value = 0,
                },
            },
        });
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
    const semitone: comptime_int = switch (letter) {
        'A' => if (isSharp) 1 else 0,
        'B' => if (isSharp) @compileError("sharp doesn't apply to this note") else 2,
        'C' => if (isSharp) 4 else 3,
        'D' => if (isSharp) 6 else 5,
        'E' => if (isSharp) @compileError("sharp doesn't apply to this note") else 7,
        'F' => if (isSharp) 9 else 8,
        'G' => if (isSharp) 11 else 10,
        else => @compileError("invalid note"),
    };

    const increment: comptime_float = @floatFromInt((octave - 4) * 12 + semitone);
    return 440.0 * @exp2(increment / 12.0);
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
