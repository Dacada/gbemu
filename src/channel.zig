const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;

inline fn dac(sample: u4) f32 {
    return switch (sample) {
        0b0000 => 15.0 / 15.0,
        0b0001 => 13.0 / 15.0,
        0b0010 => 11.0 / 15.0,
        0b0011 => 9.0 / 15.0,
        0b0100 => 7.0 / 15.0,
        0b0101 => 5.0 / 15.0,
        0b0110 => 3.0 / 15.0,
        0b0111 => 1.0 / 15.0,
        0b1000 => -1.0 / 15.0,
        0b1001 => -3.0 / 15.0,
        0b1010 => -5.0 / 15.0,
        0b1011 => -7.0 / 15.0,
        0b1100 => -9.0 / 15.0,
        0b1101 => -11.0 / 15.0,
        0b1110 => -13.0 / 15.0,
        0b1111 => -15.0 / 15.0,
    };
}

pub const Channel2 = struct {
    poweredOff: bool,
    active: bool,
    dacEnabled: bool,

    periodCounter: u11,
    dutyCounter: u3,
    dacHold: f32,
    currentEnvelope: u4,
    divtickdiv: u3,
    lengthTimer: u6, // TODO: u8 for channel 3
    sweepPaceCounter: u3,

    waveDuty: u2,
    initialLengthTimer: u6,
    initialVolume: u4,
    envDir: u1,
    sweepPace: u3,
    period: u11,
    lengthEnable: u1,

    pub fn init() Channel2 {
        return Channel2{
            .poweredOff = false,
            .active = false,
            .dacEnabled = false,
            .periodCounter = 0,
            .dutyCounter = 0,
            .dacHold = 0.0,
            .currentEnvelope = 0,
            .divtickdiv = 0,
            .lengthTimer = 0,
            .sweepPaceCounter = 0,
            .waveDuty = undefined,
            .initialLengthTimer = undefined,
            .initialVolume = undefined,
            .envDir = undefined,
            .sweepPace = undefined,
            .period = undefined,
            .lengthEnable = undefined,
        };
    }

    pub fn isActive(self: *const Channel2) bool {
        return self.active and self.dacEnabled;
    }

    pub fn poweroff(self: *Channel2) void {
        self.poweredOff = true;
        self.active = false;
        self.poke(0, 0);
        // DMG ONLY -- Length timers are unaffected by power off only on monochrome models
        //self.poke(1, 0);
        self.poke(2, 0);
        self.poke(3, 0);
        self.poke(4, 0);
        self.periodCounter = 0;
        self.dutyCounter = 0;
        self.dacHold = 0.0;
        self.currentEnvelope = 0;
    }

    pub fn tick(self: *Channel2) f32 {
        if (!self.dacEnabled) {
            return 0.0;
        }
        if (!self.active) {
            return dac(0);
        }
        self.periodCounter +%= 1;
        if (self.periodCounter == 0) {
            self.periodCounter = self.period;

            var dacSample: u8 = self.sample();
            if (self.sweepPace != 0) {
                dacSample *= self.currentEnvelope;
                dacSample /= 15;
            }
            self.dacHold = dac(@intCast(dacSample));
        }
        return self.dacHold;
    }

    fn sample(self: *Channel2) u4 {
        const waveforms: [4][8]u4 = .{
            .{ 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0x0 },
            .{ 0x0, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0x0 },
            .{ 0x0, 0xF, 0xF, 0xF, 0xF, 0x0, 0x0, 0x0 },
            .{ 0xF, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xF },
        };
        defer self.dutyCounter +%= 1;
        return waveforms[self.waveDuty][self.dutyCounter];
    }

    pub fn divtick(self: *Channel2) void {
        self.divtickdiv +%= 1;
        if (self.divtickdiv % 2 == 0) {
            self.soundLengthTick();
        }
        if (self.divtickdiv == 0) {
            self.envelopeSweepTick();
        }
    }

    fn soundLengthTick(self: *Channel2) void {
        if (self.lengthEnable == 0) {
            return;
        }
        self.lengthTimer +%= 1;
        if (self.lengthTimer == 0) {
            self.active = false;
        }
    }

    fn envelopeSweepTick(self: *Channel2) void {
        if (self.sweepPace == 0) {
            return;
        }
        self.sweepPaceCounter +%= 1;
        if (self.sweepPaceCounter >= self.sweepPace) {
            self.sweepPaceCounter = 0;
            if (self.envDir == 0) {
                self.currentEnvelope -|= 1;
            } else {
                self.currentEnvelope +|= 1;
            }
        }
    }

    fn trigger(self: *Channel2) void {
        self.active = true;
        if (self.lengthTimer == 0) {
            self.lengthTimer = self.initialLengthTimer;
        }
        self.periodCounter = self.period;
        self.sweepPaceCounter = 0;
        self.currentEnvelope = self.initialVolume;
    }

    pub fn peek(self: *Channel2, addr: u16) u8 {
        switch (addr) {
            0 => return 0xFF,
            1 => {
                var ret: u8 = 0;
                ret |= self.waveDuty;
                ret <<= 6;
                ret |= self.initialLengthTimer;
                return ret;
            },
            2 => {
                var ret: u8 = 0;
                ret |= self.initialVolume;
                ret <<= 1;
                ret |= self.envDir;
                ret <<= 3;
                ret |= self.sweepPace;
                return ret;
            },
            3 => return @intCast(self.period & 0xFF),
            4 => {
                var ret: u8 = 0;
                ret |= 1;
                ret <<= 1;
                ret |= self.lengthEnable;
                ret <<= 3;
                ret |= 0b111;
                ret <<= 3;
                ret |= @intCast(self.period & 0x700);
                return ret;
            },
            else => unreachable,
        }
    }

    pub fn poke(self: *Channel2, addr: u16, val: u8) void {
        if (self.poweredOff and addr != 1) { // DMG ONLY -- Length timers are unaffected by power off only on monochrome models
            return;
        }

        switch (addr) {
            0 => {},
            1 => {
                self.waveDuty = @intCast((val & 0b11_000000) >> 6);
                self.initialLengthTimer = @intCast(val & 0b00_111111);
            },
            2 => {
                self.initialVolume = @intCast((val & 0xF0) >> 4);
                self.envDir = @intCast((val & 0b0000_1000) >> 3);
                self.sweepPace = @intCast(val & 0b0000_0111);
            },
            3 => self.period = (self.period & 0x700) | val,
            4 => {
                self.lengthEnable = @intCast((val & 0b0100_0000) >> 6);
                self.period = (self.period & 0x0FF) | (@as(u11, @intCast(val & 0b0000_0111)) << 8);
            },
            else => unreachable,
        }
    }

    pub fn read(self: *Channel2, addr: u16) struct { MemoryFlag, u8 } {
        if (addr == 0) {
            return .{ .{ .illegal = true }, self.peek(addr) };
        }
        return .{ .{}, self.peek(addr) };
    }

    pub fn write(self: *Channel2, addr: u16, val: u8) MemoryFlag {
        self.poke(addr, val);
        if (self.envDir == 0 and self.initialVolume == 0) {
            self.dacEnabled = false;
        } else {
            self.dacEnabled = true;
        }
        if (!self.poweredOff and addr == 4 and val & 0b1000_0000 != 0) {
            self.trigger();
        }
        return .{};
    }
};

const std = @import("std");

test "dac() maps 4-bit samples to [-1,1]" {
    const eps: f32 = 1e-6;

    try std.testing.expectApproxEqAbs(@as(f32, 1.0), dac(@as(u4, 0b0000)), eps);
    try std.testing.expectApproxEqAbs(@as(f32, 1.0) / 15.0, dac(@as(u4, 0b0111)), eps);
    try std.testing.expectApproxEqAbs(-(@as(f32, 1.0) / 15.0), dac(@as(u4, 0b1000)), eps);
    try std.testing.expectApproxEqAbs(-@as(f32, 1.0), dac(@as(u4, 0b1111)), eps);
}

test "init starts inert and inactive" {
    var ch = Channel2.init();

    try std.testing.expect(!ch.isActive());
    try std.testing.expect(!ch.poweredOff);
    try std.testing.expect(!ch.dacEnabled);
    try std.testing.expectEqual(@as(u11, 0), ch.periodCounter);
    try std.testing.expectEqual(@as(u3, 0), ch.dutyCounter);
    try std.testing.expectEqual(@as(f32, 0.0), ch.dacHold);
    try std.testing.expectEqual(@as(u4, 0), ch.currentEnvelope);
}

test "write(2) enables/disables DAC according to initialVolume/envDir" {
    var ch = Channel2.init();

    // volume=0, envDir=0 -> DAC disabled
    _ = ch.write(2, 0x00);
    try std.testing.expect(!ch.dacEnabled);

    // volume>0 -> DAC enabled
    _ = ch.write(2, 0x10); // initialVolume=1
    try std.testing.expect(ch.dacEnabled);

    // volume=0 but envDir=1 -> DAC enabled
    _ = ch.write(2, 0x08); // envDir=1
    try std.testing.expect(ch.dacEnabled);

    // volume=0 and envDir=0 -> DAC disabled again
    _ = ch.write(2, 0x00);
    try std.testing.expect(!ch.dacEnabled);
}

test "poke/peek round-trips for NR21 (addr 1) and NR23 (addr 3)" {
    var ch = Channel2.init();

    // NR21: waveDuty in bits 7..6, length in bits 5..0
    const wd: u2 = 0b10;
    const len: u6 = 0b10_0000;
    _ = ch.write(1, (@as(u8, wd) << 6) | len);
    try std.testing.expectEqual((@as(u8, wd) << 6) | len, ch.peek(1));

    // NR23: low 8 bits of period
    _ = ch.write(3, 0x34);
    try std.testing.expectEqual(@as(u8, 0x34), ch.peek(3));
}

test "trigger sets active, length, envelope and period counters" {
    var ch = Channel2.init();

    // Set NR21 (wave duty + length)
    const wave_duty: u2 = 0b01;
    const init_len: u6 = 10;
    _ = ch.write(1, (@as(u8, wave_duty) << 6) | init_len);

    // Set NR22 (volume/env)
    const init_vol: u4 = 0x0F;
    _ = ch.write(2, (@as(u8, init_vol) << 4) | 0b000); // envDir=0, pace=0

    // Set period to 0x7FF so the very first tick wraps and samples immediately
    _ = ch.write(3, 0xFF);
    _ = ch.write(4, 0x87); // trigger=1, lengthEnable=0, upper period=0b111

    try std.testing.expect(ch.active);
    try std.testing.expectEqual(init_len, ch.lengthTimer);
    try std.testing.expectEqual(@as(u11, 0x7FF), ch.period);
    try std.testing.expectEqual(@as(u11, 0x7FF), ch.periodCounter);
    try std.testing.expectEqual(init_vol, ch.currentEnvelope);
    try std.testing.expectEqual(@as(u3, 0), ch.sweepPaceCounter);
}

test "tick() obeys dacEnabled and active; waveform progression matches table" {
    const eps: f32 = 1e-6;
    var ch = Channel2.init();

    // 1) DAC disabled -> 0.0 output
    _ = ch.write(2, 0x00);
    try std.testing.expectApproxEqAbs(@as(f32, 0.0), ch.tick(), eps);

    // 2) DAC enabled but not active -> dac(0) baseline (1.0)
    _ = ch.write(2, 0x10); // volume=1 enables DAC
    try std.testing.expectApproxEqAbs(@as(f32, 1.0), ch.tick(), eps);

    // 3) Now set up a predictable run: waveDuty=0, volume=15, period=0x7FF, trigger
    _ = ch.write(1, (0b00 << 6) | 0); // waveDuty=0, length=0
    _ = ch.write(2, (0x0F << 4) | 0b000); // vol=15, envDir=0, pace=0
    _ = ch.write(3, 0xFF);
    _ = ch.write(4, 0x87); // trigger, upper period=0b111

    // With waveDuty=0, the first 7 samples are 0xF (high), then 0x0.
    try std.testing.expectApproxEqAbs(-1.0, ch.tick(), eps);

    // Next 6 ticks: still 'high' (same value)
    inline for (0..6) |_| {
        try std.testing.expectApproxEqAbs(-1.0, ch.tick(), eps);
    }

    // 8th tick of the duty cycle returns 0x0 -> dac(0) = 1.0
    try std.testing.expectApproxEqAbs(@as(f32, 1.0), ch.tick(), eps);
}

test "length timer disables channel when enabled and wraps" {
    var ch = Channel2.init();

    // Prepare: length=63 so one length tick wraps to 0 and disables.
    _ = ch.write(1, (0b01 << 6) | 63); // any wave duty, length=63
    _ = ch.write(2, 0x10); // enable DAC
    _ = ch.write(4, 0xC0); // trigger + lengthEnable=1 (upper period=0)

    try std.testing.expect(ch.active);

    // divtick: length ticks on every even divtickdiv value after increment.
    ch.divtick(); // divtickdiv=1 -> no length tick
    ch.divtick(); // divtickdiv=2 -> lengthTimer increments 63 -> 0, active=false

    try std.testing.expect(!ch.active);
}

test "envelope sweep pacing increases/decreases volume as configured" {
    var ch = Channel2.init();

    // initialVolume=4, envDir=1 (increase), sweepPace=2
    _ = ch.write(2, (@as(u8, 4) << 4) | (1 << 3) | 2);
    _ = ch.write(4, 0x80); // trigger to load currentEnvelope and reset pace counter

    try std.testing.expectEqual(@as(u4, 4), ch.currentEnvelope);

    // envelopeSweepTick runs when divtickdiv wraps to 0 (every 8 divticks).
    // With sweepPace=2, need two such moments -> 16 divticks to bump envelope by +1.
    inline for (0..16) |_| ch.divtick();

    try std.testing.expectEqual(@as(u4, 5), ch.currentEnvelope);
}

test "poweroff blocks writes (except NR21 on DMG) and prevents trigger" {
    var ch = Channel2.init();

    ch.poweroff();

    // Attempt to trigger while powered off -> must not activate
    _ = ch.write(4, 0x80);
    try std.testing.expect(!ch.active);

    // Write to NR23 while powered off should be ignored
    _ = ch.write(3, 0xAA);
    try std.testing.expectEqual(@as(u11, 0), ch.period & 0xFF);

    // NR21 is still writable on power-off (DMG quirk in this implementation)
    _ = ch.write(1, (0b11 << 6) | 17);
    try std.testing.expectEqual(@as(u8, (0b11 << 6) | 17), ch.peek(1));
}

test "isActive only true when both active and DAC enabled" {
    var ch = Channel2.init();

    // Neither active nor DAC -> false
    try std.testing.expect(!ch.isActive());

    // DAC enabled, not active -> false
    _ = ch.write(2, 0x10);
    try std.testing.expect(!ch.isActive());

    // Trigger -> active now true and DAC already enabled -> true
    _ = ch.write(4, 0x80);
    try std.testing.expect(ch.isActive());

    // Disable DAC -> isActive becomes false
    _ = ch.write(2, 0x00);
    try std.testing.expect(!ch.isActive());
}
