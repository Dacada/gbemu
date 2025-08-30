const MemoryFlag = @import("memory_flag.zig").MemoryFlag;

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

const SweepPeriodUnit = struct {
    pace: u3,
    individual_step: u3,
    direction: u1,

    counter: u3,
    enabled: bool,
    shadow: u11,

    fn init() SweepPeriodUnit {
        return SweepPeriodUnit{
            .pace = undefined,
            .individual_step = undefined,
            .direction = undefined,

            .counter = 0,
            .enabled = false,
            .shadow = 0,
        };
    }

    fn tick(self: *SweepPeriodUnit, control: *ControlUnit, period: *PeriodUnit) void {
        self.counter +%= 1;
        if (self.counter < self.pace) {
            return;
        }
        self.counter = 0;

        // Entire confusing and weird implementation from Pan Docs
        if (self.enabled and self.pace != 0) {
            const new_period = self.period_calculation();
            self.overflow_check(new_period, control);
            if (new_period <= 0x7FF and self.individual_step != 0) {
                self.shadow = @intCast(new_period);
                period.period = @intCast(new_period);
                self.overflow_check(self.period_calculation(), control);
            }
        }
    }

    fn period_calculation(self: *const SweepPeriodUnit) u12 {
        const shifted = self.shadow >> self.individual_step;
        // store it as a u12 to check for "overflow" later
        var new_period: u12 = @intCast(self.shadow);
        if (self.direction == 0) {
            new_period += @intCast(shifted);
        } else {
            // shifted will always be smaller since it's the result of right shifting that value
            new_period -= @intCast(shifted);
        }
        return new_period;
    }

    fn overflow_check(self: *SweepPeriodUnit, period: u12, control: *ControlUnit) void {
        if (period > 0x7FF) {
            control.active = false;
            self.enabled = false;
        }
    }

    fn reset(self: *SweepPeriodUnit, period: *const PeriodUnit, control: *ControlUnit) void {
        self.shadow = period.period;
        self.counter = 0;
        self.enabled = self.pace != 0 or self.individual_step != 0;

        // Pan Docs says: If the individual step is non-zero, frequency calculation and overflow check are performed immediately.
        if (self.individual_step != 0) {
            self.overflow_check(self.period_calculation(), control);
        }
    }
};

const VolumeUnit = struct {
    pace: u3,
    direction: u1,
    initial_volume: u4,

    counter: u3,
    volume: u4,

    inline fn init() VolumeUnit {
        return VolumeUnit{
            .pace = undefined,
            .direction = undefined,
            .initial_volume = undefined,

            .counter = 0,
            .volume = 0,
        };
    }

    fn tick(self: *VolumeUnit) void {
        if (self.pace == 0) {
            return;
        }

        self.counter +%= 1;
        if (self.counter >= self.pace) {
            self.counter = 0;
            if (self.direction == 0) {
                self.volume -|= 1;
            } else {
                self.volume +|= 1;
            }
        }
    }

    fn reset(self: *VolumeUnit) void {
        self.counter = 0;
        self.volume = self.initial_volume;
    }
};

const PeriodUnit = struct {
    period: u11,

    counter: u11,

    inline fn init() PeriodUnit {
        return PeriodUnit{
            .period = undefined,
            .counter = 0,
        };
    }

    fn tick(self: *PeriodUnit) bool {
        self.counter +%= 1;
        if (self.counter == 0) {
            self.reset();
            return true;
        }
        return false;
    }

    fn reset(self: *PeriodUnit) void {
        self.counter = self.period;
    }
};

const ControlUnit = struct {
    active: bool,
    dac_enabled: bool,

    inline fn init() ControlUnit {
        return ControlUnit{
            .active = false,
            .dac_enabled = false,
        };
    }

    fn isActive(self: *const ControlUnit) bool {
        return self.active and self.dac_enabled;
    }

    fn poweroff(self: *ControlUnit) void {
        self.active = false;
        self.dac_enabled = false;
    }
};

const LengthUnit = struct {
    enable: u1,
    initial_counter: u6,

    counter: u6, // TODO: u8 for channel3!

    inline fn init() LengthUnit {
        return LengthUnit{
            .enable = undefined,
            .initial_counter = undefined,
            .counter = 0,
        };
    }

    fn tick(self: *LengthUnit, control: *ControlUnit) void {
        if (self.enable == 0) {
            return;
        }
        self.counter +%= 1;
        if (self.counter == 0) {
            control.active = false;
        }
    }

    fn reset(self: *LengthUnit) void {
        self.counter = self.initial_counter;
    }
};

const SquareWaveUnit = struct {
    duty: u2,

    counter: u3,

    inline fn init() SquareWaveUnit {
        return SquareWaveUnit{
            .duty = undefined,
            .counter = 0,
        };
    }

    fn sample(self: *SquareWaveUnit) u4 {
        const waveforms: [4][8]u4 = .{
            .{ 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0x0 },
            .{ 0x0, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0x0 },
            .{ 0x0, 0xF, 0xF, 0xF, 0xF, 0x0, 0x0, 0x0 },
            .{ 0xF, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xF },
        };
        defer self.counter +%= 1;
        return waveforms[self.duty][self.counter];
    }
};

pub fn Channel(number: comptime_int) type {
    return struct {
        const This = @This();

        control: ControlUnit,
        period_sweep: if (number == 1) SweepPeriodUnit else void,
        period: PeriodUnit,
        volume: VolumeUnit,
        length: LengthUnit,
        square: SquareWaveUnit,

        current: f32,
        divtick_counter: u3,
        powered_off: bool,

        pub inline fn init() This {
            return This{
                .control = ControlUnit.init(),
                .period_sweep = if (number == 1) SweepPeriodUnit.init() else {},
                .period = PeriodUnit.init(),
                .volume = VolumeUnit.init(),
                .length = LengthUnit.init(),
                .square = SquareWaveUnit.init(),
                .current = 0.0,
                .divtick_counter = 0,
                .powered_off = false,
            };
        }

        pub inline fn isActive(self: *const This) bool {
            return self.control.isActive();
        }

        pub fn poweroff(self: *This) void {
            self.powered_off = true;
            self.control.poweroff();

            self.poke(0, 0);
            // DMG ONLY -- Length timers are unaffected by power off only on monochrome models
            //self.poke(1, 0);
            self.poke(2, 0);
            self.poke(3, 0);
            self.poke(4, 0);
        }

        pub fn tick(self: *This) f32 {
            if (!self.control.dac_enabled) {
                return 0.0;
            }

            if (!self.control.active) {
                return dac(0);
            }

            if (self.period.tick()) {
                var s: u8 = self.sample();
                s *= self.volume.volume;
                s /= 15;
                self.current = dac(@intCast(s));
            }

            return self.current;
        }

        fn sample(self: *This) u4 {
            return self.square.sample();
        }

        pub fn divtick(self: *This) void {
            self.divtick_counter +%= 1;
            if (self.divtick_counter % 2 == 0) {
                self.length.tick(&self.control);
            }
            if (self.divtick_counter % 4 == 0) {
                if (number == 1) self.period_sweep.tick(&self.control, &self.period);
            }
            if (self.divtick_counter == 0) {
                self.volume.tick();
            }
        }

        fn trigger(self: *This) void {
            self.control.active = true;
            if (self.length.counter == 0) {
                self.length.reset();
            }
            self.period.reset();
            self.volume.reset();
            if (number == 1) self.period_sweep.reset(&self.period, &self.control);
        }

        fn peek(self: *This, addr: u16) u8 {
            switch (addr) {
                0 => {
                    if (number == 1) {
                        var ret: u8 = 0;
                        ret |= 1;
                        ret <<= 3;
                        ret |= self.period_sweep.pace;
                        ret <<= 1;
                        ret |= self.period_sweep.direction;
                        ret <<= 3;
                        ret |= self.period_sweep.individual_step;
                        return ret;
                    }
                    return 0xFF;
                },
                1 => {
                    var ret: u8 = 0;
                    ret |= self.square.duty;
                    ret <<= 6;
                    ret |= self.length.initial_counter;
                    return ret;
                },
                2 => {
                    var ret: u8 = 0;
                    ret |= self.volume.initial_volume;
                    ret <<= 1;
                    ret |= self.volume.direction;
                    ret <<= 3;
                    ret |= self.volume.pace;
                    return ret;
                },
                3 => return @intCast(self.period.period & 0xFF),
                4 => {
                    var ret: u8 = 0;
                    ret |= 1;
                    ret <<= 1;
                    ret |= self.length.enable;
                    ret <<= 3;
                    ret |= 0b111;
                    ret <<= 3;
                    ret |= @intCast(self.period.period & 0x700);
                    return ret;
                },
                else => unreachable,
            }
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            if (self.powered_off and addr != 1) { // DMG ONLY -- Length timers are unaffected by power off only on monochrome models
                return;
            }

            switch (addr) {
                0 => {
                    if (number == 1) {
                        self.period_sweep.pace = @intCast((val & 0b0111_0000) >> 4);
                        self.period_sweep.direction = @intCast((val & 0b0000_1000) >> 3);
                        self.period_sweep.individual_step = @intCast(val & 0b0000_0111);
                    }
                },
                1 => {
                    self.square.duty = @intCast((val & 0b11_000000) >> 6);
                    self.length.initial_counter = @intCast(val & 0b00_111111);
                },
                2 => {
                    self.volume.initial_volume = @intCast((val & 0xF0) >> 4);
                    self.volume.direction = @intCast((val & 0b0000_1000) >> 3);
                    self.volume.pace = @intCast(val & 0b0000_0111);
                },
                3 => self.period.period = (self.period.period & 0x700) | val,
                4 => {
                    self.length.enable = @intCast((val & 0b0100_0000) >> 6);
                    self.period.period = (self.period.period & 0x0FF) | (@as(u11, @intCast(val & 0b0000_0111)) << 8);
                },
                else => unreachable,
            }
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            if (addr == 0 and number != 1) {
                return .{ .{ .illegal = true }, self.peek(addr) };
            }
            return .{ .{}, self.peek(addr) };
        }

        pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            if (addr == 0 and number != 1) {
                return .{ .illegal = true };
            }
            self.poke(addr, val);
            if (self.volume.direction == 0 and self.volume.initial_volume == 0) {
                self.control.dac_enabled = false;
            } else {
                self.control.dac_enabled = true;
            }
            if (!self.powered_off and addr == 4 and val & 0b1000_0000 != 0) {
                self.trigger();
            }
            return .{};
        }
    };
}

const std = @import("std");

test "dac() maps 4-bit samples to [-1,1]" {
    const eps: f32 = 1e-6;

    try std.testing.expectApproxEqAbs(@as(f32, 1.0), dac(@as(u4, 0b0000)), eps);
    try std.testing.expectApproxEqAbs(@as(f32, 1.0) / 15.0, dac(@as(u4, 0b0111)), eps);
    try std.testing.expectApproxEqAbs(-(@as(f32, 1.0) / 15.0), dac(@as(u4, 0b1000)), eps);
    try std.testing.expectApproxEqAbs(-@as(f32, 1.0), dac(@as(u4, 0b1111)), eps);
}

test "init starts inert and inactive" {
    var ch = Channel(2).init();

    try std.testing.expect(!ch.isActive());
    try std.testing.expect(!ch.powered_off);
    try std.testing.expect(!ch.control.dac_enabled);
    try std.testing.expectEqual(@as(u11, 0), ch.period.counter);
    try std.testing.expectEqual(@as(u3, 0), ch.square.duty);
    try std.testing.expectEqual(@as(f32, 0.0), ch.current);
    try std.testing.expectEqual(@as(u4, 0), ch.volume.volume);
}

test "write(2) enables/disables DAC according to initial_volume/direction" {
    var ch = Channel(2).init();

    // volume=0, direction=0 -> DAC disabled
    _ = ch.write(2, 0x00);
    try std.testing.expect(!ch.control.dac_enabled);

    // volume>0 -> DAC enabled
    _ = ch.write(2, 0x10); // initial_volume=1
    try std.testing.expect(ch.control.dac_enabled);

    // volume=0 but direction=1 -> DAC enabled
    _ = ch.write(2, 0x08); // direction=1
    try std.testing.expect(ch.control.dac_enabled);

    // volume=0 and direction=0 -> DAC disabled again
    _ = ch.write(2, 0x00);
    try std.testing.expect(!ch.control.dac_enabled);
}

test "poke/peek round-trips for NR21 (addr 1) and NR23 (addr 3)" {
    var ch = Channel(2).init();

    // NR21: duty in bits 7..6, length in bits 5..0
    const wd: u2 = 0b10;
    const len: u6 = 0b10_0000;
    _ = ch.write(1, (@as(u8, wd) << 6) | len);
    try std.testing.expectEqual((@as(u8, wd) << 6) | len, ch.peek(1));

    // NR23: low 8 bits of period
    _ = ch.write(3, 0x34);
    try std.testing.expectEqual(@as(u8, 0x34), ch.peek(3));
}

test "trigger sets active, length, volume envelope and period counters" {
    var ch = Channel(2).init();

    // Set NR21 (duty + length)
    const duty: u2 = 0b01;
    const init_len: u6 = 10;
    _ = ch.write(1, (@as(u8, duty) << 6) | init_len);

    // Set NR22 (volume envelope)
    const init_vol: u4 = 0x0F;
    _ = ch.write(2, (@as(u8, init_vol) << 4) | 0b000); // direction=0, pace=0

    // Set period to 0x7FF so the very first tick wraps and samples immediately
    _ = ch.write(3, 0xFF);
    _ = ch.write(4, 0x87); // trigger=1, lengthEnable=0, upper period=0b111

    try std.testing.expect(ch.control.active);
    try std.testing.expectEqual(init_len, ch.length.counter);
    try std.testing.expectEqual(@as(u11, 0x7FF), ch.period.period);
    try std.testing.expectEqual(@as(u11, 0x7FF), ch.period.counter);
    try std.testing.expectEqual(init_vol, ch.volume.volume);
    try std.testing.expectEqual(@as(u3, 0), ch.volume.pace);
}

test "tick() obeys dacEnabled and active; waveform progression matches table" {
    const eps: f32 = 1e-6;
    var ch = Channel(2).init();

    // 1) DAC disabled -> 0.0 output
    _ = ch.write(2, 0x00);
    try std.testing.expectApproxEqAbs(@as(f32, 0.0), ch.tick(), eps);

    // 2) DAC enabled but not active -> dac(0) baseline (1.0)
    _ = ch.write(2, 0x10); // volume=1 enables DAC
    try std.testing.expectApproxEqAbs(@as(f32, 1.0), ch.tick(), eps);

    // 3) Predictable run: duty=0, volume=15, period=0x7FF, trigger
    _ = ch.write(1, (0b00 << 6) | 0); // duty=0, length=0
    _ = ch.write(2, (0x0F << 4) | 0b000); // vol=15, direction=0, pace=0
    _ = ch.write(3, 0xFF);
    _ = ch.write(4, 0x87); // trigger, upper period=0b111

    // With duty=0, the first 7 samples are 0xF (high), then 0x0.
    try std.testing.expectApproxEqAbs(-1.0, ch.tick(), eps);

    // Next 6 ticks: still 'high' (same value)
    inline for (0..6) |_| {
        try std.testing.expectApproxEqAbs(-1.0, ch.tick(), eps);
    }

    // 8th tick of the duty cycle returns 0x0 -> dac(0) = 1.0
    try std.testing.expectApproxEqAbs(@as(f32, 1.0), ch.tick(), eps);
}

test "length timer disables channel when enabled and wraps" {
    var ch = Channel(2).init();

    // Prepare: length=63 so one length tick wraps to 0 and disables.
    _ = ch.write(1, (0b01 << 6) | 63); // any duty, length=63
    _ = ch.write(2, 0x10); // enable DAC
    _ = ch.write(4, 0xC0); // trigger + lengthEnable=1 (upper period=0)

    try std.testing.expect(ch.control.active);

    // divtick: length ticks on every even divtick value after increment.
    ch.divtick(); // 1 -> no length tick
    ch.divtick(); // 2 -> length counter increments 63 -> 0, active=false

    try std.testing.expect(!ch.control.active);
}

test "volume envelope pace increases/decreases volume as configured" {
    var ch = Channel(2).init();

    // initial_volume=4, direction=1 (increase), pace=2
    _ = ch.write(2, (@as(u8, 4) << 4) | (1 << 3) | 2);
    _ = ch.write(4, 0x80); // trigger to load envelope and reset pace counter

    try std.testing.expectEqual(@as(u4, 4), ch.volume.volume);

    // VolumeUnit.tick runs when divtick wraps to 0 (every 8 divticks).
    // With pace=2, need two such moments -> 16 divticks to bump by +1.
    inline for (0..16) |_| ch.divtick();

    try std.testing.expectEqual(@as(u4, 5), ch.volume.volume);
}

test "poweroff blocks writes (except NR21 on DMG) and prevents trigger" {
    var ch = Channel(2).init();

    ch.poweroff();

    // Attempt to trigger while powered off -> must not activate
    _ = ch.write(4, 0x80);
    try std.testing.expect(!ch.control.active);

    // Write to NR23 while powered off should be ignored
    _ = ch.write(3, 0xAA);
    try std.testing.expectEqual(@as(u11, 0), ch.period.period & 0xFF);

    // NR21 is still writable on power-off (DMG quirk in this implementation)
    _ = ch.write(1, (0b11 << 6) | 17);
    try std.testing.expectEqual(@as(u8, (0b11 << 6) | 17), ch.peek(1));
}

test "isActive only true when both active and DAC enabled" {
    var ch = Channel(2).init();

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

test "NR10 (addr 0) poke/peek round-trip for Channel(1)" {
    var ch = Channel(1).init();

    // NR10 format: 1 | pace(3) | direction(1) | step(3)
    const pace: u3 = 0b101;
    const dir: u1 = 0b1; // subtract
    const step: u3 = 0b011;

    _ = ch.write(0, (@as(u8, pace) << 4) | (@as(u8, dir) << 3) | step);

    // Bit 7 is always reported as '1' by peek(0)
    const expected: u8 = 0b1000_0000 | (@as(u8, pace) << 4) | (@as(u8, dir) << 3) | step;
    try std.testing.expectEqual(expected, ch.peek(0));
}

test "Channel(1) trigger with sweep step add checks overflow immediately and can disable on overflow" {
    var ch = Channel(1).init();

    // Enable DAC so channel can become active
    _ = ch.write(2, 0x10); // initial_volume=1

    // Set period to maximum 0x7FF so that an immediate + (>>1) overflows
    _ = ch.write(3, 0xFF);
    _ = ch.write(4, 0x07); // upper 3 bits of period = 0b111 (no trigger yet)

    // NR10: pace=1 (non-zero), direction=0 (add), step=1 -> overflow expected on immediate calc at trigger
    _ = ch.write(0, (1 << 4) | (0 << 3) | 1);

    // Trigger
    _ = ch.write(4, 0x80 | 0x07); // trigger=1, lengthEnable=0, keep upper period

    // Immediate overflow check should have disabled the channel and sweep
    try std.testing.expect(!ch.control.active);
    try std.testing.expect(!ch.period_sweep.enabled);
}

test "Channel(1) period sweep subtract updates period on schedule (pace=2 -> 1 update after 8 divticks)" {
    var ch = Channel(1).init();

    // Enable DAC and set a known period
    _ = ch.write(2, 0x10); // initial_volume=1 enables DAC
    // Set period to 0x400
    _ = ch.write(3, 0x00);
    _ = ch.write(4, 0x84); // trigger=0, lengthEnable=0, upper bits=0b100 -> 0x400

    // NR10: pace=2, direction=1 (subtract), step=1
    _ = ch.write(0, (2 << 4) | (1 << 3) | 1);

    // Trigger loads shadow and arms sweep
    _ = ch.write(4, 0x80 | 0x04);

    try std.testing.expect(ch.control.active);
    try std.testing.expect(ch.period_sweep.enabled);
    try std.testing.expectEqual(@as(u11, 0x400), ch.period.period);

    // Sweep ticks every 4 divticks; with pace=2 we need 2 sweep ticks -> 8 divticks total
    inline for (0..8) |_| ch.divtick();

    // New period = P - (P >> 1) = 0x400 - 0x200 = 0x200
    try std.testing.expectEqual(@as(u11, 0x200), ch.period.period);
    try std.testing.expect(ch.control.active); // still active, no overflow
}

test "Channel(1) paced increment may overflow on a subsequent step (pace=1)" {
    var ch = Channel(1).init();

    // Enable DAC and set period to a value that won't overflow on the first calc,
    // but will overflow on the second calc.
    _ = ch.write(2, 0x10); // initial_volume=1
    // period = 0x500
    _ = ch.write(3, 0x00);
    _ = ch.write(4, 0x85); // upper bits=0b101 -> 0x500 (no trigger)

    // NR10: pace=1, direction=0 (add), step=1
    _ = ch.write(0, (1 << 4) | (0 << 3) | 1);

    // Trigger
    _ = ch.write(4, 0x80 | 0x05);

    try std.testing.expect(ch.control.active);
    try std.testing.expect(ch.period_sweep.enabled);
    try std.testing.expectEqual(@as(u11, 0x500), ch.period.period);

    // One sweep step after 4 divticks (pace=1)
    inline for (0..4) |_| ch.divtick();
    // First update: 0x500 + (0x500 >> 1) = 0x500 + 0x280 = 0x780 (no overflow)
    try std.testing.expectEqual(@as(u11, 0x780), ch.period.period);
    // However, the overflow check takes place early, so the channel is already disabled here
    try std.testing.expect(!ch.control.active);
}
