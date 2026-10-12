// FF00
pub const JOYP = packed struct(u8) {
    a_or_right_active_low: bool,
    b_or_left_active_low: bool,
    select_or_up_active_low: bool,
    start_or_down_active_low: bool,
    select_d_pad_active_low: bool,
    select_buttons_active_low: bool,
    _: u2 = 0b11,
};

// FF01
// SB is not structured

// FF02
pub const SC = packed struct(u8) {
    pub const ClockSelect = enum(u1) {
        slave = 0,
        master = 1,
    };

    clock_select: ClockSelect,
    _: u6 = 0b111111, // DMG ONLY -- Clock speed added by CGB
    transfer_enable: bool,
};

// FF03
// Unused

// FF04
// DIV is not structured

// FF05
// TIMA is not structured

// FF06
// TMA is not structured

// FF07
pub const TAC = packed struct(u8) {
    // DMG ONLY -- Different meanings in CGB double-speed mode
    pub const ClockSelect = enum(u2) {
        freq_4096 = 0b00,
        freq_262144 = 0b01,
        freq_65536 = 0b10,
        freq_16384 = 0b11,
    };

    clock_select: ClockSelect,
    enable: bool,
    _: u5 = 0b11111,
};

// FF08-FF0E
// Unused

// FF0F
pub const IF = packed struct(u8) {
    vblank: bool,
    lcd: bool,
    timer: bool,
    serial: bool,
    joypad: bool,
    _: u3 = 0b111,
};

// FF10
pub const NR10 = packed struct(u8) {
    pub const Direction = enum(u1) {
        addition = 0,
        subtraction = 1,
    };

    individual_step: u3,
    direction: Direction,
    pace: u3,
    _: u1 = 0b1,
};

// FF11
pub const NR11 = packed struct(u8) {
    pub const WaveDuty = enum(u2) {
        duty_12_5 = 0b00,
        duty_25 = 0b01,
        duty_50 = 0b10,
        duty_75 = 0b11,
    };

    initial_length_timer: u6,
    wave_duty: WaveDuty,
};

// FF12
pub const NR12 = packed struct(u8) {
    pub const Direction = enum(u1) {
        decrease = 0,
        increase = 1,
    };

    sweep_pace: u3,
    env_dir: Direction,
    initial_volume: u4,
};

// FF13-FF14
pub const NR13_14 = packed struct(u16) {
    period: u11,
    _: u3 = 0b111,
    length_enable: bool,
    trigger: bool,

    pub fn lo(self: NR13_14) u8 {
        return @truncate(@as(u16, @bitCast(self)));
    }

    pub fn setLo(self: *NR13_14, val: u8) void {
        self.* = @bitCast((@as(u16, @bitCast(self.*)) & 0xFF00) | @as(u16, @intCast(val)));
    }

    pub fn hi(self: NR13_14) u8 {
        return @truncate(@as(u16, @bitCast(self)) >> 8);
    }

    pub fn setHi(self: *NR13_14, val: u8) void {
        self.* = @bitCast((@as(u16, @bitCast(self.*)) & 0x00FF) | (@as(u16, @intCast(val)) << 8));
    }
};

test "NR13_14 byte accessors" {
    const std = @import("std");
    const expectEqual = std.testing.expectEqual;

    var reg: NR13_14 = @bitCast(@as(u16, 0));

    // Writing either byte must preserve the other.
    reg.setLo(0xAB);
    try expectEqual(@as(u8, 0xAB), reg.lo());
    try expectEqual(@as(u8, 0x00), reg.hi());

    reg.setHi(0xC5);
    try expectEqual(@as(u8, 0xAB), reg.lo());
    try expectEqual(@as(u8, 0xC5), reg.hi());

    // Check the logical fields.
    try expectEqual(@as(u11, 0x5AB), reg.period);
    try expectEqual(true, reg.length_enable);
    try expectEqual(true, reg.trigger);

    // Overwriting the low byte must preserve the high byte.
    reg.setLo(0x12);
    try expectEqual(@as(u11, 0x512), reg.period);
    try expectEqual(@as(u8, 0xC5), reg.hi());

    // Overwriting the high byte must preserve the low byte.
    reg.setHi(0x02);
    try expectEqual(@as(u11, 0x212), reg.period);
    try expectEqual(false, reg.length_enable);
    try expectEqual(false, reg.trigger);
    try expectEqual(@as(u8, 0x12), reg.lo());
}

// FF15
// Unused

// FF16
pub const NR21 = NR11;

// FF17
pub const NR22 = NR12;

// FF18-19
pub const NR23_24 = NR13_14;

// FF1A
pub const NR30 = packed struct(u8) {
    _: u7 = 0b1111111,
    dac_on: bool,
};

// FF1B
// NR31 is not structured

// FF1C
pub const NR32 = packed struct(u8) {
    pub const OutputLevel = enum(u2) {
        mute = 0b00,
        volume_100 = 0b01,
        volume_50 = 0b10,
        volume_25 = 0b11,
    };

    _: u5 = 0b11111,
    output_level: OutputLevel,
    __: u1 = 0b1,
};

// FF1D-FF1E
pub const NR33_34 = NR13_14;

// FF1F
// Unused

// FF20
pub const NR41 = packed struct(u8) {
    initial_length_timer: u6,
    _: u2 = 0b11,
};

// FF21
pub const NR42 = NR12;

// FF22
pub const NR43 = packed struct(u8) {
    pub const LFSRWidth = enum(u1) {
        w15_bit = 0,
        w7_bit = 1,
    };

    clock_divider: u3,
    lfsr_width: LFSRWidth,
    clock_shift: u4,
};

// FF23
pub const NR44 = packed struct(u8) {
    _: u6 = 0b111111,
    length_enable: bool,
    trigger: bool,
};

// FF24
pub const NR50 = packed struct(u8) {
    right_volume: u3,
    vin_right: bool,
    left_volume: u3,
    vin_left: bool,
};

// FF25
pub const NR51 = packed struct(u8) {
    ch1_right: bool,
    ch2_right: bool,
    ch3_right: bool,
    ch4_right: bool,
    ch1_left: bool,
    ch2_left: bool,
    ch3_left: bool,
    ch4_left: bool,
};

// FF26
pub const NR52 = packed struct(u8) {
    ch1_on: bool,
    ch2_on: bool,
    ch3_on: bool,
    ch4_on: bool,
    _: u3 = 0b111,
    audio_on: bool,
};

// FF30-FF3F
// Wave RAM is not structured
