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
