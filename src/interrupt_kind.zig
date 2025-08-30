pub const InterruptKind = enum(u3) {
    vblank = 0,
    lcd = 1,
    timer = 2,
    serial = 3,
    joypad = 4,

    pub inline fn asMask(self: InterruptKind) u8 {
        return @as(u8, 1) << self.asOffset();
    }

    pub inline fn asOffset(self: InterruptKind) u3 {
        return @intFromEnum(self);
    }
};
