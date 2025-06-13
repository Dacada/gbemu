pub const Mmio = struct {
    pub fn write(_: Mmio, _: u16, _: u8, _: *bool) void {}
    pub fn read(_: Mmio, _: u16, _: *bool, _: *bool) u8 {
        return 0;
    }
    pub fn setValue(_: Mmio, _: u16, _: u8) void {}
    pub fn getValue(_: Mmio, _: u16) u8 {
        return 0;
    }
};
