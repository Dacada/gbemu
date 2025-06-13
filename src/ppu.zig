pub const Ppu = struct {
    pub fn write_video(_: Ppu, _: u16, _: u8, _: *bool) void {}
    pub fn read_video(_: Ppu, _: u16, _: *bool, _: *bool) u8 {
        return 0;
    }
    pub fn write_oam(_: Ppu, _: u16, _: u8, _: *bool) void {}
    pub fn read_oam(_: Ppu, _: u16, _: *bool, _: *bool) u8 {
        return 0;
    }
    pub fn setValue_video(_: Ppu, _: u16, _: u8) void {}
    pub fn getValue_video(_: Ppu, _: u16) u8 {
        return 0;
    }
    pub fn setValue_oam(_: Ppu, _: u16, _: u8) void {}
    pub fn getValue_oam(_: Ppu, _: u16) u8 {
        return 0;
    }
    pub fn oam_blocked(_: Ppu) bool {
        return false;
    }
};
