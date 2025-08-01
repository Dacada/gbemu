pub const NullAudioBackend = struct {
    pub const Resampler = struct {
        pub inline fn init() Resampler {
            return Resampler{};
        }

        pub fn resample(_: *Resampler, _: u4, _: u16) ?u8 {
            return null;
        }
    };

    pub inline fn init() NullAudioBackend {
        return NullAudioBackend{};
    }

    pub fn submit(_: *NullAudioBackend, _: u8) void {}
};
