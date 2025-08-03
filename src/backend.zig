pub const NullAudioBackend = struct {
    pub const SamplingFrequency = 44100;
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
    pub fn submit(_: *NullAudioBackend, _: f32, _: f32) void {}
};
