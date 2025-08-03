pub const NullAudioBackend = struct {
    pub const SamplingRate = 44_100.0;
    pub inline fn init() NullAudioBackend {
        return NullAudioBackend{};
    }
    pub fn submit(_: *NullAudioBackend, _: f32, _: f32) void {}
};
