const This = @This();

pub const SamplingRate = 44_100.0;

pub inline fn init() This {
    return This{};
}

pub fn submit(_: *This, _: f32, _: f32) void {}
