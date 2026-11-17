const This = @This();

pub inline fn init() This {
    return This{};
}
pub fn submit(_: *This, _: u2) void {}
pub fn poweroff(_: *This) void {}
