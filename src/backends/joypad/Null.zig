const This = @This();

const JoypadButtons = @import("../../core/joypad.zig").JoypadButtons;

pub inline fn init() This {
    return .{};
}

pub inline fn poll(_: *This) JoypadButtons {
    return .{
        .a = false,
        .b = false,
        .down = false,
        .left = false,
        .right = false,
        .select = false,
        .start = false,
        .up = false,
    };
}
