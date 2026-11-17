const This = @This();

const JoypadButtons = @import("../../core/joypad.zig").JoypadButtons;

state: JoypadButtons,

pub inline fn init() This {
    return This{
        .state = .{
            .a = false,
            .b = false,
            .down = false,
            .left = false,
            .right = false,
            .select = false,
            .start = false,
            .up = false,
        },
    };
}

pub inline fn set(self: *This, state: JoypadButtons) void {
    self.state = state;
}

pub inline fn poll(self: *This) JoypadButtons {
    return self.state;
}
