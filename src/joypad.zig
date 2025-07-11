const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;

pub const Joypad = packed struct {
    select_buttons: u1,
    select_dpad: u1,
    buttons: u4,
    dpad: u4,

    pub inline fn init() Joypad {
        return Joypad{
            .select_buttons = undefined,
            .select_dpad = undefined,
            .buttons = undefined,
            .dpad = undefined,
        };
    }

    pub fn peek(self: *Joypad, _: u16) u8 {
        var lower: u4 = 0xF;
        if (self.select_buttons == 0) {
            lower &= self.buttons;
        }
        if (self.select_dpad == 0) {
            lower &= self.dpad;
        }

        var result: u8 = 0b110;
        result |= self.select_buttons;
        result <<= 1;
        result |= self.select_dpad;
        result <<= 4;
        result |= lower;

        return result;
    }

    pub fn poke(self: *Joypad, _: u16, val: u8) void {
        _ = self.write(undefined, val);

        // We allow poke to set the joypad button state
        // TODO: This is forever now, but when this is actually using proper input, it should probably be until after the next read (not peek)
        if (self.select_buttons == 0) {
            self.buttons = @intCast(val & 0x0F);
        }
        if (self.select_dpad == 0) {
            self.dpad = @intCast(val & 0x0F);
        }
    }

    pub fn read(self: *Joypad, _: u16) struct { MemoryFlag, u8 } {
        return .{ .{}, self.peek(undefined) };
    }

    pub fn write(self: *Joypad, _: u16, val: u8) MemoryFlag {
        self.select_buttons = @intCast((val & 0b0010_0000) >> 5);
        self.select_dpad = @intCast((val & 0b0001_0000) >> 4);
        return .{};
    }
};

const std = @import("std");

test "Joypad initial state reads 0xFF" {
    var joypad = Joypad{
        .select_buttons = 1,
        .select_dpad = 1,
        .buttons = 0b0000,
        .dpad = 0b0000,
    };

    _, const result = joypad.read(0x00);
    try std.testing.expectEqual(0xFF, result);
}

test "Joypad button selection returns button state" {
    var joypad = Joypad{
        .select_buttons = 0,
        .select_dpad = 1,
        .buttons = 0b0000,
        .dpad = 0b0000,
    };

    joypad.poke(0x00, 0b1101_1010);
    try std.testing.expectEqual(0b1010, joypad.buttons);

    _, const result = joypad.read(0x00);
    try std.testing.expectEqual(0b1101_1010, result);
}

test "Joypad dpad selection returns dpad state" {
    var joypad = Joypad{
        .select_buttons = 1,
        .select_dpad = 0,
        .buttons = 0b0000,
        .dpad = 0b0000,
    };

    joypad.poke(0x00, 0b1110_0101);
    try std.testing.expectEqual(0b0101, joypad.dpad);

    _, const result = joypad.read(0x00);
    try std.testing.expectEqual(0b1110_0101, result);
}

test "Joypad selection bits update correctly" {
    var joypad = Joypad{
        .select_buttons = 1,
        .select_dpad = 1,
        .buttons = 0b0000,
        .dpad = 0b0000,
    };

    _ = joypad.write(0x00, 0b1110_0000);
    try std.testing.expectEqual(1, joypad.select_buttons);
    try std.testing.expectEqual(0, joypad.select_dpad);

    _ = joypad.write(0x00, 0b1101_0000);
    try std.testing.expectEqual(0, joypad.select_buttons);
    try std.testing.expectEqual(1, joypad.select_dpad);
}

test "Joypad preserves unselected group state" {
    var joypad = Joypad{
        .select_buttons = 0,
        .select_dpad = 0,
        .buttons = 0b0000,
        .dpad = 0b1111,
    };

    joypad.poke(0x00, 0b1111_0011);
    try std.testing.expectEqual(0b0000, joypad.buttons);
    try std.testing.expectEqual(0b1111, joypad.dpad);
}
