const memory = @import("memory.zig");
const Memory = memory.Memory;
const MemoryFlag = memory.MemoryFlag;

pub const Joypad = packed struct {
    select_buttons: u1,
    select_dpad: u1,
    buttons: u4,
    dpad: u4,

    fn peek(selfptr: *anyopaque, _: u16) u8 {
        const self: *Joypad = @alignCast(@ptrCast(selfptr));

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

    fn poke(selfptr: *anyopaque, _: u16, val: u8) void {
        const self: *Joypad = @alignCast(@ptrCast(selfptr));

        _ = write(self, undefined, val);

        // We allow poke to set the joypad button state
        // TODO: This is forever now, but when this is actually using proper input, it should probably be until after the next read (not peek)
        if (self.select_buttons == 0) {
            self.buttons = @intCast(val & 0x0F);
        }
        if (self.select_dpad == 0) {
            self.dpad = @intCast(val & 0x0F);
        }
    }

    fn read(selfptr: *anyopaque, _: u16) struct { ?MemoryFlag, u8 } {
        return .{ null, peek(selfptr, undefined) };
    }

    fn write(selfptr: *anyopaque, _: u16, val: u8) ?MemoryFlag {
        const self: *Joypad = @alignCast(@ptrCast(selfptr));
        self.select_buttons = @intCast((val & 0b0010_0000) >> 5);
        self.select_dpad = @intCast((val & 0b0001_0000) >> 4);
        return null;
    }

    pub fn memory(self: *Joypad) Memory {
        return Memory{
            .ctx = self,
            .peek_cb = peek,
            .poke_cb = poke,
            .read_cb = read,
            .write_cb = write,
        };
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

    var mem = joypad.memory();
    const result = mem.read(0x00);
    try std.testing.expectEqual(0xFF, result);
}

test "Joypad button selection returns button state" {
    var joypad = Joypad{
        .select_buttons = 0,
        .select_dpad = 1,
        .buttons = 0b0000,
        .dpad = 0b0000,
    };

    var mem = joypad.memory();

    mem.poke(0x00, 0b1101_1010);
    try std.testing.expectEqual(0b1010, joypad.buttons);

    const result = mem.read(0x00);
    try std.testing.expectEqual(0b1101_1010, result);
}

test "Joypad dpad selection returns dpad state" {
    var joypad = Joypad{
        .select_buttons = 1,
        .select_dpad = 0,
        .buttons = 0b0000,
        .dpad = 0b0000,
    };

    var mem = joypad.memory();

    mem.poke(0x00, 0b1110_0101);
    try std.testing.expectEqual(0b0101, joypad.dpad);

    const result = mem.read(0x00);
    try std.testing.expectEqual(0b1110_0101, result);
}

test "Joypad selection bits update correctly" {
    var joypad = Joypad{
        .select_buttons = 1,
        .select_dpad = 1,
        .buttons = 0b0000,
        .dpad = 0b0000,
    };

    var mem = joypad.memory();

    _ = mem.write(0x00, 0b1110_0000);
    try std.testing.expectEqual(1, joypad.select_buttons);
    try std.testing.expectEqual(0, joypad.select_dpad);

    _ = mem.write(0x00, 0b1101_0000);
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

    var mem = joypad.memory();

    mem.poke(0x00, 0b1111_0011);
    try std.testing.expectEqual(0b0000, joypad.buttons);
    try std.testing.expectEqual(0b1111, joypad.dpad);
}
