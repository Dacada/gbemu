const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;

/// true -> button is pressed
pub const JoypadButtons = packed struct {
    start: bool,
    select: bool,
    b: bool,
    a: bool,
    down: bool,
    up: bool,
    left: bool,
    right: bool,
};

pub fn Joypad(Interrupt: type) type {
    return struct {
        const This = @This();

        select_buttons: u1,
        select_dpad: u1,
        buttons: u4,
        dpad: u4,
        intr: *Interrupt,

        pub inline fn init(intr: *Interrupt) This {
            return This{
                .select_buttons = undefined,
                .select_dpad = undefined,
                .buttons = undefined,
                .dpad = undefined,
                .intr = intr,
            };
        }

        pub inline fn getActive(self: *const This) JoypadButtons {
            const allbuttons: u8 = (@as(u8, @intCast(~self.buttons)) << 4) | ~self.dpad;
            return @bitCast(allbuttons);
        }

        /// Triggers interrupts!
        pub inline fn setActive(self: *This, allbuttons: JoypadButtons) void {
            // TODO: emulate bouncing?
            const old_regval = self.peek(undefined) & 0xF;

            const allbuttonsint: u8 = @bitCast(allbuttons);
            self.buttons = (~allbuttonsint >> 4) & 0x0F;
            self.dpad = ~allbuttonsint & 0x0F;

            const new_regval = self.peek(undefined) & 0xF;

            // if any bit was 1 but is now 0
            if (old_regval & ~new_regval != 0) {
                self.intr.request(InterruptKind.joypad);
            }
        }

        pub fn peek(self: *This, _: u16) u8 {
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

        pub fn poke(self: *This, _: u16, val: u8) void {
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

        pub fn read(self: *This, _: u16) struct { MemoryFlag, u8 } {
            return .{ .{}, self.peek(undefined) };
        }

        pub fn write(self: *This, _: u16, val: u8) MemoryFlag {
            self.select_buttons = @intCast((val & 0b0010_0000) >> 5);
            self.select_dpad = @intCast((val & 0b0001_0000) >> 4);
            return .{};
        }
    };
}

const std = @import("std");

const MockInterrupt = struct {
    requested: bool = false,

    fn request(self: *MockInterrupt, _: InterruptKind) void {
        self.requested = true;
    }
};

const MockedJoypad = Joypad(MockInterrupt);

test "Joypad initial state reads 0xFF" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);
    joypad.select_buttons = 1;
    joypad.select_dpad = 1;
    joypad.buttons = 0b0000;
    joypad.dpad = 0b0000;

    _, const result = joypad.read(0x00);
    try std.testing.expectEqual(0xFF, result);
}

test "Joypad button selection returns button state" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);
    joypad.select_buttons = 0;
    joypad.select_dpad = 1;
    joypad.buttons = 0b0000;

    joypad.poke(0x00, 0b1101_1010);
    try std.testing.expectEqual(0b1010, joypad.buttons);

    _, const result = joypad.read(0x00);
    try std.testing.expectEqual(0b1101_1010, result);
}

test "Joypad dpad selection returns dpad state" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);
    joypad.select_buttons = 1;
    joypad.select_dpad = 0;
    joypad.buttons = 0b0000;
    joypad.dpad = 0b0000;

    joypad.poke(0x00, 0b1110_0101);
    try std.testing.expectEqual(0b0101, joypad.dpad);

    _, const result = joypad.read(0x00);
    try std.testing.expectEqual(0b1110_0101, result);
}

test "Joypad selection bits update correctly" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);
    joypad.select_buttons = 1;
    joypad.select_dpad = 1;
    joypad.buttons = 0b0000;
    joypad.dpad = 0b0000;

    _ = joypad.write(0x00, 0b1110_0000);
    try std.testing.expectEqual(1, joypad.select_buttons);
    try std.testing.expectEqual(0, joypad.select_dpad);

    _ = joypad.write(0x00, 0b1101_0000);
    try std.testing.expectEqual(0, joypad.select_buttons);
    try std.testing.expectEqual(1, joypad.select_dpad);
}

test "Joypad preserves unselected group state" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);
    joypad.select_buttons = 0;
    joypad.select_dpad = 0;
    joypad.buttons = 0b0000;
    joypad.dpad = 0b1111;

    joypad.poke(0x00, 0b1111_0011);
    try std.testing.expectEqual(0b0000, joypad.buttons);
    try std.testing.expectEqual(0b1111, joypad.dpad);
}

test "getActive and setActive round-trip" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);

    const original = JoypadButtons{
        .start = true,
        .select = false,
        .b = true,
        .a = false,
        .down = true,
        .up = false,
        .left = true,
        .right = false,
    };

    joypad.setActive(original);
    const result = joypad.getActive();

    try std.testing.expectEqual(original, result);
}

test "setActive sets internal button states correctly" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);

    const state = JoypadButtons{
        .start = true,
        .select = true,
        .b = false,
        .a = false,
        .down = false,
        .up = true,
        .left = true,
        .right = false,
    };

    joypad.setActive(state);

    const allbuttons: u8 = @bitCast(state);
    const expected_buttons = (~allbuttons >> 4) & 0x0F;
    const expected_dpad = ~allbuttons & 0x0F;

    try std.testing.expectEqual(expected_buttons, joypad.buttons);
    try std.testing.expectEqual(expected_dpad, joypad.dpad);
}

test "setActive triggers interrupt on new press" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);

    // Initial state: all released (1s)
    joypad.buttons = 0xF;
    joypad.dpad = 0xF;

    // Configure select to check both button and dpad
    joypad.select_buttons = 0;
    joypad.select_dpad = 0;

    // Press "start" and "down" (previously unpressed)
    const pressed = JoypadButtons{
        .start = true,
        .select = false,
        .b = false,
        .a = false,
        .down = true,
        .up = false,
        .left = false,
        .right = false,
    };

    joypad.setActive(pressed);

    try std.testing.expect(intr.requested);
}

test "setActive does not trigger interrupt when unchanged" {
    var intr = MockInterrupt{};
    var joypad = MockedJoypad.init(&intr);

    joypad.select_buttons = 0;
    joypad.select_dpad = 0;

    const state = JoypadButtons{
        .start = false,
        .select = false,
        .b = false,
        .a = false,
        .down = false,
        .up = false,
        .left = false,
        .right = false,
    };

    joypad.setActive(state);
    intr.requested = false; // reset flag
    joypad.setActive(state); // same state again

    try std.testing.expect(!intr.requested);
}
