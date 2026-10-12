const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;
const JOYP = @import("mmio_register.zig").JOYP;

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

pub fn Joypad(Interrupt: type, JoypadBackend: type) type {
    return struct {
        const This = @This();

        select_buttons: bool,
        select_dpad: bool,
        joypad: JoypadButtons,

        intr: *Interrupt,
        backend: *JoypadBackend,

        pub inline fn init(intr: *Interrupt, backend: *JoypadBackend) This {
            return This{
                .select_buttons = undefined,
                .select_dpad = undefined,
                .joypad = undefined,

                .intr = intr,
                .backend = backend,
            };
        }

        fn check_trigger_interrupt(self: *This, old_regval: u8) void {
            const new_regval = self.peek(undefined) & 0xF;

            // if any bit was 1 but is now 0
            if (old_regval & ~new_regval != 0) {
                self.intr.request(InterruptKind.joypad);
            }
        }

        /// Reads button states from backend and triggers interrupts
        pub inline fn poll(self: *This) void {
            const old_regval = self.peek(undefined) & 0xF;
            self.joypad = self.backend.poll();
            self.check_trigger_interrupt(old_regval);
        }

        pub fn peek(self: *This, _: u16) u8 {
            var result: JOYP = @bitCast(@as(u8, 0xFF));

            if (self.select_buttons) {
                result.a_or_right_active_low &= !self.joypad.a;
                result.b_or_left_active_low &= !self.joypad.b;
                result.select_or_up_active_low &= !self.joypad.select;
                result.start_or_down_active_low &= !self.joypad.start;
            }

            if (self.select_dpad) {
                result.a_or_right_active_low &= !self.joypad.right;
                result.b_or_left_active_low &= !self.joypad.left;
                result.select_or_up_active_low &= !self.joypad.up;
                result.start_or_down_active_low &= !self.joypad.down;
            }

            result.select_buttons_active_low = !self.select_buttons;
            result.select_d_pad_active_low = !self.select_dpad;

            return @bitCast(result);
        }

        /// For debugging, as these values aren't usually writeable. No interrupts.
        pub fn poke(self: *This, _: u16, val: u8) void {
            const reg: JOYP = @bitCast(val);

            self.select_buttons = !reg.select_buttons_active_low;
            self.select_dpad = !reg.select_d_pad_active_low;

            if (self.select_buttons) {
                self.joypad.a = !reg.a_or_right_active_low;
                self.joypad.b = !reg.b_or_left_active_low;
                self.joypad.select = !reg.select_or_up_active_low;
                self.joypad.start = !reg.start_or_down_active_low;
            }

            if (self.select_dpad) {
                self.joypad.right = !reg.a_or_right_active_low;
                self.joypad.left = !reg.b_or_left_active_low;
                self.joypad.up = !reg.select_or_up_active_low;
                self.joypad.down = !reg.start_or_down_active_low;
            }
        }

        pub fn read(self: *This, _: u16) struct { MemoryFlag, u8 } {
            return .{ .{}, self.peek(undefined) };
        }

        /// May trigger an interrupt
        pub fn write(self: *This, _: u16, val: u8) MemoryFlag {
            const reg: JOYP = @bitCast(val);

            const old_regval = self.peek(undefined) & 0xF;
            self.select_buttons = !reg.select_buttons_active_low;
            self.select_dpad = !reg.select_d_pad_active_low;
            self.check_trigger_interrupt(old_regval);

            return .{};
        }
    };
}

const std = @import("std");

const TestContainer = @import("dependency_container.zig").Container(.{
    .interrupt = .mock,
    .joypad_backend = .mock_nil,
});

fn init_joypad(joypad: *TestContainer.Joypad) void {
    joypad.select_buttons = false;
    joypad.select_dpad = false;
    joypad.joypad = .{
        .a = false,
        .b = false,
        .select = false,
        .start = false,
        .down = false,
        .left = false,
        .right = false,
        .up = false,
    };
}

test "JOYP reads selected buttons with active-low semantics" {
    var container = TestContainer.init();
    const backend = container.get_joypad_backend();
    const joypad = container.get_joypad();

    init_joypad(joypad);

    backend.set(.{
        .a = true,
        .b = false,
        .select = false,
        .start = true,
        .down = false,
        .left = true,
        .right = false,
        .up = true,
    });
    joypad.poll();

    // No group selected.
    try std.testing.expectEqual(@as(u8, 0xFF), joypad.peek(0));

    // Action buttons: A and Start pressed.
    _ = joypad.write(0, 0xDF);
    try std.testing.expectEqual(@as(u8, 0xD6), joypad.peek(0));

    // D-pad: Left and Up pressed.
    _ = joypad.write(0, 0xEF);
    try std.testing.expectEqual(@as(u8, 0xE9), joypad.peek(0));

    // Both groups: active-low bits combine.
    _ = joypad.write(0, 0xCF);
    try std.testing.expectEqual(@as(u8, 0xC0), joypad.peek(0));
}

test "JOYP interrupts only on falling edges" {
    var container = TestContainer.init();
    const backend = container.get_joypad_backend();
    const interrupt = container.get_interrupt();
    const joypad = container.get_joypad();

    init_joypad(joypad);
    _ = joypad.write(0, 0xDF); // Select action buttons.

    var buttons = joypad.joypad;

    // Press A.
    buttons.a = true;
    backend.set(buttons);
    joypad.poll();
    try std.testing.expectEqual(
        InterruptKind.joypad,
        interrupt.requested.?,
    );

    // Same state: no new interrupt.
    interrupt.requested = null;
    joypad.poll();
    try std.testing.expect(interrupt.requested == null);

    // Release A: rising edge, no interrupt.
    buttons.a = false;
    backend.set(buttons);
    joypad.poll();
    try std.testing.expect(interrupt.requested == null);

    // Press B: another falling edge.
    buttons.b = true;
    backend.set(buttons);
    joypad.poll();
    try std.testing.expectEqual(
        InterruptKind.joypad,
        interrupt.requested.?,
    );
}

test "JOYP selection changes can trigger interrupts" {
    var container = TestContainer.init();
    const backend = container.get_joypad_backend();
    const interrupt = container.get_interrupt();
    const joypad = container.get_joypad();

    init_joypad(joypad);

    var buttons = joypad.joypad;
    buttons.a = true;
    backend.set(buttons);

    // A is pressed, but neither group is selected.
    joypad.poll();
    try std.testing.expect(interrupt.requested == null);

    // Selecting action buttons exposes the pressed A.
    _ = joypad.write(0, 0xDF);
    try std.testing.expectEqual(
        InterruptKind.joypad,
        interrupt.requested.?,
    );

    // Deselecting produces no falling edge.
    interrupt.requested = null;
    _ = joypad.write(0, 0xFF);
    try std.testing.expect(interrupt.requested == null);

    // Selecting again produces another falling edge.
    _ = joypad.write(0, 0xDF);
    try std.testing.expectEqual(
        InterruptKind.joypad,
        interrupt.requested.?,
    );
}
