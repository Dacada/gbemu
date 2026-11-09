const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;
const TrackedValue = @import("tracked_value.zig").TrackedValue;
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

fn TrackedJoypadButtons() type {
    const info = @typeInfo(JoypadButtons);
    const field_names = info.@"struct".field_names;

    var field_types: [field_names.len]type = undefined;
    for (0.., info.@"struct".field_types) |i, T| {
        field_types[i] = TrackedValue(T);
    }
    const field_attrs = info.@"struct".field_attrs;

    return @Struct(.auto, null, field_names, &field_types, field_attrs);
}

pub fn Joypad(Interrupt: type, JoypadBackend: type) type {
    return struct {
        const This = @This();

        select_buttons: TrackedValue(bool),
        select_dpad: TrackedValue(bool),
        joypad: TrackedJoypadButtons(),

        intr: *Interrupt,
        backend: *JoypadBackend,

        pub inline fn init(intr: *Interrupt, backend: *JoypadBackend) This {
            return This{
                .select_buttons = .init(true),
                .select_dpad = .init(true),
                .joypad = .{
                    .start = .init(true),
                    .select = .init(false),
                    .b = .init(true),
                    .a = .init(false),
                    .down = .init(true),
                    .up = .init(false),
                    .left = .init(true),
                    .right = .init(false),
                },

                .intr = intr,
                .backend = backend,
            };
        }

        fn check_trigger_interrupt(self: *This, old_regval: u8) void {
            const new_regval = self.inner_read().@"1" & 0xF;

            // if any bit was 1 but is now 0
            if (old_regval & ~new_regval != 0) {
                self.intr.request(InterruptKind.joypad);
            }
        }

        /// Reads button states from backend and triggers interrupts
        pub inline fn poll(self: *This) void {
            const old_regval = self.inner_read().@"1" & 0xF;

            const new_joypad = self.backend.poll();
            inline for (@typeInfo(JoypadButtons).@"struct".field_names) |name| {
                const value = @field(new_joypad, name);
                @field(self.joypad, name).set(value, true);
            }

            self.check_trigger_interrupt(old_regval);
        }

        fn inner_read(self: *This) struct { MemoryFlag, u8 } {
            var flags: MemoryFlag = .{};
            var result: JOYP = @bitCast(@as(u8, 0xFF));

            const select_buttons = self.select_buttons.resolve(&flags);
            const select_dpad = self.select_dpad.resolve(&flags);

            if (select_buttons) {
                result.a_or_right_active_low &= !self.joypad.a.resolve(&flags);
                result.b_or_left_active_low &= !self.joypad.b.resolve(&flags);
                result.select_or_up_active_low &= !self.joypad.select.resolve(&flags);
                result.start_or_down_active_low &= !self.joypad.start.resolve(&flags);
            }

            if (select_dpad) {
                result.a_or_right_active_low &= !self.joypad.right.resolve(&flags);
                result.b_or_left_active_low &= !self.joypad.left.resolve(&flags);
                result.select_or_up_active_low &= !self.joypad.up.resolve(&flags);
                result.start_or_down_active_low &= !self.joypad.down.resolve(&flags);
            }

            result.select_buttons_active_low = !select_buttons;
            result.select_d_pad_active_low = !select_dpad;

            return .{
                flags,
                @bitCast(result),
            };
        }

        pub fn peek(self: *This, _: u16) u8 {
            return self.inner_read().@"1";
        }

        /// For debugging, as these values aren't usually writeable. No interrupts.
        pub fn poke(self: *This, _: u16, val: u8) void {
            const reg: JOYP = @bitCast(val);

            const select_buttons = !reg.select_buttons_active_low;
            const select_dpad = !reg.select_d_pad_active_low;

            self.select_buttons.set(select_buttons, false);
            self.select_dpad.set(select_dpad, false);

            if (select_buttons) {
                self.joypad.a.set(!reg.a_or_right_active_low, false);
                self.joypad.b.set(!reg.b_or_left_active_low, false);
                self.joypad.select.set(!reg.select_or_up_active_low, false);
                self.joypad.start.set(!reg.start_or_down_active_low, false);
            }

            if (select_dpad) {
                self.joypad.right.set(!reg.a_or_right_active_low, false);
                self.joypad.left.set(!reg.b_or_left_active_low, false);
                self.joypad.up.set(!reg.select_or_up_active_low, false);
                self.joypad.down.set(!reg.start_or_down_active_low, false);
            }
        }

        pub fn read(self: *This, _: u16) struct { MemoryFlag, u8 } {
            return self.inner_read();
        }

        /// May trigger an interrupt
        pub fn write(self: *This, _: u16, val: u8) MemoryFlag {
            const reg: JOYP = @bitCast(val);

            const old_regval = self.inner_read().@"1" & 0xF;
            self.select_buttons.set(!reg.select_buttons_active_low, true);
            self.select_dpad.set(!reg.select_d_pad_active_low, true);
            self.check_trigger_interrupt(old_regval);

            return .{};
        }

        fn shouldPanic(self: *This) void {
            self.select_buttons.should_panic = true;
            self.select_dpad.should_panic = true;
            inline for (@typeInfo(JoypadButtons).@"struct".field_names) |name| {
                @field(self.joypad, name).should_panic = true;
            }
        }
    };
}

const std = @import("std");

const TestContainer = @import("dependency_container.zig").Container(.{
    .interrupt = .mock,
    .joypad_backend = .mock_nil,
});

fn init_joypad(joypad: *TestContainer.Joypad) void {
    joypad.select_buttons.set(false, true);
    joypad.select_dpad.set(false, true);
    joypad.joypad.a.set(false, true);
    joypad.joypad.b.set(false, true);
    joypad.joypad.select.set(false, true);
    joypad.joypad.start.set(false, true);
    joypad.joypad.down.set(false, true);
    joypad.joypad.left.set(false, true);
    joypad.joypad.right.set(false, true);
    joypad.joypad.up.set(false, true);
}

test "JOYP reads selected buttons with active-low semantics" {
    var container = TestContainer.init();
    const backend = container.get_joypad_backend();
    const joypad = container.get_joypad();
    joypad.shouldPanic();

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
    joypad.shouldPanic();

    init_joypad(joypad);
    _ = joypad.write(0, 0xDF); // Select action buttons.

    var buttons = JoypadButtons{
        .a = false,
        .b = false,
        .select = false,
        .start = false,
        .up = false,
        .right = false,
        .left = false,
        .down = false,
    };

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
    joypad.shouldPanic();

    init_joypad(joypad);

    var buttons = JoypadButtons{
        .a = false,
        .b = false,
        .select = false,
        .start = false,
        .up = false,
        .right = false,
        .left = false,
        .down = false,
    };
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
