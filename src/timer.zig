const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const TrackedValue = @import("tracked_value.zig").TrackedValue;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;

pub fn Timer(Apu: type, Interrupt: type) type {
    return struct {
        const This = @This();

        apu: *Apu,
        intr: *Interrupt,

        div: TrackedValue(u16),
        tima: TrackedValue(u8),
        tma: TrackedValue(u8),
        enable: TrackedValue(u1),
        clock_select: TrackedValue(u2),

        tima_overflow_next_tick: bool,
        wrote_tima_this_tick: bool,

        pub inline fn init(apu: *Apu, intr: *Interrupt) This {
            return This{
                .apu = apu,
                .intr = intr,
                .div = .{},
                .tima = .{},
                .tma = .{},
                .enable = .{},
                .clock_select = .{},
                .tima_overflow_next_tick = false,
                .wrote_tima_this_tick = false,
            };
        }

        pub fn tick(self: *This) void {
            if (self.tima_overflow_next_tick) {
                self.tima.set(self.tma.get());
                self.intr.request(InterruptKind.timer);
                self.tima_overflow_next_tick = false;
            }

            const prev_div = self.div.get();
            self.div.set(self.div.get() +% 1);
            self.triggerTimerTick(prev_div, self.enable.get(), self.clock_select.get());
            self.wrote_tima_this_tick = false;
        }

        pub fn peek(self: *This, addr: u16) u8 {
            _, const val = self.read(addr);
            return val;
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            switch (addr) {
                0 => self.div.set(@as(u16, @intCast(val)) << 8),
                1 => self.tima.set(val),
                2 => self.tma.set(val),
                3 => {
                    self.enable.set(@intCast((val & 0b100) >> 2));
                    self.clock_select.set(@intCast(val & 0b11));
                },
                else => unreachable,
            }
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            switch (addr) {
                0 => {
                    if (self.div.is_init()) {
                        return .{ .{}, @intCast(self.div.get() >> 8) };
                    } else {
                        return .{ .{ .undefined = true }, 0x00 };
                    }
                },
                1 => {
                    if (self.tima.is_init()) {
                        return .{ .{}, self.tima.get() };
                    } else {
                        return .{ .{ .undefined = true }, 0x00 };
                    }
                },
                2 => {
                    if (self.tma.is_init()) {
                        return .{ .{}, self.tma.get() };
                    } else {
                        return .{ .{ .undefined = true }, 0x00 };
                    }
                },
                3 => {
                    if (self.enable.is_init() and self.clock_select.is_init()) {
                        var val: u8 = 0;
                        val |= self.enable.get();
                        val <<= 2;
                        val |= self.clock_select.get();
                        return .{ .{}, val };
                    } else {
                        return .{ .{ .undefined = true }, 0x00 };
                    }
                },
                else => unreachable,
            }
        }

        pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            switch (addr) {
                0 => {
                    const prev_div = self.div.get();
                    self.div.set(0);
                    self.triggerTimerTick(prev_div, self.enable.get(), self.clock_select.get());
                },
                1 => {
                    // Assume tick will be called AFTER the CPU's tick, overwriting this write with TMA if needed
                    self.poke(addr, val);
                    // HOWEVER, if instead of overwriting it THIS TICK we would overwrite it NEXT TICK, then we DO NOT because of the write "cancelling" the overflow
                    self.wrote_tima_this_tick = true;
                },
                2 => {
                    // If this cycle would update tima, it will do so with the written to value, will work assuming timer is updated AFTER cpu
                    self.poke(addr, val);
                },
                3 => {
                    const prev_enable = self.enable.get();
                    const prev_clock_select = self.clock_select.get();
                    self.poke(addr, val);
                    self.triggerTimerTick(self.div.get(), prev_enable, prev_clock_select);
                },
                else => unreachable,
            }
            return .{};
        }

        // TODO: check if worth optimizing
        fn triggerTimerTick(self: *This, prev_div: u16, prev_enable: u1, prev_clock_select: u2) void {
            // DMG ONLY -- DIV-APU event uses a different bit in CGB in double speed mode
            if (prev_div & (1 << 10) != 0 and self.div.get() & (1 << 10) == 0) {
                self.apu.divtick();
            }

            // DMG ONLY -- In CGB the hardware is slightly different, review: https://gbdev.io/pandocs/Timer_Obscure_Behaviour.html#relation-between-timer-and-divider-register

            const mask = getBitMaskForDiv(self.clock_select.get());
            const prev_mask = getBitMaskForDiv(prev_clock_select);

            const bit = self.div.get() & mask != 0;
            const prev_bit = prev_div & prev_mask != 0;

            const curr = bit and self.enable.get() == 1;
            const prev = prev_bit and prev_enable == 1;

            if (prev and !curr) {
                self.doTimerTick();
            }
        }

        fn doTimerTick(self: *This) void {
            const prev_tima = self.tima.get();
            self.tima.set(self.tima.get() +% 1);
            const mask = 1 << 7;

            if (prev_tima & mask != 0 and self.tima.get() & mask == 0) {
                if (!self.wrote_tima_this_tick) {
                    self.tima_overflow_next_tick = true;
                }
            }
        }

        fn getBitMaskForDiv(sel: u2) u16 {
            // can be computed with bitwise operations but this looks cleaner
            return switch (sel) {
                0b00 => 1 << 9,
                0b01 => 1 << 3,
                0b10 => 1 << 5,
                0b11 => 1 << 7,
            };
        }
    };
}

const std = @import("std");

const TestContainer = @import("dependency_container.zig").Container(.{
    .apu = .mock,
    .interrupt = .mock,
});
const TestTimer = TestContainer.Timer;

test "timer increments TIMA when enabled and selected DIV bit falls" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.div.set(0b00000001111111111); // bit 9 set
    timer.enable.set(1); // enable
    timer.clock_select.set(0); // select bit 9
    timer.tima.set(0xAB);

    timer.tick(); // fall of bit 9
    try std.testing.expectEqual(0xAC, timer.tima.get());
}

test "timer does not increment TIMA when disabled" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.div.set(0b00000001111111111); // bit 9 set
    timer.enable.set(0); // disabled
    timer.clock_select.set(0); // bit 9

    const startTima = timer.tima.get();
    timer.tick();
    try std.testing.expectEqual(startTima, timer.tima.get());
}

test "TIMA overflows and sets interrupt on next tick" {
    var container = TestContainer.init();
    const intr = container.get_interrupt();
    var timer = container.get_timer();
    timer.enable.set(1); // enabled
    timer.clock_select.set(0); // bit 9
    timer.tma.set(0xAB);
    timer.div.set(0b00000001111111111); // bit 9 set

    timer.tima.set(0xFF);
    timer.tick(); // triggers doTimerTick, overflow latch

    try std.testing.expectEqual(0x00, timer.tima.get());
    try std.testing.expectEqual(false, intr.requested != null);

    timer.tick(); // overflow happens
    try std.testing.expectEqual(0xAB, timer.tima.get());
    try std.testing.expectEqual(InterruptKind.timer, intr.requested.?);
}

test "writing to TIMA cancels overflow latching" {
    var container = TestContainer.init();
    const intr = container.get_interrupt();
    var timer = container.get_timer();
    timer.enable.set(1);
    timer.clock_select.set(0);
    timer.tma.set(0x55);
    timer.div.set(0b00000001111111111); // bit 9 set
    timer.tima.set(0xFF);

    _ = timer.write(1, 0xFF); // cancel overflow
    timer.tick(); // no overflow detected
    timer.tick(); // no latched interrupt and tma write
    try std.testing.expectEqual(0x00, timer.tima.get());
    try std.testing.expectEqual(null, intr.requested);
}

test "writing to DIV causes TIMA tick if falling edge is triggered" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.enable.set(1); // enabled
    timer.clock_select.set(0); // bit 9
    timer.div.set(0b00000001111111111); // bit 9 set
    const startTima = timer.tima.get();

    _ = timer.write(0, 0x00); // reset DIV to 0, falling edge of bit 9
    try std.testing.expectEqual(startTima + 1, timer.tima.get());
}

test "writing to TAC can cause immediate TIMA increment if falling edge is triggered" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.div.set(0b00000001111111111); // bit 9 set
    timer.enable.set(1);
    timer.clock_select.set(0);
    timer.tima.set(0xAB);

    _ = timer.write(3, 0b00000000); // disable, select bit 9 — triggers fall
    try std.testing.expectEqual(0xAC, timer.tima.get());
}

test "writing to TMA during pending overflow updates TIMA correctly" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.div.set(0b00000001111111111); // bit 9 set
    timer.enable.set(1); // enabled
    timer.clock_select.set(0); // bit 9
    timer.tima.set(0xFF);

    timer.tick(); // latch overflow
    _ = timer.write(2, 0x33); // write new TMA
    timer.tick(); // apply latched overflow
    try std.testing.expectEqual(0x33, timer.tima.get());
}

test "no tick occurs when no falling edge on selected bit" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.div.set(0b0000000000000000);
    timer.enable.set(1);
    timer.clock_select.set(0);

    const startTima = timer.tima.get();
    timer.tick();
    try std.testing.expectEqual(startTima, timer.tima.get());
}

test "correct bit selected for various TAC inputs" {
    try std.testing.expectEqual(@as(u16, 1 << 9), TestTimer.getBitMaskForDiv(0b00000000));
    try std.testing.expectEqual(@as(u16, 1 << 3), TestTimer.getBitMaskForDiv(0b00000001));
    try std.testing.expectEqual(@as(u16, 1 << 5), TestTimer.getBitMaskForDiv(0b00000010));
    try std.testing.expectEqual(@as(u16, 1 << 7), TestTimer.getBitMaskForDiv(0b00000011));
}
