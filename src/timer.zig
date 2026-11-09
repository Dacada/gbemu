const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;
const TrackedValue = @import("tracked_value.zig").TrackedValue;
const TAC = @import("mmio_register.zig").TAC;

pub fn Timer(Apu: type, Interrupt: type) type {
    return struct {
        const This = @This();

        apu: *Apu,
        intr: *Interrupt,

        div: TrackedValue(u16),
        tima: TrackedValue(u8),
        tma: TrackedValue(u8),
        enable: TrackedValue(bool),
        clock_select: TrackedValue(TAC.ClockSelect),

        tima_overflow_next_tick: bool,
        wrote_tima_this_tick: bool,

        pub inline fn init(apu: *Apu, intr: *Interrupt) This {
            return This{
                .apu = apu,
                .intr = intr,
                .div = .init(0xAAAA),
                .tima = .init(0xAA),
                .tma = .init(0xAA),
                .enable = .init(false),
                .clock_select = .init(.freq_65536),
                .tima_overflow_next_tick = false,
                .wrote_tima_this_tick = false,
            };
        }

        pub fn tick(self: *This) void {
            if (self.tima_overflow_next_tick) {
                self.tima = self.tma;
                self.intr.request(InterruptKind.timer);
                self.tima_overflow_next_tick = false;
            }

            const prev_div = self.div.resolve(null);
            self.div.set(prev_div +% 1, false);
            self.triggerTimerTick(prev_div, self.enable.resolve(null), self.clock_select.resolve(null), null);
            self.wrote_tima_this_tick = false;
        }

        fn inner_read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            var flag: MemoryFlag = .{};
            const val: u8 = switch (addr) {
                0 => @intCast(self.div.resolve(&flag) >> 8),
                1 => self.tima.resolve(&flag),
                2 => self.tma.resolve(&flag),
                3 => @bitCast(TAC{
                    .clock_select = self.clock_select.resolve(&flag),
                    .enable = self.enable.resolve(&flag),
                }),
                else => unreachable,
            };

            return .{ flag, val };
        }

        fn inner_write(self: *This, addr: u16, val: u8, real: bool) void {
            switch (addr) {
                0 => self.div.set(@as(u16, @intCast(val)) << 8, real),
                1 => self.tima.set(val, real),
                2 => self.tma.set(val, real),
                3 => {
                    const reg: TAC = @bitCast(val);
                    self.enable.set(reg.enable, real);
                    self.clock_select.set(reg.clock_select, real);
                },
                else => unreachable,
            }
        }

        pub fn peek(self: *This, addr: u16) u8 {
            return self.inner_read(addr).@"1";
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            return self.inner_write(addr, val, false);
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            return self.inner_read(addr);
        }

        pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            var flag: MemoryFlag = .{};

            switch (addr) {
                0 => {
                    const prev_div = self.div.resolve(&flag);
                    self.div.set(0, true);
                    self.triggerTimerTick(prev_div, self.enable.resolve(&flag), self.clock_select.resolve(&flag), &flag);
                },
                1 => {
                    // Assume tick will be called AFTER the CPU's tick, overwriting this write with TMA if needed
                    self.inner_write(addr, val, true);
                    // HOWEVER, if instead of overwriting it THIS TICK we would overwrite it NEXT TICK, then we DO NOT because of the write "cancelling" the overflow
                    self.wrote_tima_this_tick = true;
                },
                2 => {
                    // If this cycle would update tima, it will do so with the written to value, will work assuming timer is updated AFTER cpu
                    self.inner_write(addr, val, true);
                },
                3 => {
                    const prev_enable = self.enable.resolve(&flag);
                    const prev_clock_select = self.clock_select.resolve(&flag);
                    self.inner_write(addr, val, true);
                    self.triggerTimerTick(self.div.resolve(&flag), prev_enable, prev_clock_select, &flag);
                },
                else => unreachable,
            }
            return flag;
        }

        // TODO: check if worth optimizing
        fn triggerTimerTick(self: *This, prev_div: u16, prev_enable: bool, prev_clock_select: TAC.ClockSelect, flag: ?*MemoryFlag) void {
            // DMG ONLY -- DIV-APU event uses a different bit in CGB in double speed mode
            if (prev_div & (1 << 10) != 0 and self.div.resolve(flag) & (1 << 10) == 0) {
                self.apu.divtick();
            }

            // DMG ONLY -- In CGB the hardware is slightly different, review: https://gbdev.io/pandocs/Timer_Obscure_Behaviour.html#relation-between-timer-and-divider-register

            const mask = getBitMaskForDiv(self.clock_select.resolve(flag));
            const prev_mask = getBitMaskForDiv(prev_clock_select);

            const bit = self.div.resolve(flag) & mask != 0;
            const prev_bit = prev_div & prev_mask != 0;

            const curr = bit and self.enable.resolve(flag);
            const prev = prev_bit and prev_enable;

            if (prev and !curr) {
                self.doTimerTick(flag);
            }
        }

        fn doTimerTick(self: *This, flag: ?*MemoryFlag) void {
            const prev_tima = self.tima.resolve(flag);
            self.tima.set(prev_tima +% 1, false);
            const mask = 1 << 7;

            if (prev_tima & mask != 0 and self.tima.resolve(flag) & mask == 0) {
                if (!self.wrote_tima_this_tick) {
                    self.tima_overflow_next_tick = true;
                }
            }
        }

        fn getBitMaskForDiv(sel: TAC.ClockSelect) u16 {
            // can be computed with bitwise operations but this looks cleaner
            return switch (sel) {
                .freq_262144 => 1 << 3,
                .freq_65536 => 1 << 5,
                .freq_16384 => 1 << 7,
                .freq_4096 => 1 << 9,
            };
        }

        fn shouldPanic(self: *This) void {
            self.div.should_panic = true;
            self.tima.should_panic = true;
            self.tma.should_panic = true;
            self.enable.should_panic = true;
            self.clock_select.should_panic = true;
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
    timer.shouldPanic();

    timer.div.set(0b00000001111111111, true); // bit 9 set
    timer.enable.set(true, true); // enable
    timer.clock_select.set(.freq_4096, true); // select bit 9
    timer.tima.set(0xAB, true);

    timer.tick(); // fall of bit 9
    try std.testing.expectEqual(0xAC, timer.tima.resolve(null));
}

test "timer does not increment TIMA when disabled" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.shouldPanic();

    timer.div.set(0b00000001111111111, true); // bit 9 set
    timer.enable.set(false, true); // disabled
    timer.clock_select.set(.freq_4096, true); // bit 9
    timer.tima.set(0xAB, true);

    const startTima = timer.tima.resolve(null);
    timer.tick();
    try std.testing.expectEqual(startTima, timer.tima.resolve(null));
}

test "TIMA overflows and sets interrupt on next tick" {
    var container = TestContainer.init();
    const intr = container.get_interrupt();
    var timer = container.get_timer();
    timer.shouldPanic();

    timer.enable.set(true, true); // enabled
    timer.clock_select.set(.freq_4096, true); // bit 9
    timer.tma.set(0xAB, true);
    timer.div.set(0b00000001111111111, true); // bit 9 set

    timer.tima.set(0xFF, true);
    timer.tick(); // triggers doTimerTick, overflow latch

    try std.testing.expectEqual(0x00, timer.tima.resolve(null));
    try std.testing.expectEqual(false, intr.requested != null);

    timer.tick(); // overflow happens
    try std.testing.expectEqual(0xAB, timer.tima.resolve(null));
    try std.testing.expectEqual(InterruptKind.timer, intr.requested.?);
}

test "writing to TIMA cancels overflow latching" {
    var container = TestContainer.init();
    const intr = container.get_interrupt();
    var timer = container.get_timer();
    timer.shouldPanic();

    timer.enable.set(true, true);
    timer.clock_select.set(.freq_4096, true);
    timer.tma.set(0x55, true);
    timer.div.set(0b00000001111111111, true); // bit 9 set
    timer.tima.set(0xFF, true);

    _ = timer.write(1, 0xFF); // cancel overflow
    timer.tick(); // no overflow detected
    timer.tick(); // no latched interrupt and tma write
    try std.testing.expectEqual(0x00, timer.tima.resolve(null));
    try std.testing.expectEqual(null, intr.requested);
}

test "writing to DIV causes TIMA tick if falling edge is triggered" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.shouldPanic();

    timer.enable.set(true, true); // enabled
    timer.clock_select.set(.freq_4096, true); // bit 9
    timer.div.set(0b00000001111111111, true); // bit 9 set
    timer.tima.set(0xAB, true);
    const startTima = timer.tima.resolve(null);

    _ = timer.write(0, 0x00); // reset DIV to 0, falling edge of bit 9
    try std.testing.expectEqual(startTima + 1, timer.tima.resolve(null));
}

test "writing to TAC can cause immediate TIMA increment if falling edge is triggered" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.shouldPanic();

    timer.div.set(0b00000001111111111, true); // bit 9 set
    timer.enable.set(true, true);
    timer.clock_select.set(.freq_4096, true);
    timer.tima.set(0xAB, true);

    _ = timer.write(3, 0b00000000); // disable, select bit 9 — triggers fall
    try std.testing.expectEqual(0xAC, timer.tima.resolve(null));
}

test "writing to TMA during pending overflow updates TIMA correctly" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.shouldPanic();

    timer.div.set(0b00000001111111111, true); // bit 9 set
    timer.enable.set(true, true); // enabled
    timer.clock_select.set(.freq_4096, true); // bit 9
    timer.tima.set(0xFF, true);

    timer.tick(); // latch overflow
    _ = timer.write(2, 0x33); // write new TMA
    timer.tick(); // apply latched overflow
    try std.testing.expectEqual(0x33, timer.tima.resolve(null));
}

test "no tick occurs when no falling edge on selected bit" {
    var container = TestContainer.init();
    var timer = container.get_timer();
    timer.shouldPanic();

    timer.div.set(0b0000000000000000, true);
    timer.enable.set(true, true);
    timer.clock_select.set(.freq_4096, true);
    timer.tima.set(0xAB, true);

    const startTima = timer.tima.resolve(null);
    timer.tick();
    try std.testing.expectEqual(startTima, timer.tima.resolve(null));
}

test "correct bit selected for various TAC inputs" {
    try std.testing.expectEqual(@as(u16, 1 << 9), TestTimer.getBitMaskForDiv(.freq_4096));
    try std.testing.expectEqual(@as(u16, 1 << 3), TestTimer.getBitMaskForDiv(.freq_262144));
    try std.testing.expectEqual(@as(u16, 1 << 5), TestTimer.getBitMaskForDiv(.freq_65536));
    try std.testing.expectEqual(@as(u16, 1 << 7), TestTimer.getBitMaskForDiv(.freq_16384));
}
