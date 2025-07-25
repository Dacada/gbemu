const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;
const InterruptKind = @import("interruptKind.zig").InterruptKind;

pub fn Timer(Interrupt: type) type {
    return struct {
        const This = @This();

        intr: *Interrupt,

        div: u16,
        tima: u8,
        tma: u8,
        tac: u8,

        timaOverflowNextTick: bool,
        wroteTimaThisTick: bool,

        pub inline fn init(intr: *Interrupt) This {
            return This{
                .intr = intr,
                .div = undefined,
                .tima = undefined,
                .tma = undefined,
                .tac = undefined,
                .timaOverflowNextTick = false,
                .wroteTimaThisTick = false,
            };
        }

        pub fn tick(self: *This) void {
            if (self.timaOverflowNextTick) {
                self.tima = self.tma;
                self.intr.request(InterruptKind.timer);
                self.timaOverflowNextTick = false;
            }

            const prevDiv = self.div;
            self.div +%= 1;
            self.triggerTimerTick(prevDiv, self.tac);
            self.wroteTimaThisTick = false;
        }

        pub fn peek(self: *This, addr: u16) u8 {
            return switch (addr) {
                0 => @intCast(self.div >> 8),
                1 => self.tima,
                2 => self.tma,
                3 => self.tac,
                else => unreachable,
            };
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            switch (addr) {
                0 => self.div = @as(u16, @intCast(val)) << 8,
                1 => self.tima = val,
                2 => self.tma = val,
                3 => self.tac = val,
                else => unreachable,
            }
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            return .{ .{}, self.peek(addr) };
        }

        pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            switch (addr) {
                0 => {
                    const prevDiv = self.div;
                    self.div = 0;
                    self.triggerTimerTick(prevDiv, self.tac);
                },
                1 => {
                    // Assume tick will be called AFTER the CPU's tick, overwriting this write with TMA if needed
                    self.tima = val;
                    // HOWEVER, if instead of overwriting it THIS TICK we would overwrite it NEXT TICK, then we DO NOT because of the write "cancelling" the overflow
                    self.wroteTimaThisTick = true;
                },
                2 => {
                    // If this cycle would update tima, it will do so with the written to value, will work assuming timer is updated AFTER cpu
                    self.tma = val;
                },
                3 => {
                    const prevTac = self.tac;
                    self.tac = val;
                    self.triggerTimerTick(self.div, prevTac);
                },
                else => unreachable,
            }
            return .{};
        }

        // TODO: check if worth optimizing
        fn triggerTimerTick(self: *This, prevDiv: u16, prevTac: u8) void {
            // DMG ONLY -- In CGB the hardware is slightly different, review: https://gbdev.io/pandocs/Timer_Obscure_Behaviour.html#relation-between-timer-and-divider-register

            const mask = getBitMaskForDiv(self.tac);
            const prevMask = getBitMaskForDiv(prevTac);

            const bit = self.div & mask != 0;
            const prevBit = prevDiv & prevMask != 0;

            const enable = self.tac & (1 << 2) != 0;
            const prevEnable = prevTac & (1 << 2) != 0;

            const curr = bit and enable;
            const prev = prevBit and prevEnable;

            if (prev and !curr) {
                self.doTimerTick();
            }
        }

        fn doTimerTick(self: *This) void {
            const prevTima = self.tima;
            self.tima +%= 1;
            const mask = 1 << 7;

            if (prevTima & mask != 0 and self.tima & mask == 0) {
                if (!self.wroteTimaThisTick) {
                    self.timaOverflowNextTick = true;
                }
            }
        }

        fn getBitMaskForDiv(tac: u8) u16 {
            const sel: u2 = @intCast(tac & 0b11);
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

const DummyInterrupt = struct {
    requested: ?InterruptKind = null,

    fn request(self: *DummyInterrupt, kind: InterruptKind) void {
        self.requested = kind;
    }
};

const MockedTimer = Timer(DummyInterrupt);

test "timer increments TIMA when enabled and selected DIV bit falls" {
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&intr);
    timer.div = 0b00000001111111111; // bit 9 set
    timer.tac = 0b00000100; // enable + select bit 9
    timer.tima = 0xAB;

    timer.tick(); // fall of bit 9
    try std.testing.expectEqual(0xAC, timer.tima);
}

test "timer does not increment TIMA when disabled" {
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&intr);
    timer.div = 0b00000001111111111; // bit 9 set
    timer.tac = 0b00000000; // disabled

    const startTima = timer.tima;
    timer.tick();
    try std.testing.expectEqual(startTima, timer.tima);
}

test "TIMA overflows and sets interrupt on next tick" {
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&intr);
    timer.tac = 0b00000100; // enabled, bit 9
    timer.tma = 0xAB;
    timer.div = 0b00000001111111111; // bit 9 set

    timer.tima = 0xFF;
    timer.tick(); // triggers doTimerTick, overflow latch

    try std.testing.expectEqual(0x00, timer.tima);
    try std.testing.expectEqual(false, intr.requested != null);

    timer.tick(); // overflow happens
    try std.testing.expectEqual(0xAB, timer.tima);
    try std.testing.expectEqual(InterruptKind.timer, intr.requested.?);
}

test "writing to TIMA cancels overflow latching" {
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&intr);
    timer.tac = 0b00000100;
    timer.tma = 0x55;
    timer.div = 0b00000001111111111; // bit 9 set
    timer.tima = 0xFF;

    _ = timer.write(1, 0xFF); // cancel overflow
    timer.tick(); // no overflow detected
    timer.tick(); // no latched interrupt and tma write
    try std.testing.expectEqual(0x00, timer.tima);
    try std.testing.expectEqual(null, intr.requested);
}

test "writing to DIV causes TIMA tick if falling edge is triggered" {
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&intr);
    timer.tac = 0b00000100; // enabled, bit 9
    timer.div = 0b00000001111111111; // bit 9 set
    const startTima = timer.tima;

    _ = timer.write(0, 0x00); // reset DIV to 0, falling edge of bit 9
    try std.testing.expectEqual(startTima + 1, timer.tima);
}

test "writing to TAC can cause immediate TIMA increment if falling edge is triggered" {
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&intr);
    timer.div = 0b00000001111111111; // bit 9 set
    timer.tac = 0b00000100;
    timer.tima = 0xAB;

    _ = timer.write(3, 0b00000000); // disable, select bit 9 — triggers fall
    try std.testing.expectEqual(0xAC, timer.tima);
}

test "writing to TMA during pending overflow updates TIMA correctly" {
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&intr);
    timer.div = 0b00000001111111111; // bit 9 set
    timer.tac = 0b00000100;
    timer.tima = 0xFF;

    timer.tick(); // latch overflow
    _ = timer.write(2, 0x33); // write new TMA
    timer.tick(); // apply latched overflow
    try std.testing.expectEqual(0x33, timer.tima);
}

test "no tick occurs when no falling edge on selected bit" {
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&intr);
    timer.div = 0b0000000000000000;
    timer.tac = 0b00000100;

    const startTima = timer.tima;
    timer.tick();
    try std.testing.expectEqual(startTima, timer.tima);
}

test "correct bit selected for various TAC inputs" {
    try std.testing.expectEqual(@as(u16, 1 << 9), MockedTimer.getBitMaskForDiv(0b00000000));
    try std.testing.expectEqual(@as(u16, 1 << 3), MockedTimer.getBitMaskForDiv(0b00000001));
    try std.testing.expectEqual(@as(u16, 1 << 5), MockedTimer.getBitMaskForDiv(0b00000010));
    try std.testing.expectEqual(@as(u16, 1 << 7), MockedTimer.getBitMaskForDiv(0b00000011));
}
