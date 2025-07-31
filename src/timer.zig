const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;
const InterruptKind = @import("interruptKind.zig").InterruptKind;

pub fn Timer(Apu: type, Interrupt: type) type {
    return struct {
        const This = @This();

        apu: *Apu,
        intr: *Interrupt,

        div: u16,
        tima: u8,
        tma: u8,
        enable: u1,
        clock_select: u2,

        timaOverflowNextTick: bool,
        wroteTimaThisTick: bool,

        pub inline fn init(apu: *Apu, intr: *Interrupt) This {
            return This{
                .apu = apu,
                .intr = intr,
                .div = undefined,
                .tima = undefined,
                .tma = undefined,
                .enable = undefined,
                .clock_select = undefined,
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
            self.triggerTimerTick(prevDiv, self.enable, self.clock_select);
            self.wroteTimaThisTick = false;
        }

        pub fn peek(self: *This, addr: u16) u8 {
            return switch (addr) {
                0 => @intCast(self.div >> 8),
                1 => self.tima,
                2 => self.tma,
                3 => blk: {
                    var val: u8 = 0;
                    val |= self.enable;
                    val <<= 2;
                    val |= self.clock_select;
                    break :blk val;
                },
                else => unreachable,
            };
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            switch (addr) {
                0 => self.div = @as(u16, @intCast(val)) << 8,
                1 => self.tima = val,
                2 => self.tma = val,
                3 => {
                    self.enable = @intCast((val & 0b100) >> 2);
                    self.clock_select = @intCast(val & 0b11);
                },
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
                    self.triggerTimerTick(prevDiv, self.enable, self.clock_select);
                },
                1 => {
                    // Assume tick will be called AFTER the CPU's tick, overwriting this write with TMA if needed
                    self.poke(addr, val);
                    // HOWEVER, if instead of overwriting it THIS TICK we would overwrite it NEXT TICK, then we DO NOT because of the write "cancelling" the overflow
                    self.wroteTimaThisTick = true;
                },
                2 => {
                    // If this cycle would update tima, it will do so with the written to value, will work assuming timer is updated AFTER cpu
                    self.poke(addr, val);
                },
                3 => {
                    const prevEnable = self.enable;
                    const prevClockSelect = self.clock_select;
                    self.poke(addr, val);
                    self.triggerTimerTick(self.div, prevEnable, prevClockSelect);
                },
                else => unreachable,
            }
            return .{};
        }

        // TODO: check if worth optimizing
        fn triggerTimerTick(self: *This, prevDiv: u16, prevEnable: u1, prevClockSelect: u2) void {
            // DMG ONLY -- DIV-APU event uses a different bit in CGB in double speed mode
            if (prevDiv & (1 << 10) != 0 and self.div & (1 << 10) == 0) {
                self.apu.divtick();
            }

            // DMG ONLY -- In CGB the hardware is slightly different, review: https://gbdev.io/pandocs/Timer_Obscure_Behaviour.html#relation-between-timer-and-divider-register

            const mask = getBitMaskForDiv(self.clock_select);
            const prevMask = getBitMaskForDiv(prevClockSelect);

            const bit = self.div & mask != 0;
            const prevBit = prevDiv & prevMask != 0;

            const curr = bit and self.enable == 1;
            const prev = prevBit and prevEnable == 1;

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

const DummyInterrupt = struct {
    requested: ?InterruptKind = null,

    fn request(self: *DummyInterrupt, kind: InterruptKind) void {
        self.requested = kind;
    }
};

const DummyApu = struct {
    fn divtick(_: DummyApu) void {}
};

const MockedTimer = Timer(DummyApu, DummyInterrupt);

test "timer increments TIMA when enabled and selected DIV bit falls" {
    var apu = DummyApu{};
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&apu, &intr);
    timer.div = 0b00000001111111111; // bit 9 set
    timer.enable = 1; // enable
    timer.clock_select = 0; // select bit 9
    timer.tima = 0xAB;

    timer.tick(); // fall of bit 9
    try std.testing.expectEqual(0xAC, timer.tima);
}

test "timer does not increment TIMA when disabled" {
    var apu = DummyApu{};
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&apu, &intr);
    timer.div = 0b00000001111111111; // bit 9 set
    timer.enable = 0; // disabled

    const startTima = timer.tima;
    timer.tick();
    try std.testing.expectEqual(startTima, timer.tima);
}

test "TIMA overflows and sets interrupt on next tick" {
    var apu = DummyApu{};
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&apu, &intr);
    timer.enable = 1; // enabled
    timer.clock_select = 0; // bit 9
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
    var apu = DummyApu{};
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&apu, &intr);
    timer.enable = 1;
    timer.clock_select = 0;
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
    var apu = DummyApu{};
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&apu, &intr);
    timer.enable = 1; // enabled
    timer.clock_select = 0; // bit 9
    timer.div = 0b00000001111111111; // bit 9 set
    const startTima = timer.tima;

    _ = timer.write(0, 0x00); // reset DIV to 0, falling edge of bit 9
    try std.testing.expectEqual(startTima + 1, timer.tima);
}

test "writing to TAC can cause immediate TIMA increment if falling edge is triggered" {
    var apu = DummyApu{};
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&apu, &intr);
    timer.div = 0b00000001111111111; // bit 9 set
    timer.enable = 1;
    timer.clock_select = 0;
    timer.tima = 0xAB;

    _ = timer.write(3, 0b00000000); // disable, select bit 9 — triggers fall
    try std.testing.expectEqual(0xAC, timer.tima);
}

test "writing to TMA during pending overflow updates TIMA correctly" {
    var apu = DummyApu{};
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&apu, &intr);
    timer.div = 0b00000001111111111; // bit 9 set
    timer.enable = 1; // enabled
    timer.clock_select = 0; // bit 9
    timer.tima = 0xFF;

    timer.tick(); // latch overflow
    _ = timer.write(2, 0x33); // write new TMA
    timer.tick(); // apply latched overflow
    try std.testing.expectEqual(0x33, timer.tima);
}

test "no tick occurs when no falling edge on selected bit" {
    var apu = DummyApu{};
    var intr = DummyInterrupt{};
    var timer = MockedTimer.init(&apu, &intr);
    timer.div = 0b0000000000000000;
    timer.enable = 1;
    timer.clock_select = 0;

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
