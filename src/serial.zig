const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;
const Tracked = @import("tracked_value.zig").TrackedValue;
const SC = @import("mmio_register.zig").SC;

// TODO: stub, always open bus

pub fn Serial(Scheduler: type, Interrupt: type) type {
    return struct {
        const This = @This();

        sched: *Scheduler,
        intr: *Interrupt,

        running: bool,

        data: Tracked(u8),
        transfer_enable: Tracked(bool),
        // DMG ONLY -- Clock speed added by CGB
        clock_select: Tracked(SC.ClockSelect),

        pub fn init(sched: *Scheduler, intr: *Interrupt) This {
            return This{
                .sched = sched,
                .intr = intr,

                .running = false,

                .data = .init(0xAA),
                .transfer_enable = .init(true),
                .clock_select = .init(SC.ClockSelect.master),
            };
        }

        /// only for testing: panic when attempting to read undefined values
        pub fn should_panic(self: *This) void {
            self.data.should_panic = true;
            self.transfer_enable.should_panic = true;
            self.clock_select.should_panic = true;
        }

        fn inner_read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            var flags: MemoryFlag = .{};
            const value: u8 = switch (addr) {
                0 => self.data.resolve(&flags),
                1 => @bitCast(SC{
                    .clock_select = self.clock_select.resolve(&flags),
                    .transfer_enable = self.transfer_enable.resolve(&flags),
                }),
                else => unreachable,
            };
            return .{ flags, value };
        }

        fn inner_write(self: *This, addr: u16, val: u8) void {
            switch (addr) {
                0 => self.data.set(val),
                1 => {
                    const reg: SC = @bitCast(val);
                    self.transfer_enable.set(reg.transfer_enable);
                    self.clock_select.set(reg.clock_select);
                },
                else => unreachable,
            }
        }

        pub fn peek(self: *This, addr: u16) u8 {
            return self.inner_read(addr).@"1";
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            self.inner_write(addr, val);
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            return self.inner_read(addr);
        }

        pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            self.inner_write(addr, val);
            var flags: MemoryFlag = .{};

            if (addr == 1 and !self.running and self.transfer_enable.resolve(&flags) and self.clock_select.resolve(&flags) == .master) {
                self.startTransfer();
                for (1..8) |i| {
                    self.sched.schedule(.{ .context = self, .callback = doShift }, i * 512);
                }
                self.sched.schedule(.{ .context = self, .callback = finishTransfer }, 8 * 512);
            }
            return flags;
        }

        fn startTransfer(self: *This) void {
            self.running = true;
        }

        fn doShift(selfptr: *anyopaque) void {
            const self: *This = @ptrCast(@alignCast(selfptr));

            var data = self.data.resolve(null);
            data <<= 1;
            data |= 1; // only 1s coming in, simulating open bus
            self.data.set(data);
        }

        fn finishTransfer(selfptr: *anyopaque) void {
            const self: *This = @ptrCast(@alignCast(selfptr));
            doShift(selfptr);
            self.running = false;
            self.transfer_enable.set(false);
            self.intr.request(InterruptKind.serial);
        }
    };
}

const std = @import("std");

const TestContainer = @import("dependency_container.zig").Container(.{
    .scheduler = .mock,
    .interrupt = .mock,
});
const TestSerial = TestContainer.Serial;

test "Serial register read/write" {
    var container = TestContainer.init();
    var serial = container.get_serial();
    serial.should_panic();

    // Test writing to and reading from data register (0)
    serial.poke(0, 0xA5);
    try std.testing.expectEqual(serial.peek(0), 0xA5);

    // Test writing to and reading from control register (1)
    serial.poke(1, 0b1000_0001); // Transfer enabled, clock select 1
    try std.testing.expectEqual(serial.peek(1) & 0b1000_0000, 0b1000_0000);
    try std.testing.expectEqual(serial.peek(1) & 0b0000_0001, 0b0000_0001);
}

test "Serial transfer initiation conditions" {
    var container = TestContainer.init();
    var serial = container.get_serial();
    serial.should_panic();

    // Initially not running
    try std.testing.expect(!serial.running);

    // Write to control register to start transfer
    _ = serial.write(1, 0b1000_0001);
    try std.testing.expect(serial.running);

    // Should not restart if already running
    _ = serial.write(1, 0b1000_0001);
    try std.testing.expect(serial.running); // Still running, no interruption
}

test "Serial bit shift behavior" {
    var container = TestContainer.init();
    var serial = container.get_serial();
    serial.should_panic();

    serial.data.set(0b1010_0000);

    TestSerial.doShift(serial);
    try std.testing.expectEqual(serial.data.resolve(null), 0b0100_0001); // Shift left, bring in '1'

    TestSerial.doShift(serial);
    try std.testing.expectEqual(serial.data.resolve(null), 0b1000_0011);
}

test "Serial transfer completion" {
    var container = TestContainer.init();
    var serial = container.get_serial();
    serial.should_panic();

    serial.data.set(0x00);
    serial.transfer_enable.set(true);
    serial.clock_select.set(.master);

    serial.startTransfer();
    try std.testing.expect(serial.running);

    // Simulate scheduled shifts
    for (1..7) |_| {
        TestSerial.doShift(serial);
    }

    TestSerial.finishTransfer(serial);
    try std.testing.expect(!serial.running);
    try std.testing.expectEqual(serial.transfer_enable.resolve(null), false);
}
