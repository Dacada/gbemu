const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;

// TODO: stub, always open bus

pub fn Serial(Scheduler: type, Interrupt: type) type {
    return struct {
        const This = @This();

        sched: *Scheduler,
        intr: *Interrupt,
        running: bool,

        data: u8,
        transfer_enable: u1,
        // DMG ONLY -- Clock speed added by CGB
        clock_select: u1,

        pub fn init(sched: *Scheduler, intr: *Interrupt) This {
            return This{
                .sched = sched,
                .intr = intr,
                .running = false,
                .data = undefined,
                .transfer_enable = undefined,
                .clock_select = undefined,
            };
        }

        pub fn peek(self: *This, addr: u16) u8 {
            return switch (addr) {
                0 => self.data,
                1 => blk: {
                    var output: u8 = 0x00;
                    output |= self.transfer_enable;
                    output <<= 7;
                    output |= self.clock_select;
                    break :blk output;
                },
                else => unreachable,
            };
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            switch (addr) {
                0 => self.data = val,
                1 => {
                    self.transfer_enable = @intCast((val & 0b1000_0000) >> 7);
                    self.clock_select = @intCast(val & 0b0000_0001);
                },
                else => unreachable,
            }
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            return .{ .{}, self.peek(addr) };
        }

        pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            self.poke(addr, val);

            if (addr == 1 and !self.running and self.transfer_enable == 1 and self.clock_select == 1) {
                self.startTransfer();
                for (1..8) |i| {
                    self.sched.schedule(.{ .context = self, .callback = doShift }, i * 512);
                }
                self.sched.schedule(.{ .context = self, .callback = finishTransfer }, 8 * 512);
            }
            return .{};
        }

        fn startTransfer(self: *This) void {
            self.running = true;
        }

        fn doShift(selfptr: *anyopaque) void {
            const self: *This = @ptrCast(@alignCast(selfptr));
            self.data <<= 1;
            self.data |= 1; // only 1s coming in, simulating open bus
        }

        fn finishTransfer(selfptr: *anyopaque) void {
            const self: *This = @ptrCast(@alignCast(selfptr));
            doShift(selfptr);
            self.running = false;
            self.transfer_enable = 0;
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
    var container = TestContainer.init(.{});
    var serial = try container.get_serial();

    // Test writing to and reading from data register (0)
    serial.poke(0, 0xA5);
    try std.testing.expectEqual(serial.peek(0), 0xA5);

    // Test writing to and reading from control register (1)
    serial.poke(1, 0b1000_0001); // Transfer enabled, clock select 1
    try std.testing.expectEqual(serial.peek(1) & 0b1000_0000, 0b1000_0000);
    try std.testing.expectEqual(serial.peek(1) & 0b0000_0001, 0b0000_0001);
}

test "Serial transfer initiation conditions" {
    var container = TestContainer.init(.{});
    var serial = try container.get_serial();

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
    var container = TestContainer.init(.{});
    var serial = try container.get_serial();

    serial.data = 0b1010_0000;

    TestSerial.doShift(serial);
    try std.testing.expectEqual(serial.data, 0b0100_0001); // Shift left, bring in '1'

    TestSerial.doShift(serial);
    try std.testing.expectEqual(serial.data, 0b1000_0011);
}

test "Serial transfer completion" {
    var container = TestContainer.init(.{});
    var serial = try container.get_serial();

    serial.data = 0x00;
    serial.transfer_enable = 1;
    serial.clock_select = 1;

    serial.startTransfer();
    try std.testing.expect(serial.running);

    // Simulate scheduled shifts
    for (1..7) |_| {
        TestSerial.doShift(serial);
    }

    TestSerial.finishTransfer(serial);
    try std.testing.expect(!serial.running);
    try std.testing.expectEqual(serial.transfer_enable, 0);
}
