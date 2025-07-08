const memory = @import("memory.zig");
const Memory = memory.Memory;
const MemoryFlag = memory.MemoryFlag;
const scheduler = @import("scheduler.zig");
const Scheduler = scheduler.Scheduler;

// TODO: stub, always open bus

pub const Serial = struct {
    sched: *Scheduler,
    running: bool = false,

    data: u8 = undefined,
    transfer_enable: u1 = undefined,
    // DMG ONLY -- Clock speed added by CGB
    clock_select: u1 = undefined,

    fn peek(selfptr: *anyopaque, addr: u16) u8 {
        const self: *Serial = @alignCast(@ptrCast(selfptr));
        return switch (addr) {
            0 => self.data,
            1 => blk: {
                var output: u8 = 0x00;
                output |= self.transfer_enable;
                output <<= 7;
                output |= self.clock_select;
                output |= 0b0111_1110;
                break :blk output;
            },
            else => unreachable,
        };
    }

    fn poke(selfptr: *anyopaque, addr: u16, val: u8) void {
        const self: *Serial = @alignCast(@ptrCast(selfptr));
        switch (addr) {
            0 => self.data = val,
            1 => {
                self.transfer_enable = @intCast((val & 0b1000_0000) >> 7);
                self.clock_select = @intCast(val & 0b0000_0001);
            },
            else => unreachable,
        }
    }

    fn read(selfptr: *anyopaque, addr: u16) struct { ?MemoryFlag, u8 } {
        return .{ null, peek(selfptr, addr) };
    }

    fn write(selfptr: *anyopaque, addr: u16, val: u8) ?MemoryFlag {
        poke(selfptr, addr, val);

        const self: *Serial = @alignCast(@ptrCast(selfptr));
        if (addr == 1 and !self.running and self.transfer_enable == 1 and self.clock_select == 1) {
            self.start_transfer();
            for (1..8) |i| {
                self.sched.schedule(.{ .context = selfptr, .callback = do_shift }, i * 512);
            }
            self.sched.schedule(.{ .context = selfptr, .callback = finish_transfer }, 8 * 512);
        }
        return null;
    }

    fn start_transfer(self: *Serial) void {
        self.running = true;
    }

    fn do_shift(selfptr: *anyopaque) void {
        const self: *Serial = @alignCast(@ptrCast(selfptr));
        self.data <<= 1;
        self.data |= 1; // only 1s coming in, simulating open bus
    }

    fn finish_transfer(selfptr: *anyopaque) void {
        const self: *Serial = @alignCast(@ptrCast(selfptr));
        do_shift(selfptr);
        self.running = false;
        self.transfer_enable = 0;
        // TODO: Schedule interrupt
    }

    pub fn memory(self: *Serial) Memory {
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

// Assume Serial and Scheduler are properly imported
test "Serial register read/write" {
    const sched = Scheduler{};
    var serial = Serial{ .sched = sched };

    // Test writing to and reading from data register (0)
    Serial.poke(&serial, 0, 0xA5);
    try std.testing.expectEqual(Serial.peek(&serial, 0), 0xA5);

    // Test writing to and reading from control register (1)
    Serial.poke(&serial, 1, 0b1000_0001); // Transfer enabled, clock select 1
    try std.testing.expectEqual(Serial.peek(&serial, 1) & 0b1000_0000, 0b1000_0000);
    try std.testing.expectEqual(Serial.peek(&serial, 1) & 0b0000_0001, 0b0000_0001);
}

test "Serial transfer initiation conditions" {
    const sched = Scheduler{};
    var serial = Serial{ .sched = sched };

    // Initially not running
    try std.testing.expect(!serial.running);

    // Write to control register to start transfer
    _ = Serial.write(&serial, 1, 0b1000_0001);
    try std.testing.expect(serial.running);

    // Should not restart if already running
    _ = Serial.write(&serial, 1, 0b1000_0001);
    try std.testing.expect(serial.running); // Still running, no interruption
}

test "Serial bit shift behavior" {
    const sched = Scheduler{};
    var serial = Serial{ .sched = sched, .data = 0b1010_0000 };

    Serial.do_shift(&serial);
    try std.testing.expectEqual(serial.data, 0b0100_0001); // Shift left, bring in '1'

    Serial.do_shift(&serial);
    try std.testing.expectEqual(serial.data, 0b1000_0011);
}

test "Serial transfer completion" {
    const sched = Scheduler{};
    var serial = Serial{ .sched = sched, .data = 0x00, .transfer_enable = 1, .clock_select = 1 };

    serial.start_transfer();
    try std.testing.expect(serial.running);

    // Simulate scheduled shifts
    for (1..7) |_| {
        Serial.do_shift(&serial);
    }

    Serial.finish_transfer(&serial);
    try std.testing.expect(!serial.running);
    try std.testing.expectEqual(serial.transfer_enable, 0);
}
