const std = @import("std");
const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;

pub const Interrupt = packed struct {
    ie: u8,
    @"if": u8,

    pub inline fn init() Interrupt {
        return Interrupt{
            .ie = undefined,
            .@"if" = undefined,
        };
    }

    pub inline fn request(self: *Interrupt, kind: InterruptKind) void {
        self.@"if" |= kind.asMask();
    }

    pub inline fn acknowledge(self: *Interrupt, kind: InterruptKind) void {
        self.@"if" &= ~kind.asMask();
    }

    pub fn pending(self: Interrupt) ?InterruptKind {
        inline for (comptime std.enums.values(InterruptKind)) |kind| {
            if (self.@"if" & self.ie & kind.asMask() != 0) {
                return kind;
            }
        }
        return null;
    }

    pub fn peek(self: *Interrupt, addr: u16) u8 {
        return switch (addr) {
            0 => self.@"if",
            1 => self.ie,
            else => unreachable,
        };
    }

    pub fn poke(self: *Interrupt, addr: u16, val: u8) void {
        switch (addr) {
            0 => self.@"if" = val,
            1 => self.ie = val,
            else => unreachable,
        }
    }

    pub fn read(self: *Interrupt, addr: u16) struct { MemoryFlag, u8 } {
        return .{ .{}, self.peek(addr) };
    }

    pub fn write(self: *Interrupt, addr: u16, val: u8) MemoryFlag {
        self.poke(addr, val);
        return .{};
    }
};

test "request sets appropriate bit in IF" {
    var int = Interrupt{ .ie = 0, .@"if" = 0 };
    int.request(InterruptKind.vblank);
    try std.testing.expectEqual(int.@"if", InterruptKind.vblank.asMask());
}

test "acknowledge resets appropriate bit in IF" {
    var int = Interrupt{ .ie = 0, .@"if" = 0xFF };
    int.acknowledge(InterruptKind.vblank);
    try std.testing.expectEqual(int.@"if", ~InterruptKind.vblank.asMask());
}

test "pending returns null when no interrupts are enabled" {
    var int = Interrupt{ .ie = 0x00, .@"if" = InterruptKind.vblank.asMask() };
    try std.testing.expect(int.pending() == null);
}

test "pending returns correct interrupt when enabled and requested" {
    var int = Interrupt{ .ie = InterruptKind.timer.asMask(), .@"if" = InterruptKind.timer.asMask() };
    try std.testing.expectEqual(int.pending(), InterruptKind.timer);
}

test "pending returns highest priority interrupt if multiple are set" {
    var int = Interrupt{
        .ie = InterruptKind.vblank.asMask() | InterruptKind.timer.asMask(),
        .@"if" = InterruptKind.vblank.asMask() | InterruptKind.timer.asMask(),
    };
    // Assuming VBlank is highest priority
    try std.testing.expectEqual(int.pending(), InterruptKind.vblank);
}

test "peek returns correct register values" {
    var int = Interrupt{ .ie = 0xAB, .@"if" = 0xCD };
    try std.testing.expectEqual(int.peek(0), 0xCD);
    try std.testing.expectEqual(int.peek(1), 0xAB);
}

test "poke writes correct values to registers" {
    var int = Interrupt{ .ie = 0x00, .@"if" = 0x00 };
    int.poke(0, 0x12);
    int.poke(1, 0x34);
    try std.testing.expectEqual(int.@"if", 0x12);
    try std.testing.expectEqual(int.ie, 0x34);
}

test "read returns correct tuple for IF and IE" {
    var int = Interrupt{ .ie = 0x56, .@"if" = 0x78 };
    const if_result = int.read(0);
    const ie_result = int.read(1);
    try std.testing.expectEqual(if_result[1], 0x78);
    try std.testing.expectEqual(ie_result[1], 0x56);
}

test "write modifies the correct register" {
    var int = Interrupt{ .ie = 0x00, .@"if" = 0x00 };
    _ = int.write(0, 0xAA);
    _ = int.write(1, 0xBB);
    try std.testing.expectEqual(int.@"if", 0xAA);
    try std.testing.expectEqual(int.ie, 0xBB);
}
