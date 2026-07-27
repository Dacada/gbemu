const std = @import("std");
const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const InterruptKind = @import("interrupt_kind.zig").InterruptKind;

const TrackedValue = @import("tracked_value.zig").TrackedValue;

pub const MockInterrupt = struct {
    requested: ?InterruptKind,
    vals: [2]u8,

    pub fn init() MockInterrupt {
        return MockInterrupt{
            .requested = null,
            .vals = .{ 0, 0 },
        };
    }

    pub fn pending(_: *MockInterrupt) ?InterruptKind {
        return null;
    }

    pub fn request(self: *MockInterrupt, kind: InterruptKind) void {
        self.requested = kind;
    }

    pub fn acknowledge(_: *MockInterrupt, _: InterruptKind) void {}

    pub fn peek(self: *MockInterrupt, addr: u16) u8 {
        return self.vals[addr];
    }

    pub fn poke(self: *MockInterrupt, addr: u16, val: u8) void {
        self.vals[addr] = val;
    }

    pub fn read(self: *MockInterrupt, addr: u16) struct { MemoryFlag, u8 } {
        return .{ .{}, self.peek(addr) };
    }

    pub fn write(self: *MockInterrupt, addr: u16, val: u8) MemoryFlag {
        self.poke(addr, val);
        return .{};
    }
};

pub const Interrupt = struct {
    ie: TrackedValue(u8),
    @"if": TrackedValue(u8),

    pub inline fn init() Interrupt {
        return Interrupt{
            .ie = .{},
            .@"if" = .{},
        };
    }

    pub inline fn request(self: *Interrupt, kind: InterruptKind) void {
        const v = self.@"if".get() | kind.asMask();
        self.@"if".set(v);
    }

    pub inline fn acknowledge(self: *Interrupt, kind: InterruptKind) void {
        const v = self.@"if".get() & ~kind.asMask();
        self.@"if".set(v);
    }

    pub fn pending(self: Interrupt) ?InterruptKind {
        inline for (comptime std.enums.values(InterruptKind)) |kind| {
            if (self.@"if".get() & self.ie.get() & kind.asMask() != 0) {
                return kind;
            }
        }
        return null;
    }

    pub fn peek(self: *Interrupt, addr: u16) u8 {
        _, const val = self.read(addr);
        return val;
    }

    pub fn poke(self: *Interrupt, addr: u16, val: u8) void {
        switch (addr) {
            0 => self.@"if".set(val),
            1 => self.ie.set(val),
            else => unreachable,
        }
    }

    pub fn read(self: *Interrupt, addr: u16) struct { MemoryFlag, u8 } {
        switch (addr) {
            0 => {
                if (self.@"if".is_init()) {
                    return .{ .{}, self.@"if".get() };
                } else {
                    return .{ .{ .uninitialized = true }, 0x00 };
                }
            },
            1 => {
                if (self.ie.is_init()) {
                    return .{ .{}, self.ie.get() };
                } else {
                    return .{ .{ .uninitialized = true }, 0x00 };
                }
            },
            else => unreachable,
        }
    }

    pub fn write(self: *Interrupt, addr: u16, val: u8) MemoryFlag {
        self.poke(addr, val);
        return .{};
    }
};

test "uninitialized if read flags" {
    var int = Interrupt.init();
    const flags, _ = int.read(0x00);
    try std.testing.expectEqualDeep(flags, MemoryFlag{ .uninitialized = true });
}

test "uninitialized ie read flags" {
    var int = Interrupt.init();
    const flags, _ = int.read(0x01);
    try std.testing.expectEqualDeep(flags, MemoryFlag{ .uninitialized = true });
}

test "request sets appropriate bit in IF" {
    var int = Interrupt.init();
    int.ie.set(0);
    int.@"if".set(0);
    int.request(InterruptKind.vblank);
    try std.testing.expectEqual(int.@"if".get(), InterruptKind.vblank.asMask());
}

test "acknowledge resets appropriate bit in IF" {
    var int = Interrupt.init();
    int.ie.set(0);
    int.@"if".set(0xFF);
    int.acknowledge(InterruptKind.vblank);
    try std.testing.expectEqual(int.@"if".get(), ~InterruptKind.vblank.asMask());
}

test "pending returns null when no interrupts are enabled" {
    var int = Interrupt.init();
    int.ie.set(0x00);
    int.@"if".set(InterruptKind.vblank.asMask());
    try std.testing.expect(int.pending() == null);
}

test "pending returns correct interrupt when enabled and requested" {
    var int = Interrupt.init();
    int.ie.set(InterruptKind.timer.asMask());
    int.@"if".set(InterruptKind.timer.asMask());
    try std.testing.expectEqual(int.pending(), InterruptKind.timer);
}

test "pending returns highest priority interrupt if multiple are set" {
    var int = Interrupt.init();
    int.ie.set(InterruptKind.vblank.asMask() | InterruptKind.timer.asMask());
    int.@"if".set(InterruptKind.vblank.asMask() | InterruptKind.timer.asMask());
    // Assuming VBlank is highest priority
    try std.testing.expectEqual(int.pending(), InterruptKind.vblank);
}

test "peek returns correct register values" {
    var int = Interrupt.init();
    int.ie.set(0xAB);
    int.@"if".set(0xCD);
    try std.testing.expectEqual(int.peek(0), 0xCD);
    try std.testing.expectEqual(int.peek(1), 0xAB);
}

test "poke writes correct values to registers" {
    var int = Interrupt.init();
    int.ie.set(0x00);
    int.@"if".set(0x00);
    int.poke(0, 0x12);
    int.poke(1, 0x34);
    try std.testing.expectEqual(int.@"if".get(), 0x12);
    try std.testing.expectEqual(int.ie.get(), 0x34);
}

test "read returns correct tuple for IF and IE" {
    var int = Interrupt.init();
    int.ie.set(0x56);
    int.@"if".set(0x78);
    const if_result = int.read(0);
    const ie_result = int.read(1);
    try std.testing.expectEqual(if_result[1], 0x78);
    try std.testing.expectEqual(ie_result[1], 0x56);
}

test "write modifies the correct register" {
    var int = Interrupt.init();
    int.ie.set(0x00);
    int.@"if".set(0x00);
    _ = int.write(0, 0xAA);
    _ = int.write(1, 0xBB);
    try std.testing.expectEqual(int.@"if".get(), 0xAA);
    try std.testing.expectEqual(int.ie.get(), 0xBB);
}
