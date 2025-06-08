const std = @import("std");
const builtin = @import("builtin");
const DelayedReference = @import("reference.zig").DelayedReference;

const logger = if (builtin.is_test)
    struct {
        pub fn err(comptime _: []const u8, _: anytype) void {}
        pub fn warn(comptime _: []const u8, _: anytype) void {}
        pub fn info(comptime _: []const u8, _: anytype) void {}
        pub fn debug(comptime _: []const u8, _: anytype) void {}
    }
else
    std.log.scoped(.mmu);

var STATIC_MEMORY: [0x10000 - 0x8000]u8 = undefined;
const FF_ROM = [_]u8{0xFF} ** 0x8000;
const ZERO_ROM = [_]u8{0x00} ** 0x8000;

pub const Mmu = struct {
    rom: []const u8,
    _illegalMemoryOperationHappened: bool = false,

    pub fn init() Mmu {
        return Mmu{
            .rom = &FF_ROM,
        };
    }

    pub fn zeroize(self: *Mmu) void {
        self.rom = &ZERO_ROM;
        @memset(&STATIC_MEMORY, 0);
    }

    pub fn delayedReference(self: *Mmu, addr: u16) DelayedReference {
        return DelayedReference.fromMmu(self, addr);
    }

    fn isAccessLegal(addr: u16, iswrite: bool) bool {
        if (addr < 0x8000) {
            return !iswrite;
        } else if (addr < 0xE000) {
            return true;
        } else if (addr < 0xFE00) {
            return false;
        } else if (addr < 0xFEA0) {
            return true;
        } else if (addr < 0xFF00) {
            return false;
        } else {
            return true;
        }
    }

    pub fn mapRom(self: *Mmu, program: []const u8) void {
        self.rom = program;
    }

    pub fn dumpMemory(self: *const Mmu, addr: u16, buffer: []u8) void {
        for (0..buffer.len) |i| {
            buffer[i] = self.getValue(addr + @as(u16, @intCast(i)));
        }
    }

    pub fn write(self: *Mmu, addr: u16, val: u8) void {
        if (!Mmu.isAccessLegal(addr, true)) {
            logger.warn("Illegal write of value 0x{X:0>2} to address 0x{X:0>4}", .{ val, addr });
            self._illegalMemoryOperationHappened = true;
            return;
        }

        STATIC_MEMORY[addr - 0x8000] = val;
    }

    pub fn read(self: *Mmu, addr: u16) u8 {
        if (!Mmu.isAccessLegal(addr, false)) {
            logger.warn("Illegal read from address 0x{X:0>4}", .{addr});
            self._illegalMemoryOperationHappened = true;
            return 0;
        }

        if (addr < 0x8000) {
            if (addr < self.rom.len) {
                return self.rom[addr];
            } else {
                return 0x00;
            }
        }

        return STATIC_MEMORY[addr - 0x8000];
    }

    /// Write without side effects
    pub fn setValue(_: *Mmu, addr: u16, val: u8) void {
        STATIC_MEMORY[addr - 0x8000] = val;
    }

    /// Read without side effects
    pub fn getValue(self: *const Mmu, addr: u16) u8 {
        if (addr < 0x8000) {
            if (addr < self.rom.len) {
                return self.rom[addr];
            } else {
                return 0x00;
            }
        } else {
            return STATIC_MEMORY[addr - 0x8000];
        }
    }

    pub fn illegalMemoryOperationHappened(self: *const Mmu) bool {
        return self._illegalMemoryOperationHappened;
    }

    pub fn clearIllegalOperation(self: *Mmu) void {
        self._illegalMemoryOperationHappened = false;
    }
};

test "mmu zeroize" {
    var mmu = Mmu.init();

    const expected = [_]u8{0} ** 0x10000;

    mmu.zeroize();

    var actual: [0xFFFF + 1]u8 = undefined;
    mmu.dumpMemory(0, &actual);

    try std.testing.expectEqualSlices(u8, &expected, &actual);
}

test "mmu delayed reference" {
    var mmu = Mmu.init();
    mmu.zeroize();

    const addr: u16 = 0x8888;
    const value: u8 = 123;

    const ref = mmu.delayedReference(addr);
    try std.testing.expectEqual(0, ref.read());
    ref.write(value);
    try std.testing.expectEqual(value, ref.read());
}

test "mmu write/read" {
    var mmu = Mmu.init();
    mmu.zeroize();

    const addr: u16 = 0x8888;
    const value: u8 = 123;

    mmu.write(addr, value);
    try std.testing.expectEqual(value, mmu.read(addr));
}

test "mmu illegal write" {
    var mmu = Mmu.init();
    mmu.zeroize();

    const addr: u16 = 0x0000;
    const value: u8 = 123;

    mmu.write(addr, value);
    try std.testing.expect(mmu.illegalMemoryOperationHappened());
}

test "mmu illegal read" {
    var mmu = Mmu.init();
    mmu.zeroize();

    const addr: u16 = 0xE000;

    const res = mmu.read(addr);
    try std.testing.expectEqual(0, res);
    try std.testing.expect(mmu.illegalMemoryOperationHappened());
}

test "write and read it all" {
    var expected = [_]u8{0x00} ** 0x10000;
    var rom = [_]u8{0xAA} ** 0x8000;
    for (0..0x8000) |i| {
        const val: u8 = @intCast(i & 0xFF);
        rom[i] = val;
        expected[i] = val;
    }
    var mmu = Mmu.init();
    mmu.zeroize();
    mmu.mapRom(&rom);

    for (0x8000..0xE000) |i| {
        const val: u8 = @intCast(i & 0xFF);
        mmu.write(@intCast(i), val);
        expected[i] = val;
    }
    for (0xFE00..0xFEA0) |i| {
        const val: u8 = @intCast(i & 0xFF);
        mmu.write(@intCast(i), val);
        expected[i] = val;
    }
    for (0xFF00..0x10000) |i| {
        const val: u8 = @intCast(i & 0xFF);
        mmu.write(@intCast(i), val);
        expected[i] = val;
    }

    var actual: [0x10000]u8 = undefined;
    mmu.dumpMemory(0, &actual);

    try std.testing.expectEqualSlices(u8, &expected, &actual);
}
