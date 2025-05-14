const std = @import("std");
const DelayedReference = @import("reference.zig").DelayedReference;

// Every instance of Mmu is a view into this memory region.
var STATIC_EMULATED_MEMORY: [0xFFFF + 1]u8 = undefined;

pub const Mmu = struct {
    _memory: []u8,
    _illegalMemoryOperationHappened: bool = false,

    pub fn init() Mmu {
        return Mmu{
            ._memory = &STATIC_EMULATED_MEMORY,
        };
    }

    pub fn zeroize(self: *Mmu) void {
        @memset(self._memory, 0);
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
        for (program, 0..) |instr, idx| {
            self._memory[idx] = instr;
        }
    }

    pub fn dumpMemory(self: *const Mmu, buffer: []u8) void {
        for (self._memory, 0..) |byte, idx| {
            buffer[idx] = byte;
        }
    }

    pub fn write(self: *Mmu, addr: u16, val: u8) void {
        if (!Mmu.isAccessLegal(addr, true)) {
            std.log.warn("Illegal write of value 0x{X:0>2} to address 0x{X:0>4}", .{ val, addr });
            self._illegalMemoryOperationHappened = true;
            return;
        }
        self._memory[addr] = val;
    }

    pub fn read(self: *Mmu, addr: u16) u8 {
        if (!Mmu.isAccessLegal(addr, false)) {
            std.log.warn("Illegal read from address 0x{X:0>4}", .{addr});
            self._illegalMemoryOperationHappened = true;
            return 0;
        }
        return self._memory[addr];
    }

    pub fn illegalMemoryOperationHappened(self: *const Mmu) bool {
        return self._illegalMemoryOperationHappened;
    }
};

test "mmu zeroize" {
    var mmu = Mmu.init();

    const expected = [_]u8{0} ** 0x10000;

    mmu.zeroize();

    var actual: [0xFFFF + 1]u8 = undefined;
    mmu.dumpMemory(&actual);

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
