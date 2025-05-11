const std = @import("std");
const DelayedReference = @import("reference.zig").DelayedReference;

pub const Mmu = struct {
    memory: []u8,
    illegalMemoryOperationHappened: bool = false,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Mmu {
        return Mmu{
            .memory = try allocator.alloc(u8, 0xFFFF + 1),
            .allocator = allocator,
        };
    }

    pub fn zeroize(self: *Mmu) void {
        @memset(self.memory, 0);
    }

    pub fn deinit(self: *const Mmu) void {
        self.allocator.free(self.memory);
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

    pub fn write(self: *Mmu, addr: u16, val: u8) void {
        if (!Mmu.isAccessLegal(addr, true)) {
            std.log.warn("Illegal write of value 0x{X:0>2} to address 0x{X:0>4}", .{ val, addr });
            self.illegalMemoryOperationHappened = true;
            return;
        }
        self.memory[addr] = val;
    }

    pub fn read(self: *Mmu, addr: u16) u8 {
        if (!Mmu.isAccessLegal(addr, false)) {
            std.log.warn("Illegal read from address 0x{X:0>4}", .{addr});
            self.illegalMemoryOperationHappened = true;
            return 0;
        }
        return self.memory[addr];
    }
};

test "mmu zeroize" {
    var mmu = try Mmu.init(std.testing.allocator);
    defer mmu.deinit();

    const expected = [_]u8{0} ** 0x10000;

    mmu.zeroize();

    try std.testing.expectEqualSlices(u8, &expected, mmu.memory);
}

test "mmu delayed reference" {
    var mmu = try Mmu.init(std.testing.allocator);
    defer mmu.deinit();
    mmu.zeroize();

    const addr: u16 = 0x8888;
    const value: u8 = 123;

    const ref = mmu.delayedReference(addr);
    try std.testing.expectEqual(0, mmu.memory[addr]);
    try std.testing.expectEqual(0, ref.read());
    ref.write(value);
    try std.testing.expectEqual(value, mmu.memory[addr]);
    try std.testing.expectEqual(value, ref.read());
}

test "mmu write/read" {
    var mmu = try Mmu.init(std.testing.allocator);
    defer mmu.deinit();
    mmu.zeroize();

    const addr: u16 = 0x8888;
    const value: u8 = 123;

    mmu.write(addr, value);
    try std.testing.expectEqual(value, mmu.read(addr));
}

test "mmu illegal write" {
    var mmu = try Mmu.init(std.testing.allocator);
    defer mmu.deinit();
    mmu.zeroize();

    const addr: u16 = 0x0000;
    const value: u8 = 123;

    mmu.write(addr, value);
    try std.testing.expectEqual(0, mmu.memory[addr]);
    try std.testing.expect(mmu.illegalMemoryOperationHappened);
}

test "mmu illegal read" {
    var mmu = try Mmu.init(std.testing.allocator);
    defer mmu.deinit();
    mmu.zeroize();

    const addr: u16 = 0xE000;

    const res = mmu.read(addr);
    try std.testing.expectEqual(0, res);
    try std.testing.expect(mmu.illegalMemoryOperationHappened);
}
