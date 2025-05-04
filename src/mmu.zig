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
            std.log.err("Illegal write of value {x} to address {x}", .{ val, addr });
            self.illegalMemoryOperationHappened = true;
        }
        self.memory[addr] = val;
    }

    pub fn read(self: *Mmu, addr: u16) u8 {
        if (!Mmu.isAccessLegal(addr, false)) {
            std.log.err("Illegal read from address {x}", .{addr});
            self.illegalMemoryOperationHappened = true;
            return 0;
        }
        return self.memory[addr];
    }
};
