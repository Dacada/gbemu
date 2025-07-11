const std = @import("std");

pub fn MemoryReference(Mmu: type) type {
    return union(enum) {
        const This = @This();

        memRef: struct {
            addr: u16,
            mmu: *Mmu,
        },
        ptrRef: *u8,

        pub fn fromMemory(mmu: *Mmu, addr: u16) This {
            return .{
                .memRef = .{
                    .addr = addr,
                    .mmu = mmu,
                },
            };
        }

        pub fn fromPointer(ptr: *u8) This {
            return .{
                .ptrRef = ptr,
            };
        }

        pub fn read(self: This) u8 {
            return switch (self) {
                .memRef => |memRef| memRef.mmu.read(memRef.addr),
                .ptrRef => |ptrRef| ptrRef.*,
            };
        }

        pub fn write(self: This, val: u8) void {
            switch (self) {
                .memRef => |memRef| memRef.mmu.write(memRef.addr, val),
                .ptrRef => |ptrRef| ptrRef.* = val,
            }
        }
    };
}
