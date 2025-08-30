const std = @import("std");

pub fn MemoryReference(Mmu: type) type {
    return union(enum) {
        const This = @This();

        mem_ref: struct {
            addr: u16,
            mmu: *Mmu,
        },
        ptr_ref: *u8,

        pub fn fromMemory(mmu: *Mmu, addr: u16) This {
            return .{
                .mem_ref = .{
                    .addr = addr,
                    .mmu = mmu,
                },
            };
        }

        pub fn fromPointer(ptr: *u8) This {
            return .{
                .ptr_ref = ptr,
            };
        }

        pub fn read(self: This) u8 {
            return switch (self) {
                .mem_ref => |mem_ref| mem_ref.mmu.read(mem_ref.addr),
                .ptr_ref => |ptr_ref| ptr_ref.*,
            };
        }

        pub fn write(self: This, val: u8) void {
            switch (self) {
                .mem_ref => |mem_ref| mem_ref.mmu.write(mem_ref.addr, val),
                .ptr_ref => |ptr_ref| ptr_ref.* = val,
            }
        }
    };
}
