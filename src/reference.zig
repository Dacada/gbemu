const std = @import("std");
const Memory = @import("memory.zig").Memory;

pub const MemoryReference = union(enum) {
    memRef: struct {
        addr: u16,
        mem: *Memory,
    },
    ptrRef: *u8,

    pub fn fromMemory(mem: *Memory, addr: u16) MemoryReference {
        return .{
            .memRef = .{
                .addr = addr,
                .mem = mem,
            },
        };
    }

    pub fn fromPointer(ptr: *u8) MemoryReference {
        return .{
            .ptrRef = ptr,
        };
    }

    pub fn read(self: MemoryReference) u8 {
        return switch (self) {
            .memRef => |memRef| memRef.mem.read(memRef.addr),
            .ptrRef => |ptrRef| ptrRef.*,
        };
    }

    pub fn write(self: MemoryReference, val: u8) void {
        switch (self) {
            .memRef => |memRef| memRef.mem.write(memRef.addr, val),
            .ptrRef => |ptrRef| ptrRef.* = val,
        }
    }
};
