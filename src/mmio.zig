const memory = @import("memory.zig");
const Memory = memory.Memory;
const SimpleMemory = memory.SimpleMemory;

var STATIC_MMIO = [_]u8{0} ** 0x81;

pub const Mmio = struct {
    mmio: Memory,
    pub fn init() Mmio {
        return Mmio{
            .mmio = SimpleMemory(false, &STATIC_MMIO, null).memory(),
        };
    }
};
