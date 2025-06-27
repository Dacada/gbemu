const memory = @import("memory.zig");
const Memory = memory.Memory;
const MemoryFlag = memory.MemoryFlag;

// TODO: stub, doesn't actually do any serial transfering, read/write are the same as peek/poke with no actuual
// functionality

pub const Serial = packed struct {
    data: u8,
    transfer_enable: u1,
    // DMG ONLY -- Clock added by CGB
    clock_select: u1,

    fn peek(selfptr: *anyopaque, addr: u16) u8 {
        const self: *Serial = @alignCast(@ptrCast(selfptr));
        return switch (addr) {
            0 => self.data,
            1 => blk: {
                var output: u8 = 0x00;
                output |= self.transfer_enable;
                output <<= 7;
                output |= self.clock_select;
                output |= 0b0111_1110;
                break :blk output;
            },
            else => unreachable,
        };
    }

    fn poke(selfptr: *anyopaque, addr: u16, val: u8) void {
        const self: *Serial = @alignCast(@ptrCast(selfptr));
        switch (addr) {
            0 => self.data = val,
            1 => {
                self.transfer_enable = @intCast((val & 0b1000_0000) >> 7);
                self.clock_select = @intCast(val & 0b0000_0001);
            },
            else => unreachable,
        }
    }

    fn read(selfptr: *anyopaque, addr: u16) struct { ?MemoryFlag, u8 } {
        return .{ null, peek(selfptr, addr) };
    }

    fn write(selfptr: *anyopaque, addr: u16, val: u8) ?MemoryFlag {
        poke(selfptr, addr, val);
        return null;
    }

    pub fn memory(self: *Serial) Memory {
        return Memory{
            .ctx = self,
            .peek_cb = peek,
            .poke_cb = poke,
            .read_cb = read,
            .write_cb = write,
        };
    }
};
