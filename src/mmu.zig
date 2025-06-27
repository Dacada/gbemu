const std = @import("std");
const builtin = @import("builtin");
const memory = @import("memory.zig");
const Memory = memory.Memory;
const MemoryFlag = memory.MemoryFlag;
const SimpleMemory = memory.SimpleMemory;

const logger = std.log.scoped(.mmu);

// DMG ONLY -- In CGB, the second half is a switchable bank
var STATIC_WRAM = [_]u8{0x00} ** 0x2000;
var STATIC_INIT_WRAM = [_]bool{false} ** 0x2000;

var STATIC_HRAM = [_]u8{0x00} ** 0x79;
var STATIC_INIT_HRAM = [_]bool{false} ** 0x79;

pub const Mmu = struct {
    cartRom: Memory,
    vram: Memory,
    cartRam: Memory,
    wram: Memory = SimpleMemory(false, &STATIC_WRAM, &STATIC_INIT_WRAM).memory(),
    forbidden: Memory,
    oam: Memory,
    mmio: Memory,
    hram: Memory = SimpleMemory(false, &STATIC_HRAM, &STATIC_INIT_HRAM).memory(),

    fn dispatch(self: *const Mmu, addr: u16) struct { u16, Memory } {
        if (addr < 0x8000) return .{ addr, self.cartRom };
        if (addr < 0xA000) return .{ addr - 0x8000, self.vram };
        if (addr < 0xC000) return .{ addr - 0xA000, self.cartRam };
        if (addr < 0xE000) return .{ addr - 0xC000, self.wram };
        if (addr < 0xFE00) return .{ addr - 0xE000, self.wram }; // Echo RAM
        if (addr < 0xFEA0) return .{ addr - 0xFE00, self.oam };
        if (addr < 0xFF00) return .{ addr - 0xFEA0, self.forbidden };
        if (addr < 0xFF80) return .{ addr - 0xFF00, self.mmio };
        if (addr < 0xFFFF) return .{ addr - 0xFF80, self.hram };
        return .{ 0x80, self.mmio }; // IE is handled by mmio module
    }

    fn read_cb(selfptr: *anyopaque, addr: u16) struct { ?MemoryFlag, u8 } {
        const self: *Mmu = @alignCast(@ptrCast(selfptr));
        const norm, const mem = self.dispatch(addr);
        const flag, const val = mem.read_cb(mem.ctx, norm);
        if (flag) |f| {
            if (f.illegal) {
                logger.warn("Illegal read from address 0x{X:0>4}", .{addr});
            }
            if (f.uninitialized) {
                logger.warn("Uninitialized read from address 0x{X:0>4}", .{addr});
            }
        }
        return .{ flag, val };
    }

    fn write_cb(selfptr: *anyopaque, addr: u16, val: u8) ?MemoryFlag {
        const self: *Mmu = @alignCast(@ptrCast(selfptr));
        const norm, const mem = self.dispatch(addr);
        const flag = mem.write_cb(mem.ctx, norm, val);
        if (flag) |f| {
            if (f.illegal) {
                logger.warn("Illegal write of 0x{X:0>2} to address 0x{X:0>4}", .{ val, addr });
            }
        }
        return flag;
    }

    fn peek_cb(selfptr: *anyopaque, addr: u16) u8 {
        const self: *Mmu = @alignCast(@ptrCast(selfptr));
        const norm, const mem = self.dispatch(addr);
        return mem.peek_cb(mem.ctx, norm);
    }

    fn poke_cb(selfptr: *anyopaque, addr: u16, val: u8) void {
        const self: *Mmu = @alignCast(@ptrCast(selfptr));
        const norm, const mem = self.dispatch(addr);
        return mem.poke_cb(mem.ctx, norm, val);
    }

    pub fn memory(self: *Mmu) Memory {
        return Memory{
            .ctx = self,
            .read_cb = read_cb,
            .write_cb = write_cb,
            .peek_cb = peek_cb,
            .poke_cb = poke_cb,
        };
    }
};

var rom = [_]u8{0} ** 0x8000;
var vram = [_]u8{0} ** 0x2000;
var cram = [_]u8{0} ** 0x2000;
var wram = [_]u8{0} ** 0x2000;
var oam = [_]u8{0} ** 0xA0;
var forb = [_]u8{0} ** 0x60;
var mmio = [_]u8{0} ** 0x81;
var hram = [_]u8{0} ** 0x7F;

test "Mmu: write and read to all mapped memory regions" {
    var mmu = Mmu{
        .cartRom = SimpleMemory(false, &rom, null).memory(),
        .vram = SimpleMemory(false, &vram, null).memory(),
        .cartRam = SimpleMemory(false, &cram, null).memory(),
        .wram = SimpleMemory(false, &wram, null).memory(),
        .forbidden = SimpleMemory(false, &forb, null).memory(),
        .oam = SimpleMemory(false, &oam, null).memory(),
        .mmio = SimpleMemory(false, &mmio, null).memory(),
        .hram = SimpleMemory(false, &hram, null).memory(),
    };

    var mmuMem = mmu.memory();

    for (0..0x10000) |addr| {
        const val = 0xAB;
        mmuMem.write(@intCast(addr), val);
        const res = mmuMem.read(@intCast(addr));
        try std.testing.expectEqual(val, res);
        try std.testing.expect(!mmuMem.flags.illegal);
    }
}
