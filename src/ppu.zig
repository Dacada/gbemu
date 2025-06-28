const std = @import("std");
const memory = @import("memory.zig");
const Memory = memory.Memory;
const SimpleMemory = memory.SimpleMemory;
const MemoryFlag = memory.MemoryFlag;

const logger = std.log.scoped(.ppu);

var STATIC_VRAM: [0x2000]u8 = undefined;
var STATIC_VRAM_INIT = [_]bool{false} ** 0x2000;

var STATIC_OAM: [0xA0]u8 = undefined;
var STATIC_OAM_INIT = [_]bool{false} ** 0xA0;

fn read_forbidden_cb(_: *anyopaque, _: u16) struct { ?MemoryFlag, u8 } {
    return .{ MemoryFlag{ .illegal = true }, 0xFF };
}

fn write_forbidden_cb(_: *anyopaque, _: u16, _: u8) ?MemoryFlag {
    return .{ .illegal = true };
}

fn peek_forbidden_cb(_: *anyopaque, _: u16) u8 {
    logger.warn("peek from unbacked memory will always return 0xFF", .{});
    return 0xFF;
}

fn poke_forbidden_cb(_: *anyopaque, _: u16, _: u8) void {
    logger.warn("poke to unbacked memory ignored", .{});
}

pub const Ppu = struct {
    vram: Memory,
    oam: Memory,
    forbidden: Memory,
    pub fn init() Ppu {
        return Ppu{
            .vram = SimpleMemory(false, &STATIC_VRAM, &STATIC_VRAM_INIT).memory(),
            .oam = SimpleMemory(false, &STATIC_OAM, &STATIC_OAM_INIT).memory(),
            .forbidden = Memory{
                .ctx = undefined,
                .read_cb = read_forbidden_cb,
                .write_cb = write_forbidden_cb,
                .peek_cb = peek_forbidden_cb,
                .poke_cb = poke_forbidden_cb,
            },
        };
    }
    pub fn tick(_: *const Ppu) void {}
};
