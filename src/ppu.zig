const std = @import("std");

const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;

const logger = std.log.scoped(.ppu);

// TODO: this may need to be non global
var STATIC_VRAM: [0x2000]u8 = undefined;
var STATIC_INIT_VRAM = [_]bool{false} ** 0x2000;

var STATIC_OAM: [0xA0]u8 = undefined;
var STATIC_INIT_OAM = [_]bool{false} ** 0xA0;

pub const Ppu = struct {
    pub const Vram = struct {
        pub fn read(_: *Ppu, addr: u16) struct { MemoryFlag, u8 } {
            const val = STATIC_VRAM[addr];
            const flags = MemoryFlag{ .uninitialized = !STATIC_INIT_VRAM[addr] };
            return .{ flags, val };
        }

        pub fn write(_: *Ppu, addr: u16, val: u8) MemoryFlag {
            STATIC_VRAM[addr] = val;
            STATIC_INIT_VRAM[addr] = true;
            return .{};
        }

        pub fn peek(_: *Ppu, addr: u16) u8 {
            return STATIC_VRAM[addr];
        }
        pub fn poke(_: *Ppu, addr: u16, val: u8) void {
            STATIC_VRAM[addr] = val;
        }
    };

    pub const Oam = struct {
        pub fn read(_: *Ppu, addr: u16) struct { MemoryFlag, u8 } {
            const val = STATIC_OAM[addr];
            const flags = MemoryFlag{ .uninitialized = !STATIC_INIT_OAM[addr] };
            return .{ flags, val };
        }

        pub fn write(_: *Ppu, addr: u16, val: u8) MemoryFlag {
            STATIC_OAM[addr] = val;
            STATIC_INIT_OAM[addr] = true;
            return .{};
        }

        pub fn peek(_: *Ppu, addr: u16) u8 {
            return STATIC_OAM[addr];
        }
        pub fn poke(_: *Ppu, addr: u16, val: u8) void {
            STATIC_OAM[addr] = val;
        }
    };

    pub const Forbidden = struct {
        pub fn read(_: *Ppu, _: u16) struct { MemoryFlag, u8 } {
            return .{ MemoryFlag{ .illegal = true }, 0xFF };
        }

        pub fn write(_: *Ppu, _: u16, _: u8) MemoryFlag {
            return .{ .illegal = true };
        }

        pub fn peek(_: *Ppu, _: u16) u8 {
            return 0xFF;
        }
        pub fn poke(_: *Ppu, _: u16, _: u8) void {}
    };

    pub fn init() Ppu {
        return Ppu{};
    }

    pub fn tick(_: *const Ppu) void {}
};
