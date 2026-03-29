const std = @import("std");

const MemoryFlag = @import("memory_flag.zig").MemoryFlag;

const logger = std.log.scoped(.ppu);

// TODO: this may need to be non global
var static_vram: [0x2000]u8 = undefined;
var static_init_vram = [_]bool{false} ** 0x2000;

var static_oam: [0xA0]u8 = undefined;
var static_init_oam = [_]bool{false} ** 0xA0;

const PpuMode = enum {
    Mode0,
    Mode1,
    Mode2,
    Mode3,
};

pub fn Ppu(VideoBackend: type) type {
    return struct {
        const This = @This();

        pub const Vram = struct {
            pub fn read(ppu: *This, addr: u16) struct { MemoryFlag, u8 } {
                if (ppu.mode == .Mode3) {
                    return .{ .{ .illegal = true }, 0xFF };
                }
                const val = static_vram[addr];
                const flags = MemoryFlag{ .uninitialized = !static_init_vram[addr] };
                return .{ flags, val };
            }

            pub fn write(ppu: *This, addr: u16, val: u8) MemoryFlag {
                if (ppu.mode == .Mode3) {
                    return .{ .illegal = true };
                }
                static_vram[addr] = val;
                static_init_vram[addr] = true;
                return .{};
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return static_vram[addr];
            }
            pub fn poke(_: *This, addr: u16, val: u8) void {
                static_vram[addr] = val;
            }
        };

        pub const Oam = struct {
            pub fn read(ppu: *This, addr: u16) struct { MemoryFlag, u8 } {
                if (ppu.mode == .Mode3) {
                    return .{ .{ .illegal = true }, 0xFF };
                }
                const val = static_oam[addr];
                const flags = MemoryFlag{ .uninitialized = !static_init_oam[addr] };
                return .{ flags, val };
            }

            pub fn write(ppu: *This, addr: u16, val: u8) MemoryFlag {
                if (ppu.mode == .Mode3) {
                    return .{ .illegal = true };
                }
                static_oam[addr] = val;
                static_init_oam[addr] = true;
                return .{};
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return static_oam[addr];
            }
            pub fn poke(_: *This, addr: u16, val: u8) void {
                static_oam[addr] = val;
            }

            pub fn idu_oam(_: *This, _: u16) void {}
        };

        pub const Forbidden = struct {
            pub fn read(_: *This, _: u16) struct { MemoryFlag, u8 } {
                return .{ MemoryFlag{ .illegal = true }, 0xFF };
            }

            pub fn write(_: *This, _: u16, _: u8) MemoryFlag {
                return .{ .illegal = true };
            }

            pub fn peek(_: *This, _: u16) u8 {
                return 0xFF;
            }
            pub fn poke(_: *This, _: u16, _: u8) void {}
        };

        pub const Lcd = struct {
            pub fn read(_: *This, _: u16) struct { MemoryFlag, u8 } {
                return .{ MemoryFlag{ .illegal = true }, 0xFF };
            }

            pub fn write(_: *This, _: u16, _: u8) MemoryFlag {
                return .{ .illegal = true };
            }

            pub fn peek(_: *This, _: u16) u8 {
                return 0xFF;
            }
            pub fn poke(_: *This, _: u16, _: u8) void {}
        };

        mode: PpuMode,

        pub fn init(_: *VideoBackend) This {
            return This{
                .mode = .Mode0,
            };
        }

        pub fn tick(_: *const This) void {}
    };
}
