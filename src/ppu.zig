const std = @import("std");

const MemoryFlag = @import("memory_flag.zig").MemoryFlag;

const logger = std.log.scoped(.ppu);

// TODO: this may need to be non global
var static_vram: [0x2000]u8 = undefined;
var static_init_vram = [_]bool{false} ** 0x2000;

var static_oam: [0xA0]u8 = undefined;
var static_init_oam = [_]bool{false} ** 0xA0;

pub fn Ppu(VideoBackend: type) type {
    return struct {
        const This = @This();

        pub const Vram = struct {
            pub fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = static_vram[addr];
                const flags = MemoryFlag{ .uninitialized = !static_init_vram[addr] };
                return .{ flags, val };
            }

            pub fn write(_: *This, addr: u16, val: u8) MemoryFlag {
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
            const Row = struct { u16, u16, u16, u16 };

            pub fn read(ppu: *This, addr: u16) struct { MemoryFlag, u8 } {
                const illegal_access, const val = if (ppu.mode == .Mode0 or ppu.mode == .Mode1)
                    .{ false, static_oam[addr] }
                else
                    .{ true, 0xFF };
                const flags = MemoryFlag{
                    .uninitialized = !static_init_oam[addr],
                    .illegal = illegal_access,
                };
                return .{ flags, val };
            }

            pub fn write(ppu: *This, addr: u16, val: u8) MemoryFlag {
                const illegal_access = if (ppu.mode == .Mode0 or ppu.mode == .Mode1) brk: {
                    static_oam[addr] = val;
                    static_init_oam[addr] = true;
                    break :brk false;
                } else true;
                return .{ .illegal = illegal_access };
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return static_oam[addr];
            }

            pub fn poke(_: *This, addr: u16, val: u8) void {
                static_oam[addr] = val;
            }
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

        const Mode = enum {
            Mode0,
            Mode1,
            Mode2,
            Mode3,
        };

        mode: Mode,

        current_oam_row: u5,
        corrupting_oam_read: bool,
        corrupting_oam_write: bool,

        pub fn init(_: *VideoBackend) This {
            return This{
                .mode = undefined,

                .current_oam_row = 0,
                .corrupting_oam_read = false,
                .corrupting_oam_write = false,
            };
        }

        pub fn tick(_: *This, _: u2) void {
            //everything here ...
        }

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
}
