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
            pub fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = static_oam[addr];
                const flags = MemoryFlag{ .uninitialized = !static_init_oam[addr] };
                return .{ flags, val };
            }

            pub fn write(_: *This, addr: u16, val: u8) MemoryFlag {
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

            /// For OAM corruption triggered by 16bit register increment/decrement
            pub fn fake_write(_: *This, _: u16) void {}
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

        pub fn init(_: *VideoBackend) This {
            return This{};
        }

        pub fn tick(_: *const This) void {}

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
