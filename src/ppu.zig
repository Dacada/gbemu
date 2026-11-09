const std = @import("std");

// DMG ONLY -- Basically everything, some other more specific stuff

const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
const TrackedValue = @import("tracked_value.zig").TrackedValue;

// TODO: this may need to be non global
var static_vram: [0x2000]u8 = undefined;
var static_init_vram: [0x2000]bool = @splat(false);

var static_oam: [0xA0]u8 = undefined;
var static_init_oam: [0xA0]bool = @splat(false);

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
                var flag: MemoryFlag = .{};

                const mode = ppu.mode.resolve(&flag);
                const illegal_access, const val = if (mode == .Mode0 or mode == .Mode1)
                    .{ false, static_oam[addr] }
                else
                    .{ true, 0xFF };
                const flags = MemoryFlag{
                    .uninitialized = flag.uninitialized or !static_init_oam[addr],
                    .illegal = flag.illegal or illegal_access,
                };

                return .{ flags, val };
            }

            pub fn write(ppu: *This, addr: u16, val: u8) MemoryFlag {
                var flag: MemoryFlag = .{};

                const mode = ppu.mode.resolve(&flag);
                const illegal_access = if (mode == .Mode0 or mode == .Mode1) brk: {
                    static_oam[addr] = val;
                    static_init_oam[addr] = true;
                    break :brk false;
                } else true;
                return .{ .illegal = illegal_access, .uninitialized = flag.uninitialized };
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

        const Mode = enum(u2) {
            Mode0 = 0,
            Mode1 = 1,
            Mode2 = 2,
            Mode3 = 3,
        };

        const MemoryArea = enum(u1) {
            Low = 0,
            High = 1,
        };

        const ObjectSize = enum(u1) {
            Square = 0,
            Stacked = 1,
        };

        const Shade = enum(u2) {
            White = 0,
            LightGray = 1,
            DarkGray = 2,
            Black = 3,
        };

        mode: TrackedValue(Mode),

        enable: TrackedValue(bool),
        window_enable: TrackedValue(bool),
        obj_enable: TrackedValue(bool),
        bg_and_window_enable: TrackedValue(bool), // DMG ONLY -- Different meaning in CGB

        enable_lyc_stat_int: TrackedValue(bool),
        enable_mode_2_stat_int: TrackedValue(bool),
        enable_mode_1_stat_int: TrackedValue(bool),
        enable_mode_0_stat_int: TrackedValue(bool),

        window_tile_map_area_start: TrackedValue(MemoryArea),
        bg_window_tile_data_area_start: TrackedValue(MemoryArea),
        bg_tile_map_area_start: TrackedValue(MemoryArea),

        obj_size: TrackedValue(ObjectSize),

        lcd_y_coordinate_compare: TrackedValue(u8),
        background_viewport_x: TrackedValue(u8),
        background_viewport_y: TrackedValue(u8),
        window_x_pos: TrackedValue(u8),
        window_y_pos: TrackedValue(u8),

        bg_palette: [4]TrackedValue(Shade),
        obj_palettes: [2][3]TrackedValue(Shade),

        lcd_y_coordinate: TrackedValue(u8),

        pub fn init(_: *VideoBackend) This {
            return This{
                .mode = .init(.Mode0),

                .enable = .init(true),
                .window_enable = .init(false),
                .obj_enable = .init(true),
                .bg_and_window_enable = .init(false),

                .enable_lyc_stat_int = .init(true),
                .enable_mode_2_stat_int = .init(false),
                .enable_mode_1_stat_int = .init(true),
                .enable_mode_0_stat_int = .init(false),

                .window_tile_map_area_start = .init(.High),
                .bg_window_tile_data_area_start = .init(.Low),
                .bg_tile_map_area_start = .init(.High),

                .obj_size = .init(.Stacked),

                .lcd_y_coordinate_compare = .init(0xAA),
                .background_viewport_x = .init(0xAA),
                .background_viewport_y = .init(0xAA),
                .window_x_pos = .init(0xAA),
                .window_y_pos = .init(0xAA),

                .bg_palette = .{
                    .init(.Black),
                    .init(.Black),
                    .init(.Black),
                    .init(.Black),
                },
                .obj_palettes = .{
                    .{
                        .init(.Black),
                        .init(.Black),
                        .init(.Black),
                    },
                    .{
                        .init(.Black),
                        .init(.Black),
                        .init(.Black),
                    },
                },

                .lcd_y_coordinate = .init(0xAA),
            };
        }

        pub fn tick(_: *This, _: u2) void {
            //everything here ...
        }

        fn inner_read(_: *This, _: u16) struct { MemoryFlag, u8 } {
            return .{ .{}, 0xFF };
        }

        fn inner_write(_: *This, _: u16, _: u8, _: bool) void {}

        pub fn peek(self: *This, addr: u16) u8 {
            return self.inner_read(addr).@"1";
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            return self.inner_write(addr, val, false);
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            if (addr > 0xB) {
                return .{ MemoryFlag{ .illegal = true }, 0xFF };
            }
            return self.inner_read(addr);
        }

        pub fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            if (addr > 0xB or addr == 0x4) {
                // TODO: should writing to a read-only register trigger the illegal write flag (addr=0x4)?
                return .{ .illegal = true };
            }
            if (addr == 0x0) {
                // TODO: I have internal value that are not initialized directly by register writes. Should enabling the
                // PPU initialize them? To which values?
            }

            self.inner_write(addr, val, true);
            return .{};
        }
    };
}

// TODO: regiser read/write unit tests
