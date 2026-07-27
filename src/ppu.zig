const std = @import("std");

// DMG ONLY -- Basically everything, some other more specific stuff

const MemoryFlag = @import("memory_flag.zig").MemoryFlag;
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

        mode: Mode,

        enable: bool,
        window_enable: bool,
        obj_enable: bool,
        bg_and_window_enable: bool, // DMG ONLY -- Different meaning in CGB

        enable_lyc_stat_int: bool,
        enable_mode_2_stat_int: bool,
        enable_mode_1_stat_int: bool,
        enable_mode_0_stat_int: bool,

        window_tile_map_area_start: MemoryArea,
        bg_window_tile_data_area_start: MemoryArea,
        bg_tile_map_area_start: MemoryArea,

        obj_size: ObjectSize,

        lcd_y_coordinate_compare: u8,
        background_viewport_x: u8,
        background_viewport_y: u8,
        window_x_pos: u8,
        window_y_pos: u8,

        bg_palette: [4]Shade,
        obj_palettes: [2][3]Shade,

        lcd_y_coordinate: u8,

        pub fn init(_: *VideoBackend) This {
            return This{
                .mode = undefined,

                .enable = undefined,
                .window_enable = undefined,
                .obj_enable = undefined,
                .bg_and_window_enable = undefined,

                .enable_lyc_stat_int = undefined,
                .enable_mode_2_stat_int = undefined,
                .enable_mode_1_stat_int = undefined,
                .enable_mode_0_stat_int = undefined,

                .window_tile_map_area_start = undefined,
                .bg_window_tile_data_area_start = undefined,
                .bg_tile_map_area_start = undefined,

                .obj_size = undefined,

                .lcd_y_coordinate_compare = undefined,
                .background_viewport_x = undefined,
                .background_viewport_y = undefined,
                .window_x_pos = undefined,
                .window_y_pos = undefined,

                .bg_palette = undefined,
                .obj_palettes = undefined,

                .lcd_y_coordinate = undefined,
            };
        }

        pub fn tick(_: *This, _: u2) void {
            //everything here ...
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            if (addr > 0xB) {
                return .{ MemoryFlag{ .illegal = true }, 0xFF };
            }
            return .{ .{}, self.peek(addr) };
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
            self.poke(addr, val);
            return .{};
        }

        pub fn peek(self: *This, addr: u16) u8 {
            switch (addr) {
                0 => {
                    var ret: u8 = 0;

                    ret |= @intFromBool(self.enable);
                    ret <<= 1;
                    ret |= @intFromEnum(self.window_tile_map_area_start);
                    ret <<= 1;
                    ret |= @intFromBool(self.window_enable);
                    ret <<= 1;
                    ret |= @intFromEnum(self.bg_window_tile_data_area_start);
                    ret <<= 1;
                    ret |= @intFromEnum(self.bg_tile_map_area_start);
                    ret <<= 1;
                    ret |= @intFromEnum(self.obj_size);
                    ret <<= 1;
                    ret |= @intFromBool(self.obj_enable);
                    ret <<= 1;
                    ret |= @intFromBool(self.bg_and_window_enable);

                    return ret;
                },
                1 => {
                    var ret: u8 = 0;

                    ret |= 1;
                    ret <<= 1;
                    ret |= @intFromBool(self.enable_lyc_stat_int);
                    ret <<= 1;
                    ret |= @intFromBool(self.enable_mode_2_stat_int);
                    ret <<= 1;
                    ret |= @intFromBool(self.enable_mode_1_stat_int);
                    ret <<= 1;
                    ret |= @intFromBool(self.enable_mode_0_stat_int);
                    ret <<= 1;
                    ret |= @intFromBool(self.lcd_y_coordinate == self.lcd_y_coordinate_compare);

                    const mode: u8 = if (self.enable)
                        @intFromEnum(self.mode)
                    else
                        0;

                    ret <<= 2;
                    ret |= mode;
                    return ret;
                },
                2 => {
                    return self.background_viewport_y;
                },
                3 => {
                    return self.background_viewport_x;
                },
                4 => {
                    return self.lcd_y_coordinate;
                },
                5 => {
                    return self.lcd_y_coordinate_compare;
                },
                6 => {
                    // TODO: DMA TRANSFER
                    return 0x00;
                },
                7 => {
                    var ret: u8 = 0;
                    for (0..4) |i| {
                        const ii: u2 = @intCast(i);
                        ret |= @intFromEnum(self.bg_palette[ii]);
                        ret <<= ii * 2;
                    }
                    return ret;
                },
                8, 9 => {
                    var ret: u8 = 0;
                    for (0..3) |i| {
                        const ii: u2 = @intCast(i);
                        ret |= @intFromEnum(self.obj_palettes[addr - 8][ii]);
                        ret <<= ii * 2;
                    }
                    ret <<= 2;
                    ret |= 0b11;
                    return ret;
                },
                0xA => {
                    return self.window_y_pos;
                },
                0xB => {
                    return self.window_x_pos;
                },
                else => unreachable,
            }
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            switch (addr) {
                0 => {
                    self.enable = val & 0b1000_0000 != 0;
                    self.window_tile_map_area_start = @enumFromInt((val & 0b0100_0000) >> 6);
                    self.window_enable = val & 0b0010_0000 != 0;
                    self.bg_window_tile_data_area_start = @enumFromInt((val & 0b0001_0000) >> 4);
                    self.bg_tile_map_area_start = @enumFromInt((val & 0b0000_1000) >> 3);
                    self.obj_size = @enumFromInt((val & 0b0000_0100) >> 2);
                    self.obj_enable = val & 0b0000_0010 != 0;
                    self.bg_and_window_enable = val & 0b0000_0001 != 0;
                },
                1 => {
                    self.enable_lyc_stat_int = val & 0b0100_0000 != 0;
                    self.enable_mode_2_stat_int = val & 0b0010_0000 != 0;
                    self.enable_mode_1_stat_int = val & 0b0001_0000 != 0;
                    self.enable_mode_0_stat_int = val & 0b0000_1000 != 0;
                },
                2 => {
                    self.background_viewport_y = val;
                },
                3 => {
                    self.background_viewport_x = val;
                },
                4 => {
                    // Nothing, read-only register
                },
                5 => {
                    self.lcd_y_coordinate_compare = val;
                },
                6 => {
                    // TODO: DMA TRANSFER
                },
                7 => {
                    for (0..4) |i| {
                        const ii: u2 = @intCast(i);
                        const shift: u3 = ii * 2;
                        var mask: u8 = 0b0000_0011;
                        mask <<= shift;
                        self.bg_palette[i] = @enumFromInt((val & mask) >> shift);
                    }
                },
                8, 9 => {
                    for (0..3) |i| {
                        const ii: u2 = @intCast(i);
                        const shift: u3 = (ii + 1) * 2;
                        var mask: u8 = 0b0000_0011;
                        mask <<= shift;
                        self.obj_palettes[addr - 1][i] = @enumFromInt((val & mask) >> shift);
                    }
                },
                0xA => {
                    self.window_y_pos = val;
                },
                0xB => {
                    self.window_x_pos = val;
                },
                else => unreachable,
            }
        }
    };
}

// TODO: regiser read/write unit tests
