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
                const corrupted = Oam.maybe_corrupting_read(ppu);
                const illegal_access, const val = if (ppu.mode == .Mode0 or ppu.mode == .Mode1)
                    .{ false, static_oam[addr] }
                else
                    .{ true, 0xFF };
                const flags = MemoryFlag{
                    .uninitialized = !static_init_oam[addr],
                    .illegal = corrupted or illegal_access,
                };
                return .{ flags, val };
            }

            pub fn write(ppu: *This, addr: u16, val: u8) MemoryFlag {
                const corrupted = Oam.maybe_corrupting_write(ppu);
                const illegal_access = if (ppu.mode == .Mode0 or ppu.mode == .Mode1) brk: {
                    static_oam[addr] = val;
                    static_init_oam[addr] = true;
                    break :brk false;
                } else true;
                return .{ .illegal = corrupted or illegal_access };
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return static_oam[addr];
            }

            pub fn poke(_: *This, addr: u16, val: u8) void {
                static_oam[addr] = val;
            }

            /// For OAM corruption triggered by 16bit register increment/decrement
            pub fn fake_write(ppu: *This) void {
                _ = Oam.maybe_corrupting_write(ppu);
            }

            fn maybe_corrupting_read(ppu: *This) bool {
                if (ppu.mode == .Mode2) {
                    ppu.corrupting_oam_read = true;
                    return true;
                }
                return false;
            }

            fn maybe_corrupting_write(ppu: *This) bool {
                if (ppu.mode == .Mode2) {
                    ppu.corrupting_oam_write = true;
                    return true;
                }
                return false;
            }

            fn get_row(addr: u5) Row {
                const start: u16 = addr;
                start *= 8;

                var ret: Row = undefined;
                for (0..4) |i| {
                    ret[i] = Oam.peek(start + i * 2 + 1);
                    ret[i] <<= 8;
                    ret[i] |= Oam.peek(start + i * 2);
                }
                return ret;
            }

            fn set_row(addr: u5, row: Row) void {
                const start: u16 = addr;
                start *= 8;

                for (0..4) |i| {
                    const val = row[i];
                    Oam.poke(start + i * 2 + 1, (val & 0xFF00) >> 8);
                    Oam.poke(start + i * 2, val & 0xFF);
                }
            }

            fn execute_regular_corruption(addr: u5, comptime which: enum { read, write }) void {
                // https://gbdev.io/pandocs/OAM_Corruption_Bug.html#write-corruption
                // https://gbdev.io/pandocs/OAM_Corruption_Bug.html#read-corruption

                if (addr == 0) {
                    // no corruption for the first row
                    return;
                }

                const a, _, _, _ = Oam.get_row(addr);
                const b, const r1, const c, const r3 = Oam.get_row(addr - 1);

                const new = switch (which) {
                    .write => ((a ^ c) & (b ^ c)) ^ c,
                    .read => b | (a & c),
                };

                Oam.set_row(addr, .{ new, r1, c, r3 });
            }

            fn execute_write_corruption(addr: u5) void {
                Oam.execute_regular_corruption(addr, .write);
            }

            fn execute_read_corruption(addr: u5) void {
                Oam.execute_regular_corruption(addr, .read);
            }

            fn execute_read_write_corruption(addr: u5) void {
                // https://gbdev.io/pandocs/OAM_Corruption_Bug.html#read-during-increasedecrease

                if (addr > 3 and addr < 19) {
                    const a, _, _, _ = Oam.get_row(addr - 2);
                    const b, const r1, const d, const r3 = Oam.get_row(addr - 1);
                    const c, _, _, _ = Oam.get_row(addr);

                    const new = (b & (a | c | d)) | (a & c & d);

                    const row: Row = .{ new, r1, d, r3 };
                    Oam.set_row(addr - 1, row);

                    Oam.set_row(addr, row);
                    Oam.set_row(addr - 2, row);
                }

                Oam.execute_read_corruption(addr);
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

        pub fn tick(self: *This, tcycle: u2) void {
            // do some housekeeping for the previous M-cycle
            if (tcycle == 0) {
                self.apply_oam_corruption();
            }

            //everything here ...
        }

        fn apply_oam_corruption(self: *This) void {
            if (self.corrupting_oam_read and self.corrupting_oam_write) {
                Oam.execute_read_write_corruption(self.current_oam_row);
            } else if (self.corrupting_oam_read) {
                Oam.execute_read_corruption(self.current_oam_row);
            } else if (self.corrupting_oam_write) {
                Oam.execute_write_corruption(self.current_oam_row);
            }
            self.corrupting_oam_read = false;
            self.corrupting_oam_write = false;
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
