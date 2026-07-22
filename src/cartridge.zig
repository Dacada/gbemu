const std = @import("std");

const MemoryFlag = @import("memory_flag.zig").MemoryFlag;

const logger = std.log.scoped(.cartridge);

pub const CartridgeHeaderParseError = error{
    NoHeader,
    NoRom,
    UnsupportedRomType,
    UnsupportedRomSize,
    UnsupportedRamSize,
};

const logo = [_]u8{
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
};

// TODO: might need to make rom/ram non global

// This memory will always contain the totality of the currently loaded ROM
var static_rom: [0x8000]u8 = undefined;

// This memory holds the cartridge's RAM
var static_ram: [0x2000]u8 = undefined;
var static_init_ram: [0x2000]bool = undefined;

// https://gbdev.io/pandocs/The_Cartridge_Header.html
pub fn Cartridge(comptime warn: bool) type {
    return struct {
        const This = @This();

        // DMG ONLY -- We interpret the title simply, however in "newer cartridges" this has a more complicated meaning
        title: []const u8,
        checksum: u8,

        // TODO: licensee code (old and new), CGB/SGB flag, destination code, version number, global checksum

        pub const Rom = struct {
            pub fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = static_rom[addr];
                return .{ .{}, val };
            }

            pub fn write(_: *This, _: u16, _: u8) MemoryFlag {
                return .{ .illegal = true };
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return static_rom[addr];
            }
            pub fn poke(_: *This, addr: u16, val: u8) void {
                static_rom[addr] = val;
            }
        };

        pub const Ram = struct {
            pub fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = static_ram[addr];
                const flags = MemoryFlag{ .uninitialized = !static_init_ram[addr] };
                return .{ flags, val };
            }

            pub fn write(_: *This, addr: u16, val: u8) MemoryFlag {
                static_ram[addr] = val;
                static_init_ram[addr] = true;
                return .{};
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return static_ram[addr];
            }
            pub fn poke(_: *This, addr: u16, val: u8) void {
                static_ram[addr] = val;
            }
        };

        pub fn init() This {
            return This{
                .title = "",
                .checksum = 0,
            };
        }

        pub const Diagnostics = struct {
            rom_type: ?u8,
            rom_size_code: ?u8,
            ram_size_code: ?u8,
        };

        pub fn loadFromBuffer(self: *This, rom: []const u8, diag: ?*Diagnostics) !void {
            const offset = 0x0100;

            if (rom.len < offset + 0x50) {
                return CartridgeHeaderParseError.NoHeader;
            }

            const header = rom[offset..][0..0x50];

            if (!std.mem.eql(u8, &logo, header[0x0004..0x0034])) {
                if (warn) {
                    logger.warn("could not find expected logo in cartridge header", .{});
                }
            }

            const title = try This.getTitleFromHeader(header);

            if (diag) |d| {
                d.rom_type = null;
                d.rom_size_code = null;
                d.ram_size_code = null;
            }

            const rom_type = header[0x47];
            if (diag) |d| {
                d.rom_type = rom_type;
            }
            if (rom_type != 0x00) {
                return CartridgeHeaderParseError.UnsupportedRomType;
            }

            const rom_size_code = header[0x48];
            if (diag) |d| {
                d.rom_size_code = rom_size_code;
            }
            if (rom_size_code != 0x00) {
                return CartridgeHeaderParseError.UnsupportedRomSize;
            }

            const ram_size_code = header[0x49];
            if (diag) |d| {
                d.ram_size_code = ram_size_code;
            }
            if (ram_size_code != 0x00) {
                return CartridgeHeaderParseError.UnsupportedRamSize;
            }

            var checksum: u8 = 0;
            for (0x34..0x4D) |idx| {
                checksum = checksum -% header[idx] -% 1;
            }

            if (checksum != header[0x4D]) {
                if (warn) {
                    logger.warn(
                        "header checksum does not match (0x{X} vs 0x{X})",
                        .{ checksum, header[0x4D] },
                    );
                }
            }

            if (rom.len != static_rom.len) {
                return CartridgeHeaderParseError.NoRom;
            }

            @memcpy(&static_rom, rom);
            @memset(&static_init_ram, false);

            self.title = title;
            self.checksum = checksum;
        }

        fn getTitleFromHeader(buff: []const u8) ![]const u8 {
            var len: usize = 0;
            for (buff[0x34..0x44]) |c| {
                if (c == 0x00) {
                    break;
                }
                len += 1;
            }
            return buff[0x34..(0x34 + len)];
        }
    };
}

var mock_static_memory = [_]u8{0xAA} ** 0xA000;

pub const MockCartridge = struct {
    pub const Rom = struct {
        pub fn read(_: *MockCartridge, addr: u16) struct { MemoryFlag, u8 } {
            const val = mock_static_memory[addr];
            return .{ .{}, val };
        }

        pub fn write(_: *MockCartridge, addr: u16, val: u8) MemoryFlag {
            mock_static_memory[addr] = val;
            return .{};
        }

        pub fn peek(_: *Cartridge, addr: u16) u8 {
            return mock_static_memory[addr];
        }

        pub fn poke(_: *Cartridge, addr: u16, val: u8) void {
            mock_static_memory[addr] = val;
        }
    };

    pub const Ram = struct {
        pub fn read(_: *MockCartridge, addr: u16) struct { MemoryFlag, u8 } {
            const val = mock_static_memory[addr + 0x8000];
            return .{ .{}, val };
        }

        pub fn write(_: *MockCartridge, addr: u16, val: u8) MemoryFlag {
            mock_static_memory[addr + 0x8000] = val;
            return .{};
        }

        pub fn peek(_: *Cartridge, addr: u16) u8 {
            return mock_static_memory[addr + 0x8000];
        }

        pub fn poke(_: *Cartridge, addr: u16, val: u8) void {
            mock_static_memory[addr + 0x8000] = val;
        }
    };
};

fn craftValidRomBuffer(buff: []u8, title: []const u8, checksum: u8) void {
    std.mem.copyForwards(u8, buff[0x104..], &logo);
    std.mem.copyForwards(u8, buff[0x134..0x144], &([_]u8{0} ** 0x10));
    std.mem.copyForwards(u8, buff[0x134..], title);
    buff[0x147] = 0x00;
    buff[0x148] = 0x00;
    buff[0x149] = 0x00;
    buff[0x14D] = checksum;
}

test "Cartridge loadFromBuffer loads valid ROM successfully" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    // Calculate a correct header checksum
    craftValidRomBuffer(&rom_buffer, "VALID", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    var cartridge = Cartridge(false).init();
    try cartridge.loadFromBuffer(&rom_buffer, null);

    try std.testing.expectEqualStrings("VALID", cartridge.title);
}

test "Cartridge loadFromBuffer rejects invalid logo" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    // Corrupt the logo
    rom_buffer[0x104] = 0x00;

    // Provide minimal valid header otherwise
    craftValidRomBuffer(&rom_buffer, "INVALID", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    // The logo warning is not fatal, so loading should still succeed
    var cartridge = Cartridge(false).init();
    try cartridge.loadFromBuffer(&rom_buffer, null);
    try std.testing.expectEqualStrings("INVALID", cartridge.title);
}

test "Cartridge loadFromBuffer rejects unsupported cartridge type" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    craftValidRomBuffer(&rom_buffer, "BADTYPE", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    rom_buffer[0x147] = 0x01; // Unsupported type

    var cartridge = Cartridge(false).init();
    try std.testing.expectError(CartridgeHeaderParseError.UnsupportedRomType, cartridge.loadFromBuffer(&rom_buffer, null));
}

test "Cartridge loadFromBuffer rejects unsupported rom size" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    craftValidRomBuffer(&rom_buffer, "BADSIZE", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    rom_buffer[0x148] = 0x01; // Unsupported rom size

    var cartridge = Cartridge(false).init();
    try std.testing.expectError(CartridgeHeaderParseError.UnsupportedRomSize, cartridge.loadFromBuffer(&rom_buffer, null));
}

test "Cartridge loadFromBuffer rejects unsupported ram size" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    craftValidRomBuffer(&rom_buffer, "BADRAM", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    rom_buffer[0x149] = 0x01; // Unsupported ram size

    var cartridge = Cartridge(false).init();
    try std.testing.expectError(CartridgeHeaderParseError.UnsupportedRamSize, cartridge.loadFromBuffer(&rom_buffer, null));
}

test "Cartridge loadFromBuffer warns on bad header checksum but still loads" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    craftValidRomBuffer(&rom_buffer, "BADCHK", 0x00); // Wrong checksum on purpose

    var cartridge = Cartridge(false).init();
    try cartridge.loadFromBuffer(&rom_buffer, null);
    try std.testing.expectEqualStrings("BADCHK", cartridge.title);
}
