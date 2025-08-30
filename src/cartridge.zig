const std = @import("std");

const MemoryFlag = @import("memory_flag.zig").MemoryFlag;

const logger = std.log.scoped(.cartridge);

pub const CartridgeHeaderParseError = error{
    NoHeader,
    NoRom,
    UnsupportedCartridgeType,
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
pub const Cartridge = struct {
    // DMG ONLY -- We interpret the title simply, however in "newer cartridges" this has a more complicated meaning
    title: []const u8,
    checksum: u8,

    // TODO: licensee code (old and new), CGB/SGB flag, destination code, version number, global checksum

    pub const Rom = struct {
        pub fn read(_: *Cartridge, addr: u16) struct { MemoryFlag, u8 } {
            const val = static_rom[addr];
            return .{ .{}, val };
        }

        pub fn write(_: *Cartridge, _: u16, _: u8) MemoryFlag {
            return .{ .illegal = true };
        }

        pub fn peek(_: *Cartridge, addr: u16) u8 {
            return static_rom[addr];
        }
        pub fn poke(_: *Cartridge, addr: u16, val: u8) void {
            static_rom[addr] = val;
        }
    };

    pub const Ram = struct {
        pub fn read(_: *Cartridge, addr: u16) struct { MemoryFlag, u8 } {
            const val = static_ram[addr];
            const flags = MemoryFlag{ .uninitialized = !static_init_ram[addr] };
            return .{ flags, val };
        }

        pub fn write(_: *Cartridge, addr: u16, val: u8) MemoryFlag {
            static_ram[addr] = val;
            static_init_ram[addr] = true;
            return .{};
        }

        pub fn peek(_: *Cartridge, addr: u16) u8 {
            return static_ram[addr];
        }
        pub fn poke(_: *Cartridge, addr: u16, val: u8) void {
            static_ram[addr] = val;
        }
    };

    pub fn fromFile(file: std.fs.File) !Cartridge {
        const offset = 0x0100;
        const header = static_rom[offset..(offset + 0x50)];
        try file.seekTo(offset);
        const read_size = try file.readAll(header);
        if (read_size < header.len) {
            logger.err("could not find a header in the rom: could only read 0x{X} bytes at offset 0x{X}", .{ read_size, offset });
            return CartridgeHeaderParseError.NoHeader;
        }

        if (!std.mem.eql(u8, &logo, header[0x0004..0x0034])) {
            logger.warn("could not find expected logo in cartridege header", .{});
        }

        const title = try Cartridge.getTitleFromHeader(header);

        const rom_type = header[0x47];
        if (rom_type != 0x00) { // TODO: support other types
            logger.err("unsupported cartridge type: 0x{X}", .{rom_type});
            return CartridgeHeaderParseError.UnsupportedCartridgeType;
        }

        const rom_size_code = header[0x48];
        if (rom_size_code != 0x00) { // TODO: support other sizes
            logger.err("unsupported rom size code: 0x{X}", .{rom_size_code});
            return CartridgeHeaderParseError.UnsupportedCartridgeType;
        }

        const ram_size_code = header[0x49];
        if (ram_size_code != 0x00) { // TODO: support other sizes
            logger.err("unsupported ram size code: 0x{X}", .{ram_size_code});
            return CartridgeHeaderParseError.UnsupportedCartridgeType;
        }

        var checksum: u8 = 0;
        for (0x34..0x4D) |idx| {
            checksum = checksum -% header[idx] -% 1;
        }
        if (checksum != header[0x4D]) {
            logger.warn("header checksum does not match (0x{X} vs 0x{X})", .{ checksum, header[0x4D] });
        }

        try file.seekTo(0);
        const read_size_rom = try file.readAll(&static_rom);
        if (read_size_rom != static_rom.len) {
            logger.err("unexpected cartridge file size, could only read 0x{X} bytes for rom but wanted to read 0x{X}", .{ read_size_rom, static_rom.len });
            return CartridgeHeaderParseError.NoRom;
        }

        @memset(&static_init_ram, false);

        return Cartridge{
            .title = title,
            .checksum = checksum,
        };
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

fn writeBuffAndReturnFileForReading(buff: []const u8) !struct { std.fs.File, std.testing.TmpDir } {
    var tmp_dir = std.testing.tmpDir(.{});
    const file = try tmp_dir.dir.createFile("test.gb", .{});
    defer file.close();
    try file.writeAll(buff);
    return .{ try tmp_dir.dir.openFile("test.gb", .{}), tmp_dir };
}

fn craftValidRomBuffer(buff: []u8, title: []const u8, checksum: u8) void {
    std.mem.copyForwards(u8, buff[0x104..], &logo);
    std.mem.copyForwards(u8, buff[0x134..0x144], &([_]u8{0} ** 0x10));
    std.mem.copyForwards(u8, buff[0x134..], title);
    buff[0x147] = 0x00;
    buff[0x148] = 0x00;
    buff[0x149] = 0x00;
    buff[0x14D] = checksum;
}

test "Cartridge fromFile loads valid ROM successfully" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    // Calculate a correct header checksum
    craftValidRomBuffer(&rom_buffer, "VALID", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    var tmp = try writeBuffAndReturnFileForReading(&rom_buffer);
    defer tmp[0].close();
    defer tmp[1].cleanup();

    const cartridge = try Cartridge.fromFile(tmp[0]);

    try std.testing.expectEqualStrings("VALID", cartridge.title);
}

test "Cartridge fromFile rejects invalid logo" {
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

    var tmp = try writeBuffAndReturnFileForReading(&rom_buffer);
    defer tmp[0].close();
    defer tmp[1].cleanup();

    // The logo warning is not fatal, so loading should still succeed
    const cartridge = try Cartridge.fromFile(tmp[0]);
    try std.testing.expectEqualStrings("INVALID", cartridge.title);
}

test "Cartridge fromFile rejects unsupported cartridge type" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    craftValidRomBuffer(&rom_buffer, "BADTYPE", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    rom_buffer[0x147] = 0x01; // Unsupported type

    var tmp = try writeBuffAndReturnFileForReading(&rom_buffer);
    defer tmp[0].close();
    defer tmp[1].cleanup();

    try std.testing.expectError(CartridgeHeaderParseError.UnsupportedCartridgeType, Cartridge.fromFile(tmp[0]));
}

test "Cartridge fromFile rejects unsupported rom size" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    craftValidRomBuffer(&rom_buffer, "BADSIZE", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    rom_buffer[0x148] = 0x01; // Unsupported rom size

    var tmp = try writeBuffAndReturnFileForReading(&rom_buffer);
    defer tmp[0].close();
    defer tmp[1].cleanup();

    try std.testing.expectError(CartridgeHeaderParseError.UnsupportedCartridgeType, Cartridge.fromFile(tmp[0]));
}

test "Cartridge fromFile rejects unsupported ram size" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    craftValidRomBuffer(&rom_buffer, "BADRAM", blk: {
        var sum: u8 = 0;
        for (0x34..0x4D) |idx| {
            sum = sum -% rom_buffer[idx] -% 1;
        }
        break :blk sum;
    });

    rom_buffer[0x149] = 0x01; // Unsupported ram size

    var tmp = try writeBuffAndReturnFileForReading(&rom_buffer);
    defer tmp[0].close();
    defer tmp[1].cleanup();

    try std.testing.expectError(CartridgeHeaderParseError.UnsupportedCartridgeType, Cartridge.fromFile(tmp[0]));
}

test "Cartridge fromFile warns on bad header checksum but still loads" {
    var rom_buffer = [_]u8{0} ** 0x8000;

    craftValidRomBuffer(&rom_buffer, "BADCHK", 0x00); // Wrong checksum on purpose

    var tmp = try writeBuffAndReturnFileForReading(&rom_buffer);
    defer tmp[0].close();
    defer tmp[1].cleanup();

    const cartridge = try Cartridge.fromFile(tmp[0]);
    try std.testing.expectEqualStrings("BADCHK", cartridge.title);
}
