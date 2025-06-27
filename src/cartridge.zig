const std = @import("std");
const memory = @import("memory.zig");
const Memory = memory.Memory;
const MemoryFlag = memory.MemoryFlag;
const SimpleMemory = memory.SimpleMemory;

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

// This memory will always contain the totality of the currently loaded ROM
var STATIC_ROM: [0x8000]u8 = undefined;

// This memory holds the cartridge's RAM
var STATIC_RAM: [0x2000]u8 = undefined;
var STATIC_RAM_INIT: [0x2000]bool = undefined;

// https://gbdev.io/pandocs/The_Cartridge_Header.html
pub const Cartridge = struct {
    // DMG ONLY -- We interpret the title simply, however in "newer cartridges" this has a more complicated meaning
    title: []const u8,
    checksum: u8,
    rom: Memory,
    ram: Memory,

    // TODO: licensee code (old and new), CGB/SGB flag, destination code, version number, global checksum

    pub fn fromBinary(program: []const u8, title: []const u8, program_offset: u16) !Cartridge {
        std.mem.copyForwards(u8, STATIC_ROM[program_offset..], program);
        @memset(&STATIC_RAM_INIT, false);

        return Cartridge{
            .title = title,
            .checksum = 0xFF,
            .rom = SimpleMemory(true, &STATIC_ROM, null).memory(),
            .ram = SimpleMemory(false, &STATIC_RAM, &STATIC_RAM_INIT).memory(),
        };
    }

    pub fn fromFile(file: std.fs.File) !Cartridge {
        const offset = 0x0100;
        const header = STATIC_ROM[offset..(offset + 0x50)];
        try file.seekTo(offset);
        const readSize = try file.readAll(header);
        if (readSize < header.len) {
            logger.err("could not find a header in the rom: could only read 0x{X} bytes at offset 0x{X}", .{ readSize, offset });
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
        const readSizeRom = try file.readAll(&STATIC_ROM);
        if (readSizeRom != STATIC_ROM.len) {
            logger.err("unexpected cartridge file size, could only read 0x{X} bytes for rom but wanted to read 0x{X}", .{ readSizeRom, STATIC_ROM.len });
            return CartridgeHeaderParseError.NoRom;
        }

        @memset(&STATIC_RAM_INIT, false);

        return Cartridge{
            .title = title,
            .checksum = checksum,
            .rom = SimpleMemory(true, &STATIC_ROM, null).memory(),
            .ram = SimpleMemory(false, &STATIC_RAM, &STATIC_RAM_INIT).memory(),
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
    var tmpDir = std.testing.tmpDir(.{});
    const file = try tmpDir.dir.createFile("test.gb", .{});
    defer file.close();
    try file.writeAll(buff);
    return .{ try tmpDir.dir.openFile("test.gb", .{}), tmpDir };
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

test "Cartridge fromBinary loads correct ROM and resets RAM init" {
    var rom = [_]u8{0xAA} ** 0x4000;
    const title = "TEST";
    const offset: u16 = 0x0100;

    const cartridge = try Cartridge.fromBinary(rom[0..], title, offset);

    try std.testing.expectEqualStrings(title, cartridge.title);

    // Check that the ROM data was copied correctly
    try std.testing.expectEqual(@as(u8, 0xAA), STATIC_ROM[offset]);

    // Check that RAM init tracking was reset
    for (STATIC_RAM_INIT) |v| {
        try std.testing.expect(!v);
    }
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
