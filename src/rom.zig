const std = @import("std");
const builtin = @import("builtin");

const logger = if (builtin.is_test)
    struct {
        pub fn err(comptime _: []const u8, _: anytype) void {}
        pub fn warn(comptime _: []const u8, _: anytype) void {}
        pub fn info(comptime _: []const u8, _: anytype) void {}
        pub fn debug(comptime _: []const u8, _: anytype) void {}
    }
else
    std.log.scoped(.rom);

const RomHeaderParseError = error{
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

// https://gbdev.io/pandocs/The_Cartridge_Header.html
pub const Rom = struct {
    // DMG ONLY -- We interpret the title simply, however in "newer cartridges" this has a more complicated meaning
    title: []const u8,
    checksum: u8,
    rom: []const u8,

    // TODO: licensee code (old and new), CGB/SGB flag, destination code, version number, global checksum

    pub fn fromBinary(program: []const u8, title: []const u8, program_offset: u16) !Rom {
        std.mem.copyForwards(u8, STATIC_ROM[program_offset..], program);

        return Rom{
            .title = title,
            .checksum = 0xFF,
            .rom = &STATIC_ROM,
        };
    }

    pub fn fromFile(file: std.fs.File) !Rom {
        const offset = 0x0100;
        const header = STATIC_ROM[offset..(offset + 0x50)];
        try file.seekTo(offset);
        const readSize = try file.readAll(header);
        if (readSize < header.len) {
            logger.err("could not find a header in the rom: could only read 0x{X} bytes at offset 0x{X}", .{ readSize, offset });
            return RomHeaderParseError.NoHeader;
        }

        if (!std.mem.eql(u8, &logo, header[0x0004..0x0034])) {
            logger.warn("could not find expected logo in cartridege header", .{});
        }

        const title = try Rom.getTitleFromHeader(header);

        const rom_type = header[0x47];
        if (rom_type != 0x00) { // TODO: support other types
            logger.err("unsupported cartridge type: 0x{X}", .{rom_type});
            return RomHeaderParseError.UnsupportedCartridgeType;
        }

        const rom_size_code = header[0x48];
        if (rom_size_code != 0x00) { // TODO: support other sizes
            logger.err("unsupported rom size code: 0x{X}", .{rom_size_code});
            return RomHeaderParseError.UnsupportedCartridgeType;
        }

        const ram_size_code = header[0x49];
        if (ram_size_code != 0x00) { // TODO: support other sizes
            logger.err("unsupported ram size code: 0x{X}", .{ram_size_code});
            return RomHeaderParseError.UnsupportedCartridgeType;
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
            return RomHeaderParseError.NoRom;
        }

        return Rom{
            .title = title,
            .checksum = checksum,
            .rom = &STATIC_ROM,
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

test "fromBinary" {
    const program = "test program contents";
    const title = "test title";
    const offset: u16 = 0x1234;
    const rom = try Rom.fromBinary(program, title, offset);

    try std.testing.expectEqualSlices(u8, title, rom.title);
    try std.testing.expectEqual(0xFF, rom.checksum);
    try std.testing.expectEqualSlices(u8, program, rom.rom[offset..(offset + program.len)]);
}

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

test "fromFile" {
    const title = "test title";
    const checksum: u8 = 0xE9;
    var buff = [_]u8{0xAA} ** 0x8000;
    craftValidRomBuffer(&buff, title, checksum);

    const file, var tmpDir = try writeBuffAndReturnFileForReading(&buff);
    defer tmpDir.cleanup();
    defer file.close();
    const rom = try Rom.fromFile(file);

    try std.testing.expectEqual(checksum, rom.checksum);
    try std.testing.expectEqualSlices(u8, title, rom.title);
    try std.testing.expectEqualSlices(u8, &buff, rom.rom);
}

test "fromFile small" {
    const buff = [_]u8{0xAA} ** 0x10;
    const file, var tmpDir = try writeBuffAndReturnFileForReading(&buff);
    defer tmpDir.cleanup();
    defer file.close();
    const err = Rom.fromFile(file);
    try std.testing.expectError(RomHeaderParseError.NoHeader, err);
}

test "fromFile badRomType" {
    const title = "test title";
    const checksum: u8 = 0xFF;
    var buff = [_]u8{0xAA} ** 0x8000;
    craftValidRomBuffer(&buff, title, checksum);
    buff[0x147] = 0x01;

    const file, var tmpDir = try writeBuffAndReturnFileForReading(&buff);
    defer tmpDir.cleanup();
    defer file.close();
    const err = Rom.fromFile(file);
    try std.testing.expectError(RomHeaderParseError.UnsupportedCartridgeType, err);
}

test "fromFile badRomSize" {
    const title = "test title";
    const checksum: u8 = 0xFF;
    var buff = [_]u8{0xAA} ** 0x8000;
    craftValidRomBuffer(&buff, title, checksum);
    buff[0x148] = 0x01;

    const file, var tmpDir = try writeBuffAndReturnFileForReading(&buff);
    defer tmpDir.cleanup();
    defer file.close();
    const err = Rom.fromFile(file);
    try std.testing.expectError(RomHeaderParseError.UnsupportedCartridgeType, err);
}

test "fromFile badRamSize" {
    const title = "test title";
    const checksum: u8 = 0xFF;
    var buff = [_]u8{0xAA} ** 0x8000;
    craftValidRomBuffer(&buff, title, checksum);
    buff[0x149] = 0x01;

    const file, var tmpDir = try writeBuffAndReturnFileForReading(&buff);
    defer tmpDir.cleanup();
    defer file.close();
    const err = Rom.fromFile(file);
    try std.testing.expectError(RomHeaderParseError.UnsupportedCartridgeType, err);
}

test "fromFile smallish" {
    const title = "test title";
    const checksum: u8 = 0xFF;
    var buff = [_]u8{0xAA} ** 0x4000;
    craftValidRomBuffer(&buff, title, checksum);

    const file, var tmpDir = try writeBuffAndReturnFileForReading(&buff);
    defer tmpDir.cleanup();
    defer file.close();
    const err = Rom.fromFile(file);
    try std.testing.expectError(RomHeaderParseError.NoRom, err);
}
