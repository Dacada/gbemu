const std = @import("std");

const logger = std.log.scoped(.rom);

const RomHeaderParseError = error{
    NoHeader,
    NoRom,
    UnsupportedCartridgeType,
};

fn strdup(str: []const u8, alloc: std.mem.Allocator) ![]const u8 {
    const dup = try alloc.alloc(u8, str.len);
    std.mem.copyForwards(u8, dup, str);
    return dup;
}

// https://gbdev.io/pandocs/The_Cartridge_Header.html
pub const Rom = struct {
    // DMG ONLY -- We interpret the title simply, however in "newer cartridges" this has a more complicated meaning
    title: []const u8,
    checksum: u8,
    rom: []const u8,

    // TODO: licensee code (old and new), CGB/SGB flag, destination code, version number, global checksum

    pub fn fromBinary(allocator: std.mem.Allocator, program: []const u8, title: []const u8, program_offset: u16) !Rom {
        var rom = try allocator.alloc(u8, 0x8000);
        std.mem.copyForwards(u8, rom[program_offset..], program);

        return Rom{
            .title = try strdup(title, allocator),
            .checksum = 0xFF,
            .rom = rom,
        };
    }

    pub fn fromFile(allocator: std.mem.Allocator, file: std.fs.File) !Rom {
        var buff: [0x50]u8 = undefined;
        const offset: u16 = 0x0100;
        try file.seekTo(offset);
        const readSize = try file.readAll(&buff);
        if (readSize < buff.len) {
            logger.err("could not find a header in the rom: could only read {d} bytes at offset {x}", .{ readSize, offset });
            return RomHeaderParseError.NoHeader;
        }

        // TODO: This seems a bit unnecessary for now. Revisit later.
        // const logo = [_]u8{
        //     0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
        //     0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
        //     0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
        // };
        // if (!std.mem.eql(u8, logo, buff[0x0004..0x0034])) {
        //     logger.warn("could not find expected logo in cartridege header", .{});
        // }

        const title = try Rom.getTitleFromHeader(allocator, &buff);
        errdefer allocator.free(title);

        const rom_type = buff[0x47];
        if (rom_type != 0x00) { // TODO: support other types
            logger.err("unsupported cartridge type: {x}", .{rom_type});
            return RomHeaderParseError.UnsupportedCartridgeType;
        }

        const rom_size_code = buff[0x48];
        if (rom_size_code != 0x00) { // TODO: support other sizes
            logger.err("unsupported rom size code: {x}", .{rom_size_code});
            return RomHeaderParseError.UnsupportedCartridgeType;
        }

        const ram_size_code = buff[0x49];
        if (ram_size_code != 0x00) { // TODO: support other sizes
            logger.err("unsupported ram size code: {x}", .{ram_size_code});
            return RomHeaderParseError.UnsupportedCartridgeType;
        }

        var checksum: u8 = 0;
        for (0x34..0x4D) |idx| {
            checksum = checksum -% buff[idx] -% 1;
        }
        if (checksum != buff[0x4D]) {
            logger.warn("header checksum does not match ({d} vs {d})", .{ checksum, buff[0x4D] });
        }

        const rom = try allocator.alloc(u8, 0x8000);
        errdefer allocator.free(rom);
        try file.seekTo(0x0000);
        const readSizeRom = try file.readAll(rom);
        if (readSizeRom != rom.len) {
            logger.err("unexpected cartridge file size, could only read {d} bytes for rom", .{readSizeRom});
            return RomHeaderParseError.NoRom;
        }

        return Rom{
            .title = title,
            .checksum = checksum,
            .rom = rom,
        };
    }

    fn getTitleFromHeader(allocator: std.mem.Allocator, buff: []const u8) ![]const u8 {
        var len: usize = 0;
        for (buff[0x34..0x44]) |c| {
            if (c == 0x00) {
                break;
            }
            len += 1;
        }
        return strdup(buff[0x34..(0x34 + len)], allocator);
    }

    pub fn deinit(self: *const Rom, allocator: std.mem.Allocator) void {
        allocator.free(self.rom);
        allocator.free(self.title);
    }
};

test "fromBinary" {
    const program = "test program contents";
    const title = "test title";
    const offset: u16 = 0x1234;
    const rom = try Rom.fromBinary(std.testing.allocator, program, title, offset);
    defer rom.deinit(std.testing.allocator);

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
    const logo = [_]u8{
        0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
        0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
        0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
    };
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
    const rom = try Rom.fromFile(std.testing.allocator, file);
    defer rom.deinit(std.testing.allocator);

    try std.testing.expectEqual(checksum, rom.checksum);
    try std.testing.expectEqualSlices(u8, title, rom.title);
    try std.testing.expectEqualSlices(u8, &buff, rom.rom);
}

test "fromFile small" {
    const buff = [_]u8{0xAA} ** 0x10;
    const file, var tmpDir = try writeBuffAndReturnFileForReading(&buff);
    defer tmpDir.cleanup();
    defer file.close();
    const err = Rom.fromFile(std.testing.allocator, file);
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
    const err = Rom.fromFile(std.testing.allocator, file);
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
    const err = Rom.fromFile(std.testing.allocator, file);
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
    const err = Rom.fromFile(std.testing.allocator, file);
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
    const err = Rom.fromFile(std.testing.allocator, file);
    try std.testing.expectError(RomHeaderParseError.NoRom, err);
}
