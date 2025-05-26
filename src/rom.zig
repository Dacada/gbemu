const std = @import("std");

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

    // TODO: licensee code (old and new), SGB flag, mapper (we are asserting it's 0x00 for now), rom size (asserting no banking), ram size (asserting no ram), destination code, version number, global checksum

    pub fn fromBinary(allocator: std.mem.Allocator, program: []const u8, title: []const u8, program_offset: u16) !Rom {
        var rom = try allocator.alloc(u8, 0x8000);
        std.mem.copyForwards(u8, rom[program_offset..], program);

        return Rom{
            .title = try strdup(title, allocator),
            .checksum = 0xFF,
            .rom = rom,
        };
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
