const std = @import("std");

pub const MmuMemoryError = error{
    WriteToReadOnlyMemory,
    AccessToForbiddenMemory,
};

pub const MmuSetupError = error{
    UnexpectedMemoryCapacity,
} || std.mem.Allocator.Error;

pub const Mmu = struct {
    rom: []const u8,
    vram: []u8,
    exram: []u8,
    wram: []u8,
    oam: []u8,
    io: []u8,
    hram: []u8,
    ie: u8,

    allocator: std.mem.Allocator,

    pub fn init(rom: []const u8, exram: []u8, allocator: std.mem.Allocator) MmuSetupError!Mmu {
        if (rom.len != 0x8000) {
            return MmuSetupError.UnexpectedMemoryCapacity;
        }
        if (exram.len != 0xC000 - 0xA000) {
            return MmuSetupError.UnexpectedMemoryCapacity;
        }
        return Mmu{
            .rom = rom,
            .vram = try allocator.alloc(u8, 0xA000 - 0x8000),
            .exram = exram,
            .wram = try allocator.alloc(u8, 0xE000 - 0xC000),
            .oam = try allocator.alloc(u8, 0xFEA0 - 0xFE00),
            .io = try allocator.alloc(u8, 0xFF80 - 0xFF00),
            .hram = try allocator.alloc(u8, 0xFFFF - 0xFF80),
            .ie = undefined,
            .allocator = allocator,
        };
    }

    pub fn zeroize(self: *Mmu) void {
        @memset(self.vram, 0);
        @memset(self.wram, 0);
        @memset(self.oam, 0);
        @memset(self.io, 0);
        @memset(self.hram, 0);
        self.ie = 0;
    }

    pub fn deinit(self: *const Mmu) void {
        self.allocator.free(self.vram);
        self.allocator.free(self.wram);
        self.allocator.free(self.oam);
        self.allocator.free(self.io);
        self.allocator.free(self.hram);
    }

    fn generateReference(comptime is_const: bool) fn (if (is_const) *const Mmu else *Mmu, u16) if (is_const) MmuMemoryError!*const u8 else MmuMemoryError!*u8 {
        return struct {
            fn reference(self: if (is_const) *const Mmu else *Mmu, addr: u16) if (is_const) MmuMemoryError!*const u8 else MmuMemoryError!*u8 {
                if (addr < 0x8000) {
                    if (is_const) {
                        return &self.rom[addr];
                    } else {
                        return MmuMemoryError.WriteToReadOnlyMemory;
                    }
                }
                if (addr < 0xA000) {
                    return &self.vram[addr - 0x8000];
                }
                if (addr < 0xC000) {
                    return &self.exram[addr - 0xA000];
                }
                if (addr < 0xE000) {
                    return &self.wram[addr - 0xC000];
                }
                if (addr < 0xFE00) {
                    return MmuMemoryError.AccessToForbiddenMemory;
                }
                if (addr < 0xFEA0) {
                    return &self.oam[addr - 0xFE00];
                }
                if (addr < 0xFF00) {
                    return MmuMemoryError.AccessToForbiddenMemory;
                }
                if (addr < 0xFF80) {
                    return &self.io[addr - 0xFF00];
                }
                if (addr < 0xFFFF) {
                    return &self.hram[addr - 0xFF80];
                }
                return &self.ie;
            }
        }.reference;
    }

    pub const referenceRW = Mmu.generateReference(false);
    pub const referenceRO = Mmu.generateReference(true);

    pub fn write(self: *Mmu, addr: u16, val: u8) MmuMemoryError!void {
        (try self.referenceRW(addr)).* = val;
    }

    pub fn read(self: *const Mmu, addr: u16) MmuMemoryError!u8 {
        return (try self.referenceRO(addr)).*;
    }
};

test "mmu bad init 1" {
    const rom = try std.testing.allocator.alloc(u8, 1);
    defer std.testing.allocator.free(rom);
    const exram = try std.testing.allocator.alloc(u8, 1);
    defer std.testing.allocator.free(exram);
    try std.testing.expectError(MmuSetupError.UnexpectedMemoryCapacity, Mmu.init(rom, exram, std.testing.allocator));
}

test "mmu bad init 2" {
    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);
    const exram = try std.testing.allocator.alloc(u8, 1);
    defer std.testing.allocator.free(exram);
    try std.testing.expectError(MmuSetupError.UnexpectedMemoryCapacity, Mmu.init(rom, exram, std.testing.allocator));
}

test "mmu" {
    var rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);
    var mmu = try Mmu.init(rom, exram, std.testing.allocator);
    defer mmu.deinit();

    // Set up ROM
    for (0..0x8000) |addr| {
        const val: u8 = @intCast(addr & 0xFF);
        rom[addr] = val;
    }

    // Test writing
    for (0..(0xFFFF + 1)) |addr| {
        const val: u8 = @intCast(addr & 0xFF);
        if (addr <= 0x7FFF) {
            try std.testing.expectError(MmuMemoryError.WriteToReadOnlyMemory, mmu.write(@intCast(addr), val));
        } else if ((addr >= 0xE000 and addr <= 0xFDFF) or (addr >= 0xFEA0 and addr <= 0xFEFF)) {
            try std.testing.expectError(MmuMemoryError.AccessToForbiddenMemory, mmu.write(@intCast(addr), val));
        } else {
            try mmu.write(@intCast(addr), val);
        }
    }

    // Test reading
    for (0..(0xFFFF + 1)) |addr| {
        const val: u8 = @intCast(addr & 0xFF);
        if ((addr >= 0xE000 and addr <= 0xFDFF) or (addr >= 0xFEA0 and addr <= 0xFEFF)) {
            try std.testing.expectError(MmuMemoryError.AccessToForbiddenMemory, mmu.write(@intCast(addr), val));
        } else {
            try std.testing.expectEqual(val, try mmu.read(@intCast(addr)));
        }
    }
}

test "zeroize mmu owned memory" {
    const rom = try std.testing.allocator.alloc(u8, 0x8000);
    defer std.testing.allocator.free(rom);
    const exram = try std.testing.allocator.alloc(u8, 0x2000);
    defer std.testing.allocator.free(exram);
    var mmu = try Mmu.init(rom, exram, std.testing.allocator);
    defer mmu.deinit();
    mmu.zeroize();
    inline for (.{ mmu.vram, mmu.wram, mmu.oam, mmu.io, mmu.hram }) |slice| {
        for (slice) |cell| {
            try std.testing.expectEqual(0, cell);
        }
    }
}
