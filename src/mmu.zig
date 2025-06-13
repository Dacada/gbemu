const std = @import("std");
const builtin = @import("builtin");
const MemoryReference = @import("reference.zig").MemoryReference;
const Cartridge = @import("cartridge.zig").Cartridge;
const Ppu = @import("ppu.zig").Ppu;
const Mmio = @import("mmio.zig").Mmio;

const logger = std.log.scoped(.mmu);

// DMG ONLY -- In CGB, the second half is a switchable bank
var STATIC_WRAM = [_]u8{0x00} ** 0x2000;
var STATIC_INIT_WRAM = [_]bool{false} ** 0x2000;

var STATIC_HRAM = [_]u8{0x00} ** 0x79;
var STATIC_INIT_HRAM = [_]bool{false} ** 0x79;

const MemoryArea = enum {
    cartridge_rom,
    video_ram,
    cartridge_ram,
    work_ram,
    echo,
    oam,
    forbidden,
    mmio,
    high_ram,
    ie,

    fn from_address(addr: u16) struct { MemoryArea, u16 } {
        const map = [_]struct { MemoryArea, u16 }{
            .{ .cartridge_rom, 0x8000 },
            .{ .video_ram, 0xA000 },
            .{ .cartridge_ram, 0xC000 },
            .{ .work_ram, 0xE000 },
            .{ .echo, 0xFE00 },
            .{ .oam, 0xFEA0 },
            .{ .forbidden, 0xFF80 },
            .{ .mmio, 0x8000 },
            .{ .high_ram, 0xFFFF },
        };

        var prev_addr: u16 = 0;
        inline for (0..map.len) |i| {
            const area, const max_addr = map[i];
            if (addr < max_addr) {
                return .{ area, addr - prev_addr };
            }
            prev_addr = max_addr;
        }
        return .{ .ie, 0 };
    }
};

pub const Mmu = struct {
    cartridge: ?*Cartridge,
    ppu: *Ppu,
    mmio: *Mmio,

    wram: []u8,
    init_wram: []bool,

    hram: []u8,
    init_hram: []bool,

    ie: u8,

    _illegalMemoryOperationHappened: bool = false,
    _readFromUninitializedMemoryHappened: bool = false,

    pub fn init(ppu: *Ppu, mmio: *Mmio) Mmu {
        return Mmu{
            .cartridge = null,
            .ppu = ppu,
            .mmio = mmio,
            .wram = &STATIC_WRAM,
            .init_wram = &STATIC_INIT_WRAM,
            .hram = &STATIC_HRAM,
            .init_hram = &STATIC_INIT_HRAM,
            .ie = 0x00,
        };
    }

    /// For testing, set everything to 0 so we can dump memory and compare against slices
    pub fn zeroize(self: *Mmu) void {
        self.rom = null;
        self.ppu.zeroize();
        self.mmio.zeroize();
        @memset(self.wram, 0);
        @memset(self.hram, 0);
        self.ie = 0;
    }

    pub fn delayedReference(self: *Mmu, addr: u16) MemoryReference {
        return MemoryReference.fromMmu(self, addr);
    }

    pub fn setCartridge(self: *Mmu, cartridge: *Cartridge) void {
        self.cartridge = cartridge;
    }

    pub fn dumpMemory(self: *const Mmu, addr: u16, buffer: []u8) void {
        for (0..buffer.len) |i| {
            buffer[i] = self.getValue(addr + @as(u16, @intCast(i)));
        }
    }

    pub fn write(self: *Mmu, addr: u16, val: u8) void {
        const addr_type, const normalized = MemoryArea.from_address(addr);
        switch (addr_type) {
            .cartridge_rom => if (self.cartridge) |c| c.write_rom(normalized, val, &self._illegalMemoryOperationHappened),
            .video_ram => self.ppu.write_video(normalized, val, &self._illegalMemoryOperationHappened),
            .cartridge_ram => if (self.cartridge) |c| c.write_ram(normalized, val, &self._illegalMemoryOperationHappened),
            .work_ram => {
                self.wram[normalized] = val;
                self.init_wram[normalized] = true;
            },
            .echo => {
                self._illegalMemoryOperationHappened = true;
                logger.warn("Write to echo RAM (addr=0x{X:0>4} val=0x{X:0>2})", .{ addr, val });
                self.write(addr - 0x2000, val);
            },
            .oam => self.ppu.write_oam(normalized, val, &self._illegalMemoryOperationHappened),
            .forbidden => {
                // Documentation says nothing about writes to this area, just reads https://gbdev.io/pandocs/Memory_Map.html#fea0feff-range
                // So we just ignore it.
                self._illegalMemoryOperationHappened = true;
            },
            .mmio => self.mmio.write(normalized, val, &self._illegalMemoryOperationHappened),
            .high_ram => {
                self.hram[normalized] = val;
                self.init_hram[normalized] = true;
            },
            .ie => self.ie = val,
        }
    }

    pub fn read(self: *Mmu, addr: u16) u8 {
        const addr_type, const normalized = MemoryArea.from_address(addr);
        return switch (addr_type) {
            .cartridge_rom => if (self.cartridge) |c| c.read_rom(normalized, &self._illegalMemoryOperationHappened) else 0x00,
            .video_ram => self.ppu.read_video(normalized, &self._illegalMemoryOperationHappened, &self._readFromUninitializedMemoryHappened),
            .cartridge_ram => if (self.cartridge) |c| c.read_ram(normalized, &self._illegalMemoryOperationHappened, &self._readFromUninitializedMemoryHappened) else 0x00,
            .work_ram => blk: {
                if (!self.init_wram[normalized]) {
                    self._readFromUninitializedMemoryHappened = true;
                    logger.warn("Read from uninitialized WRAM (addr=0x{X:0>4})", .{addr});
                }
                break :blk self.wram[normalized];
            },
            .echo => blk: {
                self._illegalMemoryOperationHappened = true;
                logger.warn("Read from echo RAM (addr=0x{X:0>4})", .{addr});
                break :blk self.read(addr - 0x2000);
            },
            .oam => self.ppu.read_oam(normalized, &self._illegalMemoryOperationHappened, &self._readFromUninitializedMemoryHappened),
            .forbidden => blk: {
                if (self.ppu.oam_blocked()) {
                    break :blk 0xFF;
                }

                // TODO: pending oam corruption emulation both here and possibly in cpu
                // DMG ONLY -- Different behaviors depending on revision
                return 0x00;
            },
            .mmio => self.mmio.read(normalized, &self._illegalMemoryOperationHappened, &self._readFromUninitializedMemoryHappened),
            .high_ram => blk: {
                if (!self.init_hram[normalized]) {
                    self._readFromUninitializedMemoryHappened = true;
                    logger.warn("Read from uninitialized HRAM (addr=0x{X:0>4})", .{addr});
                }
                break :blk self.hram[normalized];
            },
            .ie => self.ie,
        };
    }

    /// Write without side effects. No flags are set, no peripheral behavior is triggered. For debugging and testing.
    pub fn setValue(self: *Mmu, addr: u16, val: u8) void {
        const addr_type, const normalized = MemoryArea.from_address(addr);
        switch (addr_type) {
            .cartridge_rom => if (self.cartridge) |c| {
                c.setValue_rom(normalized, val);
            } else {
                logger.err("No cartridge loaded. Write ignored!", .{});
            },
            .video_ram => self.ppu.setValue_video(normalized, val),
            .cartridge_ram => if (self.cartridge) |c| {
                c.setValue_ram(normalized, val);
            } else {
                logger.err("No cartridge loaded. Write ignored!", .{});
            },
            .work_ram => self.wram[normalized] = val,
            .echo => self.setValue(addr - 0x2000, val),
            .oam => self.ppu.setValue_oam(normalized, val),
            .forbidden => {
                logger.err("Write to forbidden memory ignored!", .{});
            },
            .mmio => self.mmio.setValue(normalized, val),
            .high_ram => self.hram[normalized] = val,
            .ie => self.ie = val,
        }
    }

    /// Read without side effects. No flags are set, no peripheral behavior is triggered. For debugging and testing.
    pub fn getValue(self: *const Mmu, addr: u16) u8 {
        const addr_type, const normalized = MemoryArea.from_address(addr);
        return switch (addr_type) {
            .cartridge_rom => if (self.cartridge) |c| blk: {
                break :blk c.getValue_rom(normalized);
            } else blk: {
                logger.err("No cartridge loaded. Read returns default value!", .{});
                break :blk 0x00;
            },
            .video_ram => self.ppu.getValue_video(normalized),
            .cartridge_ram => if (self.cartridge) |c| blk: {
                break :blk c.getValue_ram(normalized);
            } else blk: {
                logger.err("No cartridge loaded. Read returns default value!", .{});
                break :blk 0x00;
            },
            .work_ram => self.wram[normalized],
            .echo => self.getValue(addr - 0x2000),
            .oam => self.ppu.getValue_oam(normalized),
            .forbidden => blk: {
                logger.err("Read from forbidden memory returns default value!", .{});
                break :blk 0x00;
            },
            .mmio => self.mmio.getValue(normalized),
            .high_ram => self.hram[normalized],
            .ie => self.ie,
        };
    }

    pub fn illegalMemoryOperationHappened(self: *const Mmu) bool {
        return self._illegalMemoryOperationHappened;
    }

    pub fn clearIllegalOperation(self: *Mmu) void {
        self._illegalMemoryOperationHappened = false;
    }
};
