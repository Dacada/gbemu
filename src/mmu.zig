const std = @import("std");

const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;

const logger = std.log.scoped(.mmu);

// TODO: May need to make this non global
// DMG ONLY -- In CGB, the second half is a switchable bank
var STATIC_WRAM = [_]u8{0x00} ** 0x2000;
var STATIC_INIT_WRAM = [_]bool{false} ** 0x2000;

var STATIC_HRAM = [_]u8{0x00} ** 0x7F;
var STATIC_INIT_HRAM = [_]bool{false} ** 0x7F;

pub fn Mmu(Cartridge: type, Ppu: type, Mmio: type) type {
    return struct {
        const This = @This();

        const Wram = struct {
            fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = STATIC_WRAM[addr];
                const flags = MemoryFlag{ .uninitialized = !STATIC_INIT_WRAM[addr] };
                return .{ flags, val };
            }

            fn write(_: *This, addr: u16, val: u8) MemoryFlag {
                STATIC_WRAM[addr] = val;
                STATIC_INIT_WRAM[addr] = true;
                return .{};
            }

            fn peek(_: *This, addr: u16) u8 {
                return STATIC_WRAM[addr];
            }
            fn poke(_: *This, addr: u16, val: u8) void {
                STATIC_WRAM[addr] = val;
            }
        };

        const Hram = struct {
            fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = STATIC_HRAM[addr];
                const flags = MemoryFlag{ .uninitialized = !STATIC_INIT_HRAM[addr] };
                return .{ flags, val };
            }
            fn write(_: *This, addr: u16, val: u8) MemoryFlag {
                STATIC_HRAM[addr] = val;
                STATIC_INIT_HRAM[addr] = true;
                return .{};
            }
            fn peek(_: *This, addr: u16) u8 {
                return STATIC_HRAM[addr];
            }
            fn poke(_: *This, addr: u16, val: u8) void {
                STATIC_HRAM[addr] = val;
            }
        };

        cart: *Cartridge,
        ppu: *Ppu,
        mmio: *Mmio,

        flags: MemoryFlag,

        const Operation = enum {
            read,
            write,
            peek,
            poke,
        };

        const Target = enum {
            cart_rom,
            cart_ram,
            vram,
            oam,
            forbidden,
            mmio,
            wram,
            hram,
        };

        pub inline fn init(cart: *Cartridge, ppu: *Ppu, mmio: *Mmio) This {
            return This{
                .cart = cart,
                .ppu = ppu,
                .mmio = mmio,
                .flags = .{},
            };
        }

        inline fn decode(addr: u16) struct { u16, Target } {
            return switch (addr) {
                0x0000...0x7FFF => .{ addr, .cart_rom },
                0x8000...0x9FFF => .{ addr - 0x8000, .vram },
                0xA000...0xBFFF => .{ addr - 0xA000, .cart_ram },
                0xC000...0xDFFF => .{ addr - 0xC000, .wram },
                0xE000...0xFDFF => .{ addr - 0xE000, .wram }, // Echo RAM
                0xFE00...0xFE9F => .{ addr - 0xFE00, .oam },
                0xFEA0...0xFEFF => .{ addr - 0xFEA0, .forbidden },
                0xFF00...0xFF7F => .{ addr - 0xFF00, .mmio },
                0xFF80...0xFFFE => .{ addr - 0xFF80, .hram },
                else => .{ 0x80, .mmio }, // IE is handled by mmio module
            };
        }

        inline fn dispatch(self: *This, addr: u16, comptime operation: Operation, value: u8) u8 {
            const resolved_address, const target = decode(addr);
            return switch (target) {
                .cart_rom => self.make_call(self.cart, Cartridge.Rom, operation, resolved_address, value),
                .cart_ram => self.make_call(self.cart, Cartridge.Ram, operation, resolved_address, value),
                .vram => self.make_call(self.ppu, Ppu.Vram, operation, resolved_address, value),
                .oam => self.make_call(self.ppu, Ppu.Oam, operation, resolved_address, value),
                .forbidden => self.make_call(self.ppu, Ppu.Forbidden, operation, resolved_address, value),
                .mmio => self.make_call(self.mmio, Mmio, operation, resolved_address, value),
                .wram => self.make_call(self, This.Wram, operation, resolved_address, value),
                .hram => self.make_call(self, This.Hram, operation, resolved_address, value),
            };
        }

        inline fn make_call(self: *This, instance: anytype, comptime namespace: type, comptime operation: Operation, addr: u16, value: u8) u8 {
            const opname = @tagName(operation);
            switch (operation) {
                .peek => return @field(namespace, opname)(instance, addr),
                .poke => {
                    @field(namespace, opname)(instance, addr, value);
                    return undefined;
                },
                .read => {
                    self.flags, const res = @field(namespace, opname)(instance, addr);
                    return res;
                },
                .write => {
                    self.flags = @field(namespace, opname)(instance, addr, value);
                    return undefined;
                },
            }
        }

        pub fn peek(self: *This, addr: u16) u8 {
            return self.dispatch(addr, .peek, undefined);
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            _ = self.dispatch(addr, .poke, val);
        }

        pub fn read(self: *This, addr: u16) u8 {
            const ret = self.dispatch(addr, .read, undefined);
            if (self.flags.illegal) {
                logger.warn("Illegal read from address 0x{X:0>4}", .{addr});
            }
            if (self.flags.uninitialized) {
                logger.warn("Uninitialized read from address 0x{X:0>4}", .{addr});
            }
            return ret;
        }

        pub fn write(self: *This, addr: u16, val: u8) void {
            _ = self.dispatch(addr, .write, val);
            if (self.flags.illegal) {
                logger.warn("Illegal write of 0x{X:0>2} to address 0x{X:0>4}", .{ val, addr });
            }
        }

        pub fn dumpMemory(self: *This, start: u16, buff: []u8) void {
            for (0..buff.len) |i| {
                buff[i] = self.peek(start + @as(u16, @intCast(i)));
            }
        }
    };
}

pub const MockMmu = struct {
    // TODO: might need to make this non global
    pub var backingArray = [_]u8{0x00} ** 0x10000;

    flags: MemoryFlag = .{},

    pub inline fn peek(_: *MockMmu, addr: u16) u8 {
        return backingArray[addr];
    }

    pub inline fn poke(_: *MockMmu, addr: u16, val: u8) void {
        backingArray[addr] = val;
    }

    pub inline fn read(self: *MockMmu, addr: u16) u8 {
        return self.peek(addr);
    }

    pub inline fn write(self: *MockMmu, addr: u16, val: u8) void {
        return self.poke(addr, val);
    }
};

/// To emulate any memory region where we don't care beyond being able to retrieve the last write
pub const MockMemory = struct {
    lastWrite: u8 = 0x00,

    fn read(self: anytype, _: u16) struct { MemoryFlag, u8 } {
        return .{ .{}, self.lastWrite };
    }

    fn write(self: anytype, _: u16, val: u8) MemoryFlag {
        self.lastWrite = val;
        return .{};
    }

    fn peek(self: anytype, addr: u16) u8 {
        _, const res = read(self, addr);
        return res;
    }

    fn poke(self: anytype, addr: u16, val: u8) void {
        _ = write(self, addr, val);
    }
};

const MockCartridge = struct {
    lastWrite: u8 = 0x00,

    const Rom = MockMemory;
    const Ram = MockMemory;
};

const MockPpu = struct {
    lastWrite: u8 = 0x00,

    const Vram = MockMemory;
    const Oam = MockMemory;
    const Forbidden = MockMemory;
};

const MockedMmu = Mmu(MockCartridge, MockPpu, MockMemory);

test "Mmu: write and read to all mapped memory regions" {
    var cart = MockCartridge{};
    var ppu = MockPpu{};
    var mmio = MockMemory{};
    var mmu = MockedMmu.init(&cart, &ppu, &mmio);

    for (0..0x10000) |addr| {
        const val = 0xAB;
        mmu.write(@intCast(addr), val);
        const res = mmu.read(@intCast(addr));
        try std.testing.expectEqual(val, res);
        try std.testing.expect(!mmu.flags.illegal);
    }
}
