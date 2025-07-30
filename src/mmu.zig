const std = @import("std");

const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;
const router = @import("router.zig");
const GenericRouter = router.Router;
const GenericRange = router.Range;
const GenericTargetField = router.TargetField;

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
            pub fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = STATIC_WRAM[addr];
                const flags = MemoryFlag{ .uninitialized = !STATIC_INIT_WRAM[addr] };
                return .{ flags, val };
            }

            pub fn write(_: *This, addr: u16, val: u8) MemoryFlag {
                STATIC_WRAM[addr] = val;
                STATIC_INIT_WRAM[addr] = true;
                return .{};
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return STATIC_WRAM[addr];
            }
            pub fn poke(_: *This, addr: u16, val: u8) void {
                STATIC_WRAM[addr] = val;
            }
        };

        const Hram = struct {
            pub fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = STATIC_HRAM[addr];
                const flags = MemoryFlag{ .uninitialized = !STATIC_INIT_HRAM[addr] };
                return .{ flags, val };
            }
            pub fn write(_: *This, addr: u16, val: u8) MemoryFlag {
                STATIC_HRAM[addr] = val;
                STATIC_INIT_HRAM[addr] = true;
                return .{};
            }
            pub fn peek(_: *This, addr: u16) u8 {
                return STATIC_HRAM[addr];
            }
            pub fn poke(_: *This, addr: u16, val: u8) void {
                STATIC_HRAM[addr] = val;
            }
        };

        cart: *Cartridge,
        ppu: *Ppu,
        mmio: *Mmio,

        flags: MemoryFlag,

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

        const Range = GenericRange(Target);
        const TargetField = GenericTargetField(Target);
        const Router = GenericRouter(
            Target,
            &[_]Range{
                .{
                    .start = 0x0000,
                    .end = 0x7FFF,
                    .target = .cart_rom,
                },
                .{
                    .start = 0x8000,
                    .end = 0x9FFF,
                    .target = .vram,
                },
                .{
                    .start = 0xA000,
                    .end = 0xBFFF,
                    .target = .cart_ram,
                },
                .{
                    .start = 0xC000,
                    .end = 0xDFFF,
                    .target = .wram,
                },
                .{
                    .start = 0xE000,
                    .end = 0xFDFF,
                    .target = .wram,
                },
                .{
                    .start = 0xFE00,
                    .end = 0xFE9F,
                    .target = .oam,
                },
                .{
                    .start = 0xFEA0,
                    .end = 0xFEFF,
                    .target = .forbidden,
                },
                .{
                    .start = 0xFF00,
                    .end = 0xFF7F,
                    .target = .mmio,
                },
                .{
                    .start = 0xFF80,
                    .end = 0xFFFE,
                    .target = .hram,
                },
            },
            .{
                .addr = 0x80,
                .target = .mmio,
            },
            &[_]TargetField{
                .{
                    .target = .cart_rom,
                    .field = "cart",
                    .namespace = Cartridge.Rom,
                },
                .{
                    .target = .cart_ram,
                    .field = "cart",
                    .namespace = Cartridge.Ram,
                },
                .{
                    .target = .vram,
                    .field = "ppu",
                    .namespace = Ppu.Vram,
                },
                .{
                    .target = .oam,
                    .field = "ppu",
                    .namespace = Ppu.Oam,
                },
                .{
                    .target = .forbidden,
                    .field = "ppu",
                    .namespace = Ppu.Forbidden,
                },
                .{
                    .target = .mmio,
                    .field = "mmio",
                    .namespace = Mmio,
                },
                .{
                    .target = .wram,
                    .field = null,
                    .namespace = This.Wram,
                },
                .{
                    .target = .hram,
                    .field = null,
                    .namespace = This.Hram,
                },
            },
        );

        pub inline fn init(cart: *Cartridge, ppu: *Ppu, mmio: *Mmio) This {
            return This{
                .cart = cart,
                .ppu = ppu,
                .mmio = mmio,
                .flags = .{},
            };
        }

        pub fn peek(self: *This, addr: u16) u8 {
            _, const ret = Router.dispatch(self, addr, .peek, undefined);
            return ret;
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            _ = Router.dispatch(self, addr, .poke, val);
        }

        pub fn read(self: *This, addr: u16) u8 {
            const flags, const ret = Router.dispatch(self, addr, .read, undefined);
            self.flags = flags;

            if (self.flags.illegal) {
                logger.warn("Illegal read from address 0x{X:0>4}", .{addr});
            }
            if (self.flags.uninitialized) {
                logger.warn("Uninitialized read from address 0x{X:0>4}", .{addr});
            }

            return ret;
        }

        pub fn write(self: *This, addr: u16, val: u8) void {
            const flags, _ = Router.dispatch(self, addr, .write, val);
            self.flags = flags;

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

    pub fn read(self: anytype, _: u16) struct { MemoryFlag, u8 } {
        return .{ .{}, self.lastWrite };
    }

    pub fn write(self: anytype, _: u16, val: u8) MemoryFlag {
        self.lastWrite = val;
        return .{};
    }

    pub fn peek(self: anytype, addr: u16) u8 {
        _, const res = read(self, addr);
        return res;
    }

    pub fn poke(self: anytype, addr: u16, val: u8) void {
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
