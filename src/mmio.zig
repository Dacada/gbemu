const std = @import("std");

const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;

const logger = std.log.scoped(.mmio);

pub fn Mmio(Joypad: type, Serial: type, Timer: type, Interrupt: type, Audio: type, Wave: type, Lcd: type, BootRom: type) type {
    return struct {
        const This = @This();

        joypad: *Joypad,
        serial: *Serial,
        timer: *Timer,
        interrupt: *Interrupt,
        audio: *Audio,
        wave: *Wave,
        lcd: *Lcd,
        boot_rom: *BootRom,

        const Invalid = struct {
            fn read(_: *This, _: u16) struct { MemoryFlag, u8 } {
                return .{ .{ .illegal = true }, 0xFF };
            }

            fn write(_: *This, _: u16, _: u8) MemoryFlag {
                return .{ .illegal = true };
            }

            fn peek(_: *This, _: u16) u8 {
                return 0xFF;
            }
            fn poke(_: *This, _: u16, _: u8) void {}
        };

        const Operation = enum {
            read,
            write,
            peek,
            poke,
        };

        const Target = enum {
            joypad,
            serial,
            timer,
            interrupt,
            audio,
            wave,
            lcd,
            boot_rom,
            invalid,
        };

        pub inline fn init(
            joypad: *Joypad,
            serial: *Serial,
            timer: *Timer,
            interrupt: *Interrupt,
            audio: *Audio,
            wave: *Wave,
            lcd: *Lcd,
            boot_rom: *BootRom,
        ) This {
            return This{
                .joypad = joypad,
                .serial = serial,
                .timer = timer,
                .interrupt = interrupt,
                .audio = audio,
                .wave = wave,
                .lcd = lcd,
                .boot_rom = boot_rom,
            };
        }

        inline fn decode(addr: u16) struct { u16, Target } {
            return switch (addr) {
                0x00 => .{ 0x00, .joypad },
                0x01...0x02 => .{ addr - 0x01, .serial },
                0x04...0x07 => .{ addr - 0x04, .timer },
                0x0F => .{ 0x00, .interrupt },
                0x10...0x26 => .{ addr - 0x10, .audio },
                0x30...0x3F => .{ addr - 0x30, .wave },
                0x40...0x4B => .{ addr - 0x40, .lcd },
                0x50 => .{ 0x00, .boot_rom },
                0x80 => .{ 0x01, .interrupt },
                else => .{ addr, .invalid },
            };
        }

        inline fn dispatch(self: *This, addr: u16, comptime operation: Operation, value: u8) struct { MemoryFlag, u8 } {
            const resolved_address, const target = decode(addr);
            return switch (target) {
                .joypad => make_call(self.joypad, Joypad, operation, resolved_address, value),
                .serial => make_call(self.serial, Serial, operation, resolved_address, value),
                .timer => make_call(self.timer, Timer, operation, resolved_address, value),
                .interrupt => make_call(self.interrupt, Interrupt, operation, resolved_address, value),
                .audio => make_call(self.audio, Audio, operation, resolved_address, value),
                .wave => make_call(self.wave, Wave, operation, resolved_address, value),
                .lcd => make_call(self.lcd, Lcd, operation, resolved_address, value),
                .boot_rom => make_call(self.boot_rom, BootRom, operation, resolved_address, value),
                .invalid => make_call(self, This.Invalid, operation, resolved_address, value),
            };
        }

        inline fn make_call(instance: anytype, comptime namespace: type, comptime operation: Operation, addr: u16, value: u8) struct { MemoryFlag, u8 } {
            const opname = @tagName(operation);
            switch (operation) {
                .peek => return .{ undefined, @field(namespace, opname)(instance, addr) },
                .poke => {
                    @field(namespace, opname)(instance, addr, value);
                    return undefined;
                },
                .read => return @field(namespace, opname)(instance, addr),
                .write => return .{ @field(namespace, opname)(instance, addr, value), undefined },
            }
        }

        pub inline fn peek(self: *This, addr: u16) u8 {
            _, const ret = self.dispatch(addr, .peek, undefined);
            return ret;
        }

        pub inline fn poke(self: *This, addr: u16, val: u8) void {
            _ = self.dispatch(addr, .poke, val);
        }

        pub inline fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            return self.dispatch(addr, .read, undefined);
        }

        pub inline fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            const ret, _ = self.dispatch(addr, .write, val);
            return ret;
        }
    };
}

pub const Dummy = struct {
    lastval: u8 = 0,

    pub fn peek(self: *Dummy, _: u16) u8 {
        return self.lastval;
    }

    pub fn poke(self: *Dummy, _: u16, val: u8) void {
        self.lastval = val;
    }

    pub fn read(self: *Dummy, addr: u16) struct { MemoryFlag, u8 } {
        return .{ .{}, self.peek(addr) };
    }

    pub fn write(self: *Dummy, addr: u16, val: u8) MemoryFlag {
        self.poke(addr, val);
        return .{};
    }
};

const MockedMmio = Mmio(Dummy, Dummy, Dummy, Dummy, Dummy, Dummy, Dummy, Dummy);

test "Mmio unit tests with Dummy" {
    var joypad = Dummy{};
    var serial = Dummy{};
    var timer = Dummy{};
    var interrupts = Dummy{};
    var audio = Dummy{};
    var wave = Dummy{};
    var lcd = Dummy{};
    var boot_rom = Dummy{};

    var mmio = MockedMmio.init(&joypad, &serial, &timer, &interrupts, &audio, &wave, &lcd, &boot_rom);

    // Test writing and reading joypad
    _ = mmio.write(0x00, 0xAA);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0xAA }, mmio.read(0x00));

    // Test writing and reading serial
    _ = mmio.write(0x01, 0xBB);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0xBB }, mmio.read(0x01));

    _ = mmio.write(0x02, 0xCC);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0xCC }, mmio.read(0x02));

    // Test writing and reading timer
    _ = mmio.write(0x04, 0xDD);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0xDD }, mmio.read(0x04));

    _ = mmio.write(0x07, 0xEE);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0xEE }, mmio.read(0x07));

    // Test writing and reading interrupts
    _ = mmio.write(0x0F, 0x12);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0x12 }, mmio.read(0x0F));

    // Test writing and reading audio
    _ = mmio.write(0x10, 0x34);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0x34 }, mmio.read(0x10));

    _ = mmio.write(0x26, 0x56);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0x56 }, mmio.read(0x26));

    // Test writing and reading wave
    _ = mmio.write(0x30, 0x78);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0x78 }, mmio.read(0x30));

    _ = mmio.write(0x3F, 0x9A);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0x9A }, mmio.read(0x3F));

    // Test writing and reading lcd
    _ = mmio.write(0x40, 0xBC);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0xBC }, mmio.read(0x40));

    _ = mmio.write(0x4B, 0xDE);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0xDE }, mmio.read(0x4B));

    // Test writing and reading boot_rom
    _ = mmio.write(0x50, 0xF0);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0xF0 }, mmio.read(0x50));

    // Test reading unmapped address
    try std.testing.expectEqual(.{ MemoryFlag{ .illegal = true }, 0xFF }, mmio.read(0x60));
}
