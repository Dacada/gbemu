const std = @import("std");

const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;
const GenericRouter = @import("router.zig").Router;
const GenericRange = @import("router.zig").Range;
const GenericTargetField = @import("router.zig").TargetField;

const logger = std.log.scoped(.mmio);

pub fn Mmio(Joypad: type, Serial: type, Timer: type, Interrupt: type, Apu: type, Lcd: type, BootRom: type) type {
    return struct {
        const This = @This();

        joypad: *Joypad,
        serial: *Serial,
        timer: *Timer,
        interrupt: *Interrupt,
        apu: *Apu,
        lcd: *Lcd,
        boot_rom: *BootRom,

        const Invalid = struct {
            pub fn read(_: *This, _: u16) struct { MemoryFlag, u8 } {
                return .{ .{ .illegal = true }, 0xFF };
            }

            pub fn write(_: *This, _: u16, _: u8) MemoryFlag {
                return .{ .illegal = true };
            }

            pub fn peek(_: *This, _: u16) u8 {
                return 0xFF;
            }
            pub fn poke(_: *This, _: u16, _: u8) void {}
        };

        const Target = enum {
            joypad,
            serial,
            timer,
            interrupt,
            apu,
            lcd,
            boot_rom,
            invalid,
        };

        const Range = GenericRange(Target);
        const TargetField = GenericTargetField(Target);
        const Router = GenericRouter(
            Target,
            &[_]Range{
                .{
                    .start = 0x00,
                    .end = 0x00,
                    .target = .joypad,
                },
                .{
                    .start = 0x01,
                    .end = 0x02,
                    .target = .serial,
                },
                .{
                    .start = 0x04,
                    .end = 0x07,
                    .target = .timer,
                },
                .{
                    .start = 0x0F,
                    .end = 0x0F,
                    .target = .interrupt,
                },
                .{
                    .start = 0x10,
                    .end = 0x3F,
                    .target = .apu,
                },
                .{
                    .start = 0x40,
                    .end = 0x4B,
                    .target = .lcd,
                },
                .{
                    .start = 0x50,
                    .end = 0x50,
                    .target = .boot_rom,
                },
                .{
                    .start = 0x80,
                    .end = 0x80,
                    .target = .interrupt,
                },
            },
            .{
                .addr = undefined,
                .target = .invalid,
            },
            &[_]TargetField{
                .{
                    .target = .joypad,
                    .field = "joypad",
                    .namespace = Joypad,
                },
                .{
                    .target = .serial,
                    .field = "serial",
                    .namespace = Serial,
                },
                .{
                    .target = .timer,
                    .field = "timer",
                    .namespace = Timer,
                },
                .{
                    .target = .interrupt,
                    .field = "interrupt",
                    .namespace = Interrupt,
                },
                .{
                    .target = .apu,
                    .field = "apu",
                    .namespace = Apu,
                },
                .{
                    .target = .lcd,
                    .field = "lcd",
                    .namespace = Lcd,
                },
                .{
                    .target = .boot_rom,
                    .field = "boot_rom",
                    .namespace = BootRom,
                },
                .{
                    .target = .invalid,
                    .field = null,
                    .namespace = This.Invalid,
                },
            },
        );

        pub inline fn init(
            joypad: *Joypad,
            serial: *Serial,
            timer: *Timer,
            interrupt: *Interrupt,
            apu: *Apu,
            lcd: *Lcd,
            boot_rom: *BootRom,
        ) This {
            return This{
                .joypad = joypad,
                .serial = serial,
                .timer = timer,
                .interrupt = interrupt,
                .apu = apu,
                .lcd = lcd,
                .boot_rom = boot_rom,
            };
        }

        pub inline fn peek(self: *This, addr: u16) u8 {
            _, const ret = Router.dispatch(self, addr, .peek, undefined);
            return ret;
        }

        pub inline fn poke(self: *This, addr: u16, val: u8) void {
            _ = Router.dispatch(self, addr, .poke, val);
        }

        pub inline fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            return Router.dispatch(self, addr, .read, undefined);
        }

        pub inline fn write(self: *This, addr: u16, val: u8) MemoryFlag {
            const ret, _ = Router.dispatch(self, addr, .write, val);
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

const MockedMmio = Mmio(Dummy, Dummy, Dummy, Dummy, Dummy, Dummy, Dummy);

test "Mmio unit tests with Dummy" {
    var joypad = Dummy{};
    var serial = Dummy{};
    var timer = Dummy{};
    var interrupts = Dummy{};
    var apu = Dummy{};
    var lcd = Dummy{};
    var boot_rom = Dummy{};

    var mmio = MockedMmio.init(&joypad, &serial, &timer, &interrupts, &apu, &lcd, &boot_rom);

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

    // Test writing and reading apu
    _ = mmio.write(0x10, 0x34);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0x34 }, mmio.read(0x10));

    _ = mmio.write(0x26, 0x56);
    try std.testing.expectEqual(.{ MemoryFlag{}, 0x56 }, mmio.read(0x26));

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
