const std = @import("std");
const memory = @import("memory.zig");
const Memory = memory.Memory;
const MemoryFlag = memory.MemoryFlag;

const logger = std.log.scoped(.mmio);

pub const Mmio = struct {
    joypad: Memory,
    serial: Memory,
    timer: Memory,
    interrupts: Memory,
    audio: Memory,
    wave: Memory,
    lcd: Memory,
    boot_rom: Memory,

    fn dispatch(self: *const Mmio, addr: u16) ?struct { u16, Memory } {
        if (addr == 0x00) {
            return .{ 0x00, self.joypad };
        } else if (addr >= 0x01 and addr <= 0x02) {
            return .{ addr - 0x01, self.serial };
        } else if (addr >= 0x04 and addr <= 0x07) {
            return .{ addr - 0x04, self.timer };
        } else if (addr == 0x0F) {
            return .{ 0x00, self.interrupts };
        } else if (addr >= 0x10 and addr <= 0x26) {
            return .{ addr - 0x10, self.audio };
        } else if (addr >= 0x30 and addr <= 0x3F) {
            return .{ addr - 0x30, self.wave };
        } else if (addr >= 0x40 and addr <= 0x4B) {
            return .{ addr - 0x40, self.lcd };
        } else if (addr == 0x50) {
            return .{ 0x00, self.boot_rom };
        }

        // DMG ONLY -- CGB adds new MMIO registers

        return null;
    }

    fn peek(selfptr: *anyopaque, addr: u16) u8 {
        const self: *Mmio = @alignCast(@ptrCast(selfptr));
        if (self.dispatch(addr)) |disp| {
            const newaddr, const mem = disp;
            return mem.peek(newaddr);
        }
        return 0xFF;
    }

    fn poke(selfptr: *anyopaque, addr: u16, val: u8) void {
        const self: *Mmio = @alignCast(@ptrCast(selfptr));
        if (self.dispatch(addr)) |disp| {
            const newaddr, const mem = disp;
            mem.poke(newaddr, val);
            return;
        }
    }

    fn read(selfptr: *anyopaque, addr: u16) struct { ?MemoryFlag, u8 } {
        const self: *Mmio = @alignCast(@ptrCast(selfptr));
        if (self.dispatch(addr)) |disp| {
            const newaddr, var mem = disp;
            return .{ mem.flags, mem.read(newaddr) };
        }
        return .{ MemoryFlag{ .illegal = true }, 0xFF };
    }

    fn write(selfptr: *anyopaque, addr: u16, val: u8) ?MemoryFlag {
        const self: *Mmio = @alignCast(@ptrCast(selfptr));
        if (self.dispatch(addr)) |disp| {
            const newaddr, var mem = disp;
            mem.write(newaddr, val);
            return mem.flags;
        }
        return MemoryFlag{ .illegal = true };
    }

    pub fn memory(mmio: *Mmio) Memory {
        return Memory{
            .ctx = mmio,
            .peek_cb = peek,
            .poke_cb = poke,
            .read_cb = read,
            .write_cb = write,
        };
    }
};

const SimpleMemory = memory.SimpleMemory;

// Static backing arrays for SimpleMemory
var joypad_data = [_]u8{0} ** 0x10000;
var serial_data = [_]u8{0} ** 0x10000;
var timer_data = [_]u8{0} ** 0x10000;
var interrupts_data = [_]u8{0} ** 0x10000;
var audio_data = [_]u8{0} ** 0x10000;
var wave_data = [_]u8{0} ** 0x10000;
var lcd_data = [_]u8{0} ** 0x10000;
var boot_rom_data = [_]u8{0} ** 0x10000;

test "Mmio unit tests with SimpleMemory" {
    const JoypadMemory = SimpleMemory(false, &joypad_data, null);
    const SerialMemory = SimpleMemory(false, &serial_data, null);
    const TimerMemory = SimpleMemory(false, &timer_data, null);
    const InterruptsMemory = SimpleMemory(false, &interrupts_data, null);
    const AudioMemory = SimpleMemory(false, &audio_data, null);
    const WaveMemory = SimpleMemory(false, &wave_data, null);
    const LcdMemory = SimpleMemory(false, &lcd_data, null);
    const BootRomMemory = SimpleMemory(false, &boot_rom_data, null);

    var mmio = Mmio{
        .joypad = JoypadMemory.memory(),
        .serial = SerialMemory.memory(),
        .timer = TimerMemory.memory(),
        .interrupts = InterruptsMemory.memory(),
        .audio = AudioMemory.memory(),
        .wave = WaveMemory.memory(),
        .lcd = LcdMemory.memory(),
        .boot_rom = BootRomMemory.memory(),
    };

    var mmio_mem = Mmio.memory(&mmio);

    // Test writing and reading joypad
    mmio_mem.write(0x00, 0xAA);
    try std.testing.expect(mmio_mem.read(0x00) == 0xAA);

    // Test writing and reading serial
    mmio_mem.write(0x01, 0xBB);
    try std.testing.expect(mmio_mem.read(0x01) == 0xBB);

    mmio_mem.write(0x02, 0xCC);
    try std.testing.expect(mmio_mem.read(0x02) == 0xCC);

    // Test writing and reading timer
    mmio_mem.write(0x04, 0xDD);
    try std.testing.expect(mmio_mem.read(0x04) == 0xDD);

    mmio_mem.write(0x07, 0xEE);
    try std.testing.expect(mmio_mem.read(0x07) == 0xEE);

    // Test writing and reading interrupts
    mmio_mem.write(0x0F, 0x12);
    try std.testing.expect(mmio_mem.read(0x0F) == 0x12);

    // Test writing and reading audio
    mmio_mem.write(0x10, 0x34);
    try std.testing.expect(mmio_mem.read(0x10) == 0x34);

    mmio_mem.write(0x26, 0x56);
    try std.testing.expect(mmio_mem.read(0x26) == 0x56);

    // Test writing and reading wave
    mmio_mem.write(0x30, 0x78);
    try std.testing.expect(mmio_mem.read(0x30) == 0x78);

    mmio_mem.write(0x3F, 0x9A);
    try std.testing.expect(mmio_mem.read(0x3F) == 0x9A);

    // Test writing and reading lcd
    mmio_mem.write(0x40, 0xBC);
    try std.testing.expect(mmio_mem.read(0x40) == 0xBC);

    mmio_mem.write(0x4B, 0xDE);
    try std.testing.expect(mmio_mem.read(0x4B) == 0xDE);

    // Test writing and reading boot_rom
    mmio_mem.write(0x50, 0xF0);
    try std.testing.expect(mmio_mem.read(0x50) == 0xF0);

    // Test reading unmapped address
    const unmapped_value = mmio_mem.read(0x60);
    try std.testing.expect(unmapped_value == 0xFF);
    try std.testing.expect(mmio_mem.flags.illegal == true);
}
