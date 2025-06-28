const std = @import("std");
const Cpu = @import("cpu.zig").Cpu;
const Ppu = @import("ppu.zig").Ppu;
const Memory = @import("memory.zig").Memory;
const Scheduler = @import("scheduler.zig").Scheduler;

pub fn Emulator(Debugger: type) type {
    return struct {
        const This = @This();

        cpu: *Cpu,
        ppu: *Ppu,
        sched: *Scheduler,
        debugger: *Debugger,

        divider: u2 = 0,

        pub fn tick(self: *This) !bool {
            self.ppu.tick();
            if (self.divider == 0) {
                self.cpu.tick();
                const result = try self.debugger.enter_debugger_if_needed();
                if (result == .should_stop) {
                    return true;
                }
            }
            self.sched.tick();

            self.divider +%= 1;
            return false;
        }

        pub fn run(self: *This, start_in_debugger: bool) !void {
            if (start_in_debugger) {
                const result = try self.debugger.enter();
                if (result == .should_stop) {
                    return;
                }
            }
            while (true) {
                if (try self.tick()) {
                    return;
                }
            }
        }
    };
}

pub fn initialize_cpu(cpu: *Cpu, header_checksum: u8) void {
    // DMG ONLY -- https://gbdev.io/pandocs/Power_Up_Sequence.html
    cpu.reg.AF.Hi = 0x01;
    cpu.reg.AF.Lo.Z = 1;
    cpu.reg.AF.Lo.N = 0;
    cpu.reg.AF.Lo.H = @intFromBool(header_checksum != 0);
    cpu.reg.AF.Lo.C = @intFromBool(header_checksum != 0);
    cpu.reg.BC.Hi = 0x00;
    cpu.reg.BC.Lo = 0x13;
    cpu.reg.DE.Hi = 0x00;
    cpu.reg.DE.Lo = 0xD8;
    cpu.reg.HL.Hi = 0x01;
    cpu.reg.HL.Lo = 0x4D;
    cpu.reg.PC = 0x0100;
    cpu.reg.SP.setAll(0xFFFE);
}

pub fn initialize_memory(mem: Memory) void {
    // DMG ONLY -- https://gbdev.io/pandocs/Power_Up_Sequence.html
    var memory = mem;
    memory.write(0xFF00, 0xCF);
    memory.write(0xFF01, 0x00);
    memory.poke(0xFF02, 0x7E);
    memory.poke(0xFF04, 0xAB);
    memory.poke(0xFF05, 0x00);
    memory.poke(0xFF06, 0x00);
    memory.poke(0xFF07, 0xF8);
    memory.poke(0xFF0F, 0xE1);
    memory.poke(0xFF10, 0x80);
    memory.poke(0xFF11, 0xBF);
    memory.poke(0xFF12, 0xF3);
    memory.poke(0xFF13, 0xFF);
    memory.poke(0xFF14, 0xBF);
    memory.poke(0xFF16, 0x3F);
    memory.poke(0xFF17, 0x00);
    memory.poke(0xFF18, 0xFF);
    memory.poke(0xFF19, 0xBF);
    memory.poke(0xFF1A, 0x7F);
    memory.poke(0xFF1B, 0xFF);
    memory.poke(0xFF1C, 0x9F);
    memory.poke(0xFF1D, 0xFF);
    memory.poke(0xFF1E, 0xBF);
    memory.poke(0xFF20, 0xFF);
    memory.poke(0xFF21, 0x00);
    memory.poke(0xFF22, 0x00);
    memory.poke(0xFF23, 0xBF);
    memory.poke(0xFF24, 0x77);
    memory.poke(0xFF25, 0xF3);
    memory.poke(0xFF26, 0xF1);
    memory.poke(0xFF40, 0x91);
    memory.poke(0xFF41, 0x85);
    memory.poke(0xFF42, 0x00);
    memory.poke(0xFF43, 0x00);
    memory.poke(0xFF44, 0x00);
    memory.poke(0xFF45, 0x00);
    memory.poke(0xFF46, 0xFF);
    memory.poke(0xFF47, 0xFC);
    memory.poke(0xFF48, 0xFF);
    memory.poke(0xFF49, 0xFF);
    memory.poke(0xFF4A, 0x00);
    memory.poke(0xFF4B, 0x00);
    memory.poke(0xFFFF, 0x00);
}
