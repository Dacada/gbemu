const std = @import("std");

pub fn Emulator(Cpu: type, Ppu: type, Scheduler: type, Debugger: type) type {
    return struct {
        const This = @This();

        cpu: *Cpu,
        ppu: *Ppu,
        sched: *Scheduler,
        dbg: *Debugger,

        divider: u2,

        pub inline fn init(cpu: *Cpu, ppu: *Ppu, sched: *Scheduler, dbg: *Debugger) This {
            return This{
                .cpu = cpu,
                .ppu = ppu,
                .sched = sched,
                .dbg = dbg,
                .divider = 0,
            };
        }

        pub fn tick(self: *This) !bool {
            self.ppu.tick();
            if (self.divider == 0) {
                self.cpu.tick();
                const result = try self.dbg.enter_debugger_if_needed();
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
                const result = try self.dbg.enter();
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

pub fn initialize_cpu(T: type, cpu: *T, header_checksum: u8) void {
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
    cpu.reg.IME = 0;
}

pub fn initialize_memory(T: type, mmu: *T) void {
    // DMG ONLY -- https://gbdev.io/pandocs/Power_Up_Sequence.html
    mmu.write(0xFF00, 0xCF);
    mmu.write(0xFF01, 0x00);
    mmu.poke(0xFF02, 0x7E);
    mmu.poke(0xFF04, 0xAB);
    mmu.poke(0xFF05, 0x00);
    mmu.poke(0xFF06, 0x00);
    mmu.poke(0xFF07, 0xF8);
    mmu.poke(0xFF0F, 0xE1);
    mmu.poke(0xFF10, 0x80);
    mmu.poke(0xFF11, 0xBF);
    mmu.poke(0xFF12, 0xF3);
    mmu.poke(0xFF13, 0xFF);
    mmu.poke(0xFF14, 0xBF);
    mmu.poke(0xFF16, 0x3F);
    mmu.poke(0xFF17, 0x00);
    mmu.poke(0xFF18, 0xFF);
    mmu.poke(0xFF19, 0xBF);
    mmu.poke(0xFF1A, 0x7F);
    mmu.poke(0xFF1B, 0xFF);
    mmu.poke(0xFF1C, 0x9F);
    mmu.poke(0xFF1D, 0xFF);
    mmu.poke(0xFF1E, 0xBF);
    mmu.poke(0xFF20, 0xFF);
    mmu.poke(0xFF21, 0x00);
    mmu.poke(0xFF22, 0x00);
    mmu.poke(0xFF23, 0xBF);
    mmu.poke(0xFF24, 0x77);
    mmu.poke(0xFF25, 0xF3);
    mmu.poke(0xFF26, 0xF1);
    mmu.poke(0xFF40, 0x91);
    mmu.poke(0xFF41, 0x85);
    mmu.poke(0xFF42, 0x00);
    mmu.poke(0xFF43, 0x00);
    mmu.poke(0xFF44, 0x00);
    mmu.poke(0xFF45, 0x00);
    mmu.poke(0xFF46, 0xFF);
    mmu.poke(0xFF47, 0xFC);
    mmu.poke(0xFF48, 0xFF);
    mmu.poke(0xFF49, 0xFF);
    mmu.poke(0xFF4A, 0x00);
    mmu.poke(0xFF4B, 0x00);
    mmu.poke(0xFFFF, 0x00);
}
