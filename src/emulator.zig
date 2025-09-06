const std = @import("std");

pub fn Emulator(Cpu: type, Apu: type, Ppu: type, Timer: type, Scheduler: type, Debugger: type) type {
    return struct {
        const This = @This();

        cpu: *Cpu,
        apu: *Apu,
        ppu: *Ppu,
        timer: *Timer,
        sched: *Scheduler,
        dbg: *Debugger,

        divider: u2,

        pub inline fn init(cpu: *Cpu, apu: *Apu, ppu: *Ppu, timer: *Timer, sched: *Scheduler, dbg: *Debugger) This {
            return This{
                .cpu = cpu,
                .apu = apu,
                .ppu = ppu,
                .timer = timer,
                .sched = sched,
                .dbg = dbg,
                .divider = 0,
            };
        }

        pub fn tick(self: *This) !bool {
            self.ppu.tick();
            if (self.divider == 0) {
                self.cpu.tick();

                // timer overwrites some registers even if CPU writes to them on the same tick and it also needs to
                // react to falling edges on memory writes, to emulate all this we update it right after the CPU but
                // before any debugger calls
                self.timer.tick();
            }

            if (self.divider % 2 == 0) {
                // Sound system counters advance. They depend on timer having ticked.
                self.apu.tick();
            }

            if (self.divider == 0) {
                const result = try self.dbg.enterDebuggerIfNeeded();
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

pub fn initializeCpu(T: type, cpu: *T, header_checksum: u8) void {
    // DMG ONLY -- https://gbdev.io/pandocs/Power_Up_Sequence.html
    cpu.reg.af.hi = 0x01;
    cpu.reg.af.lo.z = 1;
    cpu.reg.af.lo.n = 0;
    cpu.reg.af.lo.h = @intFromBool(header_checksum != 0);
    cpu.reg.af.lo.c = @intFromBool(header_checksum != 0);
    cpu.reg.bc.hi = 0x00;
    cpu.reg.bc.lo = 0x13;
    cpu.reg.de.hi = 0x00;
    cpu.reg.de.lo = 0xD8;
    cpu.reg.hl.hi = 0x01;
    cpu.reg.hl.lo = 0x4D;
    cpu.reg.pc = 0x0100;
    cpu.reg.sp.setAll(0xFFFE);
    cpu.reg.ime = 0;
}

pub fn initializeMemory(T: type, mmu: *T) void {
    // DMG ONLY -- https://gbdev.io/pandocs/Power_Up_Sequence.html
    mmu.write(0xFF00, 0xCF);
    mmu.write(0xFF01, 0x00);
    mmu.poke(0xFF02, 0x7E);
    mmu.poke(0xFF04, 0xAB);
    mmu.poke(0xFF05, 0x00);
    mmu.poke(0xFF06, 0x00);
    mmu.write(0xFF07, 0xF8);
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
