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

        /// Emulate a T-cycle
        pub fn tick(self: *This) !bool {
            // Step the PPU first thing on the T-cycle. This might be useful for the OAM bug later on.
            self.ppu.tick(self.divider);

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

    // Joypad
    mmu.write(0xFF00, 0xCF); // select buttons + select d-pad available in the lower nibble

    // Serial
    mmu.write(0xFF01, 0x00); // data transfer out shift register initialized to zero
    mmu.poke(0xFF02, 0x7E); // set in a state meaning: do not request a transfer, external clock (slave)

    // Timer
    mmu.poke(0xFF04, 0xAB); // divider value forced to this without triggering extra stuff as if it had been changed normally
    mmu.poke(0xFF05, 0x00); // set timer counter, usually would remember that it was written this tick but technically it wasn't, so we poke
    mmu.poke(0xFF06, 0x00); // timer module, write and poke are the same anyway, but still don't wanna set possible side effects, timer is complex
    mmu.write(0xFF07, 0xF8); // do not increment TIMA, clock select lowest freq

    // Interrupts
    mmu.write(0xFF0F, 0xE1); // disable all interrupts except vblank

    // Audio -> Write values but don't run side effects on the "trigger" registers
    mmu.write(0xFF10, 0x80); // ch1
    mmu.write(0xFF11, 0xBF); // ch1
    mmu.write(0xFF12, 0xF3); // ch1
    mmu.write(0xFF13, 0xFF); // ch1
    mmu.poke(0xFF14, 0xBF); // ch1 (trigger)
    mmu.write(0xFF16, 0x3F); // ch2
    mmu.write(0xFF17, 0x00); // ch2
    mmu.write(0xFF18, 0xFF); // ch2
    mmu.poke(0xFF19, 0xBF); // ch2 (trigger)
    mmu.write(0xFF1A, 0x7F); // ch3
    mmu.write(0xFF1B, 0xFF); // ch3
    mmu.write(0xFF1C, 0x9F); // ch3
    mmu.write(0xFF1D, 0xFF); // ch3
    mmu.poke(0xFF1E, 0xBF); // ch3 (trigger)
    mmu.write(0xFF20, 0xFF); // ch4
    mmu.write(0xFF21, 0x00); // ch4
    mmu.write(0xFF22, 0x00); // ch4
    mmu.poke(0xFF23, 0xBF); // ch4 (trigger)
    mmu.write(0xFF24, 0x77); // global
    mmu.write(0xFF25, 0xF3); // global
    mmu.write(0xFF26, 0xF1); // global

    // PPU
    mmu.write(0xFF40, 0x91); // enable ppu
    mmu.write(0xFF41, 0x85); // unset all conditions for STAT interrupt
    mmu.write(0xFF42, 0x00); // viewport Y to zero
    mmu.write(0xFF43, 0x00); // viewport X to zero
    mmu.poke(0xFF44, 0x00); // read only LCD Y
    mmu.write(0xFF45, 0x00); // LCD Y compare value
    mmu.poke(0xFF46, 0xFF); // OAM DMA (do not trigger the transfer, poke only)
    mmu.write(0xFF47, 0xFC); // BG monochrome palette
    mmu.write(0xFF48, 0xFF); // OBJ monochrome palette 0
    mmu.write(0xFF49, 0xFF); // OBJ monochrome palette 1
    mmu.write(0xFF4A, 0x00); // window position Y
    mmu.write(0xFF4B, 0x00); // window position X

    // IE
    mmu.write(0xFFFF, 0x00); // disable all interrupts
}
