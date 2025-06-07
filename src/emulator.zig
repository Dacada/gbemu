const std = @import("std");
const Mmu = @import("mmu.zig").Mmu;
const Cpu = @import("cpu.zig").Cpu;
const Rom = @import("rom.zig").Rom;
const Debugger = @import("debugger.zig").Debugger;

pub const Emulator = struct {
    mmu: *Mmu,
    cpu: *Cpu,
    debugger: *Debugger,

    pub fn init(mmu: *Mmu, cpu: *Cpu, debugger: *Debugger) Emulator {
        return .{
            .mmu = mmu,
            .cpu = cpu,
            .debugger = debugger,
        };
    }

    fn initialize_components(self: *Emulator, header_checksum: u8) void {
        // DMG ONLY -- https://gbdev.io/pandocs/Power_Up_Sequence.html
        self.cpu.reg.AF.Hi = 0x01;
        self.cpu.reg.AF.Lo.Z = 1;
        self.cpu.reg.AF.Lo.N = 0;
        self.cpu.reg.AF.Lo.H = @intFromBool(header_checksum != 0);
        self.cpu.reg.AF.Lo.C = @intFromBool(header_checksum != 0);
        self.cpu.reg.BC.Hi = 0x00;
        self.cpu.reg.BC.Lo = 0x13;
        self.cpu.reg.DE.Hi = 0x00;
        self.cpu.reg.DE.Lo = 0xD8;
        self.cpu.reg.HL.Hi = 0x01;
        self.cpu.reg.HL.Lo = 0x4D;
        self.cpu.reg.PC = 0x0100;
        self.cpu.reg.SP.setAll(0xFFFE);
        self.mmu.setValue(0xFF00, 0xCF);
        self.mmu.setValue(0xFF01, 0x00);
        self.mmu.setValue(0xFF02, 0x7E);
        self.mmu.setValue(0xFF04, 0xAB);
        self.mmu.setValue(0xFF05, 0x00);
        self.mmu.setValue(0xFF06, 0x00);
        self.mmu.setValue(0xFF07, 0xF8);
        self.mmu.setValue(0xFF0F, 0xE1);
        self.mmu.setValue(0xFF10, 0x80);
        self.mmu.setValue(0xFF11, 0xBF);
        self.mmu.setValue(0xFF12, 0xF3);
        self.mmu.setValue(0xFF13, 0xFF);
        self.mmu.setValue(0xFF14, 0xBF);
        self.mmu.setValue(0xFF16, 0x3F);
        self.mmu.setValue(0xFF17, 0x00);
        self.mmu.setValue(0xFF18, 0xFF);
        self.mmu.setValue(0xFF19, 0xBF);
        self.mmu.setValue(0xFF1A, 0x7F);
        self.mmu.setValue(0xFF1B, 0xFF);
        self.mmu.setValue(0xFF1C, 0x9F);
        self.mmu.setValue(0xFF1D, 0xFF);
        self.mmu.setValue(0xFF1E, 0xBF);
        self.mmu.setValue(0xFF20, 0xFF);
        self.mmu.setValue(0xFF21, 0x00);
        self.mmu.setValue(0xFF22, 0x00);
        self.mmu.setValue(0xFF23, 0xBF);
        self.mmu.setValue(0xFF24, 0x77);
        self.mmu.setValue(0xFF25, 0xF3);
        self.mmu.setValue(0xFF26, 0xF1);
        self.mmu.setValue(0xFF40, 0x91);
        self.mmu.setValue(0xFF41, 0x85);
        self.mmu.setValue(0xFF42, 0x00);
        self.mmu.setValue(0xFF43, 0x00);
        self.mmu.setValue(0xFF44, 0x00);
        self.mmu.setValue(0xFF45, 0x00);
        self.mmu.setValue(0xFF46, 0xFF);
        self.mmu.setValue(0xFF47, 0xFC);
        self.mmu.setValue(0xFF48, 0xFF);
        self.mmu.setValue(0xFF49, 0xFF);
        self.mmu.setValue(0xFF4A, 0x00);
        self.mmu.setValue(0xFF4B, 0x00);
        self.mmu.setValue(0xFFFF, 0x00);
    }

    pub fn mapRom(self: *Emulator, rom: *const Rom) void {
        self.initialize_components(rom.checksum);
        self.mmu.mapRom(rom.rom);
    }

    pub fn run(self: *Emulator, start_in_debugger: bool) !void {
        if (start_in_debugger) {
            const result = try self.debugger.enter();
            if (result == .should_stop) {
                return;
            }
        }
        while (true) {
            self.cpu.tick();
            const result = try self.debugger.enter_debugger_if_needed();
            if (result == .should_stop) {
                break;
            }
        }
    }
};
