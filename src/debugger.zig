const std = @import("std");
const Cpu = @import("cpu.zig").Cpu;
const assembler = @import("assembler.zig");

extern fn readline(prompt: [*:0]const u8) callconv(.C) ?[*:0]u8;
extern fn add_history(line: [*:0]const u8) callconv(.C) void;

pub const DebuggerResult = enum {
    should_continue,
    should_stop,
};

const Commands = struct {
    pub fn help(_: *Debugger, _: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
        const text =
            \\continue c - continue execution until next breakpoint
            \\registers reg - dump register state
            \\step s - step one instruction
            \\stepc sc - step one cpu cycle
            \\peek - read memory (no side effects, no forbidden reads)
            \\poke - modify memory (with side effects, read-only forbidden)
            \\disassemble d - disassemble instructions at address
            \\break b - create/list/delete breakpoint
            \\quit q - stop emulation and exit
            \\
        ;
        try writer.writeAll(text);
        return null;
    }

    pub const c = @"continue";
    pub fn @"continue"(_: *Debugger, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
        return .should_continue;
    }

    pub const reg = registers;
    pub fn registers(dbg: *Debugger, _: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
        const fmt =
            \\AF: 0x{X:0>4}   A: 0x{X:0>2}  F: 0x{X:0>2}  [Z:{d} N:{d} H:{d} C:{d}]
            \\BC: 0x{X:0>4}   B: 0x{X:0>2}  C: 0x{X:0>2}
            \\DE: 0x{X:0>4}   D: 0x{X:0>2}  E: 0x{X:0>2}
            \\HL: 0x{X:0>4}   H: 0x{X:0>2}  L: 0x{X:0>2}
            \\SP: 0x{X:0>4}
            \\PC: 0x{X:0>4}
            \\IME: {d}
            \\
            \\ --- Internal ---
            \\WZ: 0x{X:0>4}   W: 0x{X:0>2}  Z:x{X:0>2}
            \\IR: 0x{X:0>2}
            \\
        ;
        try writer.print(fmt, .{
            dbg.cpu.reg.AF.all(), dbg.cpu.reg.AF.Hi, dbg.cpu.reg.AF.Lo.all(), dbg.cpu.reg.AF.Lo.Z, dbg.cpu.reg.AF.Lo.N, dbg.cpu.reg.AF.Lo.H, dbg.cpu.reg.AF.Lo.C, //
            dbg.cpu.reg.BC.all(), dbg.cpu.reg.BC.Hi, dbg.cpu.reg.BC.Lo, //
            dbg.cpu.reg.DE.all(), dbg.cpu.reg.DE.Hi, dbg.cpu.reg.DE.Lo, //
            dbg.cpu.reg.HL.all(), dbg.cpu.reg.HL.Hi, dbg.cpu.reg.HL.Lo, //
            dbg.cpu.reg.SP.all(), //
            dbg.cpu.reg.PC, //
            dbg.cpu.reg.IME, //
            dbg.cpu.reg.WZ.all(), dbg.cpu.reg.WZ.Hi, dbg.cpu.reg.WZ.Lo, //
            dbg.cpu.reg.IR, //
        });
        return null;
    }

    pub const s = step;
    pub fn step(dbg: *Debugger, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
        dbg.stepping = true;
        return .should_continue;
    }

    pub const sc = stepc;
    pub fn stepc(dbg: *Debugger, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
        dbg.stepping_cycles = true;
        return .should_continue;
    }

    pub fn peek(dbg: *Debugger, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
        const usage = "usage: peek addr [count]\n";

        const addr: u16 = std.fmt.parseInt(u16, it.next() orelse {
            try writer.writeAll(usage);
            return null;
        }, 0) catch {
            try writer.writeAll(usage);
            return null;
        };

        const count: u16 = blk: {
            break :blk std.fmt.parseInt(u16, it.next() orelse {
                break :blk 1;
            }, 0) catch {
                try writer.writeAll(usage);
                return null;
            };
        };

        try _print_memory(dbg, writer, addr, count);
        dbg.cpu.mmu.clearIllegalOperation();
        return null;
    }

    pub fn poke(dbg: *Debugger, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
        const usage = "usage: poke addr value\n";

        const addr: u16 = std.fmt.parseInt(u16, it.next() orelse {
            try writer.writeAll(usage);
            return null;
        }, 0) catch {
            try writer.writeAll(usage);
            return null;
        };

        const value: u8 = std.fmt.parseInt(u8, it.next() orelse {
            try writer.writeAll(usage);
            return null;
        }, 0) catch {
            try writer.writeAll(usage);
            return null;
        };

        dbg.cpu.mmu.write(addr, value);
        dbg.cpu.mmu.clearIllegalOperation();
        return null;
    }

    pub const d = disassemble;
    pub fn disassemble(dbg: *Debugger, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
        const usage = "usage: disassemble [count] [addr]\n  count defaults to 10\n  addr defaults to PC\n";

        const count: u16 = blk: {
            break :blk std.fmt.parseInt(u16, it.next() orelse {
                break :blk 10;
            }, 0) catch {
                try writer.writeAll(usage);
                return null;
            };
        };

        var addr: u16 = blk: {
            break :blk std.fmt.parseInt(u16, it.next() orelse {
                break :blk dbg.cpu.reg.PC - 1; // we are at pc - 1 because the cpu is about to decode, so it has already fetched and increased pc
            }, 0) catch {
                try writer.writeAll(usage);
                return null;
            };
        };

        var memory: [10]u8 = undefined;
        dbg.cpu.mmu.dumpMemory(addr, &memory);
        for (0..count) |_| {
            if (dbg.cpu.reg.PC - 1 == addr) {
                try writer.print("[{X:0>4}]:  ", .{addr});
            } else {
                try writer.print("{X:0>4}:  ", .{addr});
            }
            const next = try assembler.formatNext(&memory, writer);
            const adv: u16 = 10 - @as(u16, @intCast(next.len));
            try writer.writeAll("  [ ");
            for (0..adv) |i| {
                try writer.print("{X:0>2} ", .{memory[i]});
            }
            try writer.writeAll("]\n");

            addr += adv;
            dbg.cpu.mmu.dumpMemory(addr, &memory);
        }

        return null;
    }

    pub const b = @"break";
    pub fn @"break"(dbg: *Debugger, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
        const usage =
            \\usage: break add addr
            \\       break list
            \\       break del idx
            \\
        ;

        const command: []const u8 = it.next() orelse {
            try writer.writeAll(usage);
            return null;
        };

        if (std.mem.eql(u8, "list", command)) {
            var i: u8 = 0;
            for (dbg.breakpoints) |brk| {
                if (brk) |bb| {
                    try writer.print("{d}. {X:0>4}\n", .{ i, bb });
                    i += 1;
                }
            }
            return null;
        }

        const add = std.mem.eql(u8, "add", command);
        if (!add) {
            if (!std.mem.eql(u8, "del", command)) {
                try writer.writeAll(usage);
                return null;
            }
        }

        const addr_or_idx: u16 = std.fmt.parseInt(u16, it.next() orelse {
            try writer.writeAll(usage);
            return null;
        }, 0) catch {
            try writer.writeAll(usage);
            return null;
        };

        if (add) {
            if (!dbg.add_breakpoint(addr_or_idx)) {
                try writer.writeAll("too many breakpoints, remove some\n");
            } else {
                try writer.print("add breakpoint on address 0x{X:0>4}\n", .{addr_or_idx});
            }
        } else {
            if (!dbg.remove_breakpoint(addr_or_idx)) {
                try writer.writeAll("breakpoint does not exist\n");
            } else {
                try writer.print("remove breakpoint on index {d}\n", .{addr_or_idx});
            }
        }

        return null;
    }

    pub const q = quit;
    pub fn quit(_: *Debugger, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
        return .should_stop;
    }

    pub fn _invalid_command(writer: anytype) !void {
        try writer.writeAll("invalid command\n");
    }

    pub fn _print_memory(dbg: *Debugger, writer: anytype, addr: u16, count: u16) !void {
        if (count == 1) {
            const val = dbg.cpu.mmu.getValue(addr);
            try writer.print("0x{X:0>2}", .{val});
            if (std.ascii.isPrint(val)) {
                try writer.print(" ({c})", .{val});
            }
            try writer.writeByte('\n');
            return;
        }

        var offset: u16 = 0;
        while (true) {
            try writer.print("{X:0>4}:  ", .{addr + offset});
            for (0..0x10) |idx| {
                const i: u16 = @intCast(idx);
                if (offset + i >= count) {
                    try writer.writeAll("   ");
                } else {
                    try writer.print("{X:0>2} ", .{dbg.cpu.mmu.getValue(addr + offset + i)});
                }
            }
            try writer.writeAll(" |");
            for (0..0x10) |idx| {
                const i: u16 = @intCast(idx);
                if (offset + i >= count - 1) {
                    try writer.writeAll(" ");
                } else {
                    const val = dbg.cpu.mmu.getValue(addr + offset + i);
                    if (std.ascii.isPrint(val)) {
                        try writer.print("{c}", .{val});
                    } else {
                        try writer.writeByte('.');
                    }
                }
            }
            try writer.writeAll("|\n");
            if (offset + 0xF >= count - 1) {
                break;
            }
            offset += 0x10;
        }
    }
};

pub const Debugger = struct {
    cpu: *Cpu,
    output: std.fs.File,

    stepping_cycles: bool = false,
    stepping: bool = false,
    breakpoints: [0x10]?u16 = [_]?u16{null} ** 0x10,

    pub fn init(cpu: *Cpu, output: std.fs.File) Debugger {
        return Debugger{
            .cpu = cpu,
            .output = output,
        };
    }

    fn add_breakpoint(self: *Debugger, addr: u16) bool {
        for (0..0x10) |i| {
            if (self.breakpoints[i] == null) {
                self.breakpoints[i] = addr;
                return true;
            }
        }
        return false;
    }

    fn remove_breakpoint(self: *Debugger, idx: u16) bool {
        var curr: u16 = 0;
        for (0..0x10) |i| {
            if (self.breakpoints[i] != null) {
                if (curr == idx) {
                    self.breakpoints[i] = null;
                    return true;
                }
                curr += 1;
            }
        }
        return false;
    }

    fn should_enter(self: *const Debugger) !bool {
        if (self.stepping_cycles) {
            return true;
        }

        if (!self.cpu.instructionBoundary()) {
            return false;
        }
        if (self.stepping) {
            return true;
        }
        if (self.cpu.breakpointHappened()) {
            return true;
        }
        if (self.cpu.illegalInstructionExecuted()) {
            return true;
        }
        if (self.cpu.mmu.illegalMemoryOperationHappened()) {
            return true;
        }

        for (self.breakpoints) |b| {
            if (b) |addr| {
                if (self.cpu.reg.PC == addr) {
                    return true;
                }
            }
        }

        return false;
    }

    fn clear_flags(self: *Debugger) void {
        self.stepping_cycles = false;
        self.stepping = false;
        self.cpu.clearBreakpoint();
        self.cpu.mmu.clearIllegalOperation();
    }

    pub fn enter(self: *Debugger) !DebuggerResult {
        var promptBuff = [_]u8{undefined} ** 256;
        const prompt = try std.fmt.bufPrintZ(&promptBuff, "[0x{X:0>4}] dbg> ", .{self.cpu.reg.PC});

        while (true) {
            const input = readline(prompt);
            if (input == null) continue;
            defer std.c.free(input);

            const line = std.mem.span(input.?);
            if (line.len == 0) continue;
            add_history(input.?);

            var it = std.mem.tokenizeScalar(u8, line, ' ');
            const command = it.next();
            if (command) |c| {
                if (try self.handle_command(c, &it, self.output.writer())) |ret| {
                    return ret;
                }
            }
        }

        return .should_continue;
    }

    fn handle_command(self: *Debugger, command: []const u8, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
        inline for (@typeInfo(Commands).@"struct".decls) |decl| {
            if (decl.name[0] == '_') continue;
            if (std.mem.eql(u8, decl.name, command)) {
                const func = @field(Commands, decl.name);
                return func(self, it, writer);
            }
        }
        try Commands._invalid_command(writer);
        return null;
    }

    pub fn enter_debugger_if_needed(self: *Debugger) !DebuggerResult {
        if (!try self.should_enter()) {
            return .should_continue;
        }
        self.clear_flags();
        return self.enter();
    }
};
