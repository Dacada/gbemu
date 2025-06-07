const std = @import("std");
const Cpu = @import("cpu.zig").Cpu;

extern fn readline(prompt: [*:0]const u8) callconv(.C) ?[*:0]u8;
extern fn add_history(line: [*:0]const u8) callconv(.C) void;

pub const DebuggerResult = enum {
    should_continue,
    should_stop,
};

const Commands = struct {
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

    pub fn quit(_: *Debugger, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
        return .should_stop;
    }

    pub fn _invalid_command(writer: anytype) !void {
        try writer.writeAll("invalid command\n");
    }
};

pub const Debugger = struct {
    cpu: *Cpu,
    output: std.fs.File,

    stepping_cycles: bool = false,
    stepping: bool = false,

    pub fn init(cpu: *Cpu, output: std.fs.File) Debugger {
        return Debugger{
            .cpu = cpu,
            .output = output,
        };
    }

    fn should_enter(self: *const Debugger) !bool {
        const writer = self.output.writer();

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
            try writer.writeAll("Hit software breakpoint!\n");
            return true;
        }
        if (self.cpu.illegalInstructionExecuted()) {
            try writer.writeAll("Illegal instruction executed!\n");
            return true;
        }
        if (self.cpu.mmu.illegalMemoryOperationHappened()) {
            try writer.writeAll("Illegal memory operation!\n");
            return true;
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
        inline for (@typeInfo(Commands).Struct.decls) |decl| {
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
