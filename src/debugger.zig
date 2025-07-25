const std = @import("std");
const assembler = @import("assembler.zig");
const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;

extern fn readline(prompt: [*:0]const u8) callconv(.C) ?[*:0]u8;
extern fn add_history(line: [*:0]const u8) callconv(.C) void;

pub const DebuggerResult = enum {
    should_continue,
    should_stop,
};

fn Commands(DebuggerImpl: type) type {
    return struct {
        pub fn help(_: *DebuggerImpl, _: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
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
        pub fn @"continue"(_: *DebuggerImpl, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
            return .should_continue;
        }

        pub const reg = registers;
        pub fn registers(dbg: *DebuggerImpl, _: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
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
        pub fn step(dbg: *DebuggerImpl, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
            dbg.stepping = true;
            return .should_continue;
        }

        pub const sc = stepc;
        pub fn stepc(dbg: *DebuggerImpl, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
            dbg.stepping_cycles = true;
            return .should_continue;
        }

        pub fn peek(dbg: *DebuggerImpl, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
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
            return null;
        }

        pub fn poke(dbg: *DebuggerImpl, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
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

            dbg.mmu.poke(addr, value);
            return null;
        }

        pub const d = disassemble;
        pub fn disassemble(dbg: *DebuggerImpl, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
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
            dbg.mmu.dumpMemory(addr, &memory);
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
                dbg.mmu.dumpMemory(addr, &memory);
            }

            return null;
        }

        pub const b = @"break";
        pub fn @"break"(dbg: *DebuggerImpl, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
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
        pub fn quit(_: *DebuggerImpl, _: *std.mem.TokenIterator(u8, .scalar), _: anytype) !?DebuggerResult {
            return .should_stop;
        }

        pub fn _invalid_command(writer: anytype) !void {
            try writer.writeAll("invalid command\n");
        }

        pub fn _print_memory(dbg: *DebuggerImpl, writer: anytype, addr: u16, count: u16) !void {
            if (count == 1) {
                const val = dbg.mmu.peek(addr);
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
                        try writer.print("{X:0>2} ", .{dbg.mmu.peek(addr + offset + i)});
                    }
                }
                try writer.writeAll(" |");
                for (0..0x10) |idx| {
                    const i: u16 = @intCast(idx);
                    if (offset + i >= count - 1) {
                        try writer.writeAll(" ");
                    } else {
                        const val = dbg.mmu.peek(addr + offset + i);
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
}

pub fn Debugger(Cpu: type, Mmu: type, Writer: type) type {
    return struct {
        const This = @This();

        cpu: *Cpu,
        mmu: *Mmu,
        output: Writer,

        stepping_cycles: bool,
        stepping: bool,
        breakpoints: [0x10]?u16,

        pub inline fn init(cpu: *Cpu, mmu: *Mmu, output: Writer) This {
            return This{
                .cpu = cpu,
                .mmu = mmu,
                .output = output,
                .stepping_cycles = false,
                .stepping = false,
                .breakpoints = [_]?u16{null} ** 0x10,
            };
        }

        fn add_breakpoint(self: *This, addr: u16) bool {
            for (0..0x10) |i| {
                if (self.breakpoints[i] == null) {
                    self.breakpoints[i] = addr;
                    return true;
                }
            }
            return false;
        }

        fn remove_breakpoint(self: *This, idx: u16) bool {
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

        fn should_enter(self: *const This) !bool {
            if (self.stepping_cycles) {
                return true;
            }

            if (!self.cpu.isInstructionBoundary()) {
                return false;
            }
            if (self.stepping) {
                return true;
            }
            if (self.cpu.flags.breakpoint) {
                return true;
            }
            if (self.cpu.flags.illegal) {
                return true;
            }
            if (self.mmu.flags.any()) {
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

        fn clear_flags(self: *This) void {
            self.stepping_cycles = false;
            self.stepping = false;
            self.cpu.flags.breakpoint = false;
            self.mmu.flags = .{};
        }

        pub fn enter(self: *This) !DebuggerResult {
            var promptBuff = [_]u8{undefined} ** 256;
            // show pc - 1 as that is the currently fetched opcode's location
            const prompt = try std.fmt.bufPrintZ(&promptBuff, "[0x{X:0>4}] dbg> ", .{self.cpu.reg.PC - 1});

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
                    if (try self.handle_command(c, &it, self.output)) |ret| {
                        return ret;
                    }
                }
            }

            return .should_continue;
        }

        fn handle_command(self: *This, command: []const u8, it: *std.mem.TokenIterator(u8, .scalar), writer: anytype) !?DebuggerResult {
            inline for (@typeInfo(Commands(This)).@"struct".decls) |decl| {
                if (decl.name[0] == '_') continue;
                if (std.mem.eql(u8, decl.name, command)) {
                    const func = @field(Commands(This), decl.name);
                    return func(self, it, writer);
                }
            }
            try Commands(This)._invalid_command(writer);
            return null;
        }

        pub fn enter_debugger_if_needed(self: *This) !DebuggerResult {
            if (!try self.should_enter()) {
                return .should_continue;
            }
            self.clear_flags();
            return self.enter();
        }
    };
}

const MockMmu = struct {
    flags: MemoryFlag = .{},
    pub fn poke(_: *MockMmu, _: u16, _: u8) void {}
};

const MockCpu = struct {
    const This = @This();

    reg: struct {
        PC: u16 = 0,
    } = .{},
    flags: struct {
        illegal: bool = false,
        breakpoint: bool = false,
    } = .{},
    mmu: *MockMmu,

    pub fn isInstructionBoundary(_: *This) bool {
        return true;
    }

    pub fn clearBreakpoint(_: *This) void {}
};

const MockedDebugger = Debugger(MockCpu, MockMmu, @TypeOf(std.io.null_writer));
const Cmds = Commands(MockedDebugger);

test "Debugger add and remove breakpoints" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    try std.testing.expect(dbg.add_breakpoint(0x1234));
    try std.testing.expect(dbg.add_breakpoint(0x5678));

    try std.testing.expectEqual(dbg.breakpoints[0].?, 0x1234);
    try std.testing.expectEqual(dbg.breakpoints[1].?, 0x5678);

    try std.testing.expect(dbg.remove_breakpoint(0)); // Remove the first breakpoint
    try std.testing.expectEqual(dbg.breakpoints[0], null);
    try std.testing.expectEqual(dbg.breakpoints[1].?, 0x5678);

    try std.testing.expect(!dbg.remove_breakpoint(5)); // Invalid index removal
}

test "Debugger clears flags correctly" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    dbg.stepping = true;
    dbg.stepping_cycles = true;
    dbg.mmu.flags = .{ .illegal = true };

    dbg.clear_flags();

    try std.testing.expect(!dbg.stepping);
    try std.testing.expect(!dbg.stepping_cycles);
    try std.testing.expect(!dbg.mmu.flags.any());
}

test "Debugger should_enter triggers on stepping" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    dbg.stepping = true;

    try std.testing.expect(try dbg.should_enter());
}

test "Debugger should_enter triggers on stepping_cycles" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    dbg.stepping_cycles = true;

    try std.testing.expect(try dbg.should_enter());
}

test "Debugger should_enter triggers on breakpoint match" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    try std.testing.expect(dbg.add_breakpoint(0x0042));

    fake_cpu.reg.PC = 0x0042;

    try std.testing.expect(try dbg.should_enter());
}

test "Debugger should_enter triggers on memory flags" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    dbg.mmu.flags = .{ .uninitialized = true };

    try std.testing.expect(try dbg.should_enter());
}

test "Commands: help command outputs text" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    var buffer: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);

    const input = "help";
    var it = std.mem.tokenizeScalar(u8, input, ' ');

    const result = try Cmds.help(&dbg, &it, stream.writer());
    try std.testing.expectEqual(null, result);
    try std.testing.expect(std.mem.indexOf(u8, stream.getWritten(), "continue") != null);
}

test "Commands: step command sets stepping flag" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    const input = "step";
    var it = std.mem.tokenizeScalar(u8, input, ' ');

    const result = try Cmds.step(&dbg, &it, std.io.null_writer);
    try std.testing.expectEqual(DebuggerResult.should_continue, result.?);
    try std.testing.expect(dbg.stepping);
}

test "Commands: stepc command sets stepping_cycles flag" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    const input = "stepc";
    var it = std.mem.tokenizeScalar(u8, input, ' ');

    const result = try Cmds.stepc(&dbg, &it, std.io.null_writer);
    try std.testing.expectEqual(DebuggerResult.should_continue, result.?);
    try std.testing.expect(dbg.stepping_cycles);
}

test "Commands: quit command returns should_stop" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    const input = "quit";
    var it = std.mem.tokenizeScalar(u8, input, ' ');

    const result = try Cmds.quit(&dbg, &it, std.io.null_writer);
    try std.testing.expectEqual(DebuggerResult.should_stop, result.?);
}

test "Commands: break add, list, and del" {
    var fake_mmu = MockMmu{};
    var fake_cpu = MockCpu{ .mmu = &fake_mmu };
    var dbg = MockedDebugger.init(&fake_cpu, &fake_mmu, std.io.null_writer);

    // Add breakpoint
    const add_input = "add 0x0042";
    var add_it = std.mem.tokenizeScalar(u8, add_input, ' ');

    const add_result = try Cmds.b(&dbg, &add_it, std.io.null_writer);
    try std.testing.expectEqual(null, add_result);
    try std.testing.expect(dbg.breakpoints[0] != null);

    // List breakpoints
    const list_input = "list";
    var list_it = std.mem.tokenizeScalar(u8, list_input, ' ');

    var buffer: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);

    const list_result = try Cmds.b(&dbg, &list_it, stream.writer());
    try std.testing.expectEqual(null, list_result);
    try std.testing.expect(std.mem.indexOf(u8, stream.getWritten(), "0042") != null);

    // Delete breakpoint
    const del_input = "del 0";
    var del_it = std.mem.tokenizeScalar(u8, del_input, ' ');

    const del_result = try Cmds.b(&dbg, &del_it, std.io.null_writer);
    try std.testing.expectEqual(null, del_result);
    try std.testing.expect(dbg.breakpoints[0] == null);
}
