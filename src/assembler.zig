const std = @import("std");
const builtin = @import("builtin");

const logger = if (builtin.is_test)
    struct {
        pub fn err(comptime _: []const u8, _: anytype) void {}
        pub fn warn(comptime _: []const u8, _: anytype) void {}
        pub fn info(comptime _: []const u8, _: anytype) void {}
        pub fn debug(comptime _: []const u8, _: anytype) void {}
    }
else
    std.log.scoped(.assembler);

pub const AssemblerError = error{
    InvalidInstruction,
    MissmatchedParenthesis,
    RedefinedLabel,
    InvalidArgumentCount,
    InvalidArgument,
    InvalidLabel,
    InvalidInstructionArguments,
    UndefinedLabel,
};

pub const TokenKind = enum {
    Label,
    Instruction,
    Argument,
};

pub const Token = struct {
    line: usize,
    source: []const u8,
    which: TokenKind,
};

fn skip_characters(input: []const u8, pred: fn (u8) bool) usize {
    var i: usize = 0;
    while (input.len > i and pred(input[i])) {
        i += 1;
    }
    return i;
}

fn is_char_valid(char: u8) bool {
    if (char == ';') {
        return false;
    }
    return !std.ascii.isWhitespace(char);
}

fn skip_valid_chars(input: []const u8) usize {
    return skip_characters(input, is_char_valid);
}

fn skip_whitespace(input: []const u8) usize {
    return skip_characters(input, std.ascii.isWhitespace);
}

fn read_one_token(input: []const u8) struct { []const u8, []const u8 } {
    const i = skip_whitespace(input);
    if (input.len <= i or input[i] == ';') {
        return .{ input[i..i], input[i..i] };
    }

    const j = i + skip_valid_chars(input[i..]);
    return .{ input[i..j], input[j..] };
}

pub fn lexer(input: []const u8, allocator: std.mem.Allocator) ![]Token {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var it = std.mem.splitScalar(u8, input, '\n');
    var idx: usize = 0;
    while (it.next()) |line| {
        defer idx += 1;
        var rest: []const u8 = line;
        while (rest.len > 0) {
            var token, rest = read_one_token(rest);
            if (token.len == 0) {
                break;
            } else {
                const which = if (token[token.len - 1] == ':')
                    TokenKind.Label
                else
                    TokenKind.Instruction;
                if (which == TokenKind.Label) {
                    token = token[0 .. token.len - 1];
                }
                try tokens.append(Token{
                    .line = idx + 1,
                    .source = token,
                    .which = which,
                });
                if (which == TokenKind.Instruction) {
                    break;
                }
            }
        }
        while (rest.len > 0) {
            var token, rest = read_one_token(rest);
            if (token.len == 0) {
                break;
            } else {
                if (token[token.len - 1] == ',') {
                    token = token[0 .. token.len - 1];
                }
                if (token.len != 0) {
                    try tokens.append(Token{
                        .line = idx + 1,
                        .source = token,
                        .which = TokenKind.Argument,
                    });
                }
            }
        }
    }

    return tokens.toOwnedSlice();
}

test "lexer" {
    const input =
        \\LABEL1: LABEL2: LABEL3: MOV R1, R2;
        \\START: MOV R1, #10
        \\MOV R1 #10
        \\   LABEL1:       MOV    R1   ,   #10
        \\MOV R1, #10 ; This is a comment
        \\; This whole line is a comment
        \\LABEL1: JMP LABEL2 ; Inline comment after a label
        \\LABEL1:  ; A label with no instruction
        \\LABEL1: LABEL2: .directive
        \\NOP
        \\LOAD R1, [0x1000]
        \\TEST$: JMP LOOP_123
        \\MOV R1, #10 ADD R2, R1, #5 JMP END
    ;
    const expected = [_]Token{
        Token{ .line = 1, .source = "LABEL1", .which = TokenKind.Label },
        Token{ .line = 1, .source = "LABEL2", .which = TokenKind.Label },
        Token{ .line = 1, .source = "LABEL3", .which = TokenKind.Label },
        Token{ .line = 1, .source = "MOV", .which = TokenKind.Instruction },
        Token{ .line = 1, .source = "R1", .which = TokenKind.Argument },
        Token{ .line = 1, .source = "R2", .which = TokenKind.Argument },
        Token{ .line = 2, .source = "START", .which = TokenKind.Label },
        Token{ .line = 2, .source = "MOV", .which = TokenKind.Instruction },
        Token{ .line = 2, .source = "R1", .which = TokenKind.Argument },
        Token{ .line = 2, .source = "#10", .which = TokenKind.Argument },
        Token{ .line = 3, .source = "MOV", .which = TokenKind.Instruction },
        Token{ .line = 3, .source = "R1", .which = TokenKind.Argument },
        Token{ .line = 3, .source = "#10", .which = TokenKind.Argument },
        Token{ .line = 4, .source = "LABEL1", .which = TokenKind.Label },
        Token{ .line = 4, .source = "MOV", .which = TokenKind.Instruction },
        Token{ .line = 4, .source = "R1", .which = TokenKind.Argument },
        Token{ .line = 4, .source = "#10", .which = TokenKind.Argument },
        Token{ .line = 5, .source = "MOV", .which = TokenKind.Instruction },
        Token{ .line = 5, .source = "R1", .which = TokenKind.Argument },
        Token{ .line = 5, .source = "#10", .which = TokenKind.Argument },
        Token{ .line = 7, .source = "LABEL1", .which = TokenKind.Label },
        Token{ .line = 7, .source = "JMP", .which = TokenKind.Instruction },
        Token{ .line = 7, .source = "LABEL2", .which = TokenKind.Argument },
        Token{ .line = 8, .source = "LABEL1", .which = TokenKind.Label },
        Token{ .line = 9, .source = "LABEL1", .which = TokenKind.Label },
        Token{ .line = 9, .source = "LABEL2", .which = TokenKind.Label },
        Token{ .line = 9, .source = ".directive", .which = TokenKind.Instruction },
        Token{ .line = 10, .source = "NOP", .which = TokenKind.Instruction },
        Token{ .line = 11, .source = "LOAD", .which = TokenKind.Instruction },
        Token{ .line = 11, .source = "R1", .which = TokenKind.Argument },
        Token{ .line = 11, .source = "[0x1000]", .which = TokenKind.Argument },
        Token{ .line = 12, .source = "TEST$", .which = TokenKind.Label },
        Token{ .line = 12, .source = "JMP", .which = TokenKind.Instruction },
        Token{ .line = 12, .source = "LOOP_123", .which = TokenKind.Argument },
        Token{ .line = 13, .source = "MOV", .which = TokenKind.Instruction },
        Token{ .line = 13, .source = "R1", .which = TokenKind.Argument },
        Token{ .line = 13, .source = "#10", .which = TokenKind.Argument },
        Token{ .line = 13, .source = "ADD", .which = TokenKind.Argument },
        Token{ .line = 13, .source = "R2", .which = TokenKind.Argument },
        Token{ .line = 13, .source = "R1", .which = TokenKind.Argument },
        Token{ .line = 13, .source = "#5", .which = TokenKind.Argument },
        Token{ .line = 13, .source = "JMP", .which = TokenKind.Argument },
        Token{ .line = 13, .source = "END", .which = TokenKind.Argument },
    };

    const actual = try lexer(input, std.testing.allocator);
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualDeep(&expected, actual);
}

fn toUpper(input: []const u8, allocator: std.mem.Allocator) ![]u8 {
    var upper: []u8 = try allocator.alloc(u8, input.len);
    for (input, 0..) |c, i| {
        upper[i] = if (c >= 'a' and c <= 'z') c - ('a' - 'A') else c;
    }
    return upper;
}

test "toUpper" {
    const value = "-SoME str1ng_";
    const expected = "-SOME STR1NG_";

    const actual = try toUpper(value, std.testing.allocator);
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualSlices(u8, expected, actual);
}

fn validateLabel(source: []const u8, line: usize) !void {
    if (source[0] != '_' and !std.ascii.isAlphabetic(source[0])) {
        logger.err("Line {d}: Invalid label '{s}'.", .{ line, source });
        return AssemblerError.InvalidLabel;
    }
    if (std.meta.stringToEnum(Register8, source) != null) {
        logger.err("Line {d}: Invalid label '{s}' is actually a Register8", .{ line, source });
        return AssemblerError.InvalidLabel;
    }
    if (std.meta.stringToEnum(Register16, source) != null) {
        logger.err("Line {d}: Invalid label '{s}' is actually a Register16", .{ line, source });
        return AssemblerError.InvalidLabel;
    }
    if (std.meta.stringToEnum(Condition, source) != null) {
        logger.err("Line {d}: Invalid label '{s}' is actually a Condition", .{ line, source });
        return AssemblerError.InvalidLabel;
    }
    for (source) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') {
            logger.err("Line {d}: Invalid label '{s}'.", .{ line, source });
            return AssemblerError.InvalidLabel;
        }
    }
}

test "validateLabel incorrect 1" {
    const expected = AssemblerError.InvalidLabel;
    const actual = validateLabel("-0x1", 0);
    try std.testing.expectError(expected, actual);
}

test "validateLabel incorrect 2" {
    const expected = AssemblerError.InvalidLabel;
    const actual = validateLabel("_0$1", 0);
    try std.testing.expectError(expected, actual);
}

test "validateLabel correct" {
    try validateLabel("_01", 0);
}

const Instruction = enum { LD, LDH, PUSH, POP, ADD, ADC, SUB, SBC, CP, INC, DEC, AND, OR, XOR, CCF, SCF, DAA, CPL, RLCA, RRCA, RLA, RRA, RLC, RRC, RL, RR, SLA, SRA, SWAP, SRL, BIT, RES, SET, JP, JR, CALL, RET, RETI, RST, HALT, STOP, DI, EI, NOP };
const Register8 = enum { A, B, C, D, E, F, H, L };
const Register16 = enum { AF, BC, DE, HL, SP };
const Condition = enum { NZ, Z, NC, C };
const BasicArgumentType = enum {
    Immediate,
    Reserved,
    Label,
};

const Argument = struct {
    arg: union(BasicArgumentType) {
        Immediate: i32,
        Reserved: []const u8,
        Label: []const u8,
    },

    indirect: bool,
    incDec: ?bool,
    offset: ?i8,

    fn format(self: *const Argument, writer: anytype) !void {
        if (self.indirect) {
            try writer.writeByte('(');
        }
        switch (self.arg) {
            .Immediate => |n| {
                if (n >= 0) {
                    try writer.print("0x{X}", .{n});
                } else {
                    try writer.print("{d}", .{n});
                }
            },
            .Reserved, .Label => |s| try writer.writeAll(s),
        }
        if (self.incDec) |incDec| {
            if (incDec) {
                try writer.writeByte('+');
            } else {
                try writer.writeByte('-');
            }
        }
        if (self.offset) |off| {
            if (off < 0) {
                try writer.print("{d}", .{off});
            } else {
                try writer.print("+{d}", .{off});
            }
        }
        if (self.indirect) {
            try writer.writeByte(')');
        }
    }

    fn parseIndirect(input: []const u8) !struct { bool, []const u8 } {
        if (input[0] == '(') {
            if (input[input.len - 1] != ')') {
                return AssemblerError.MissmatchedParenthesis;
            }
            return .{
                true,
                input[1..(input.len - 1)],
            };
        } else {
            return .{
                false,
                input,
            };
        }
    }

    fn parseIncDec(input: []const u8) struct { ?bool, []const u8 } {
        return switch (input[input.len - 1]) {
            '+' => .{
                true,
                input[0..(input.len - 1)],
            },
            '-' => .{
                false,
                input[0..(input.len - 1)],
            },
            else => .{
                null,
                input,
            },
        };
    }

    fn parseOffset(input: []const u8) struct { ?i8, []const u8 } {
        if (input[0] == '+' or input[0] == '-') {
            return .{ null, input };
        }

        var idx: ?usize = null;
        for (input, 0..) |c, i| {
            if (c == '+' or c == '-') {
                idx = i;
                break;
            }
        }

        if (idx == null) {
            return .{ null, input };
        }

        const offset = parseImmediate(input[idx.?..]);
        if (offset == null) {
            return .{ null, input };
        }

        if (offset.? < std.math.minInt(i8) or offset.? > std.math.maxInt(i8)) {
            return .{ null, input };
        }

        return .{ @intCast(offset.?), input[0..idx.?] };
    }

    fn parseReg8(input: []const u8) ?Register8 {
        return std.meta.stringToEnum(Register8, input);
    }

    fn parseReg16(input: []const u8) ?Register16 {
        return std.meta.stringToEnum(Register16, input);
    }

    fn parseCondition(input: []const u8) ?Condition {
        return std.meta.stringToEnum(Condition, input);
    }

    fn parseBase(input: []const u8) struct { u8, []const u8 } {
        var base: u8 = 10;
        var start: usize = 0;
        if (input[0] == '0' and input.len >= 2) {
            if (input[1] == 'B') {
                base = 2;
            } else if (input[1] == 'X') {
                base = 16;
            } else if (input[1] == 'O') {
                base = 8;
            }
            start = 2;
        }
        return .{ base, input[start..] };
    }

    fn parseImmediate(input: []const u8) ?i32 {
        const base, const rest = Argument.parseBase(input);
        return std.fmt.parseInt(i32, rest, base) catch null;
    }

    fn fromTokenInner(token: []const u8, allocator: std.mem.Allocator) !Argument {
        var source = token;

        const indirect, source = try Argument.parseIndirect(source);
        const incDec, source = Argument.parseIncDec(source);
        const offset, source = Argument.parseOffset(source);

        if (Argument.parseReg8(source) != null or Argument.parseReg16(source) != null or Argument.parseCondition(source) != null) {
            const condition_text = try allocator.alloc(u8, source.len);
            @memcpy(condition_text, source);
            return Argument{
                .arg = .{
                    .Reserved = condition_text,
                },
                .indirect = indirect,
                .incDec = incDec,
                .offset = offset,
            };
        }

        if (Argument.parseImmediate(source)) |imm| {
            return Argument{
                .arg = .{
                    .Immediate = imm,
                },
                .indirect = indirect,
                .incDec = incDec,
                .offset = offset,
            };
        }

        const label_text = try allocator.alloc(u8, source.len);
        @memcpy(label_text, source);
        return Argument{
            .arg = .{
                .Label = label_text,
            },
            .indirect = indirect,
            .incDec = incDec,
            .offset = offset,
        };
    }

    fn fromToken(token: Token, allocator: std.mem.Allocator) !Argument {
        const token_upper = try toUpper(token.source, allocator);
        defer allocator.free(token_upper);
        const res = Argument.fromTokenInner(token_upper, allocator) catch |e| {
            if (e == AssemblerError.MissmatchedParenthesis) {
                logger.err("Line {d}: Missmatched parenthesis '{s}'", .{ token.line, token.source });
            }
            return e;
        };
        return res;
    }

    fn free(self: *const Argument, allocator: std.mem.Allocator) void {
        switch (self.arg) {
            .Reserved => |r| allocator.free(r),
            .Label => |l| allocator.free(l),
            else => {},
        }
    }
};

test "parseIndirect 1" {
    const input = "(foo)";
    const expected1 = true;
    const expected2 = "foo";

    const actual1, const actual2 = try Argument.parseIndirect(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseIndirect 2" {
    const input = "foo";
    const expected1 = false;
    const expected2 = "foo";

    const actual1, const actual2 = try Argument.parseIndirect(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqual(expected2, actual2);
}

test "parseIndirect 3" {
    const input = "(foo";
    const expected = AssemblerError.MissmatchedParenthesis;

    const actual = Argument.parseIndirect(input);

    try std.testing.expectError(expected, actual);
}

test "parseIncDec 1" {
    const input = "foo+";
    const expected1 = true;
    const expected2 = "foo";

    const actual1, const actual2 = Argument.parseIncDec(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseIncDec 2" {
    const input = "foo-";
    const expected1 = false;
    const expected2 = "foo";

    const actual1, const actual2 = Argument.parseIncDec(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseIncDec 3" {
    const input = "foo";
    const expected1: ?bool = null;
    const expected2 = "foo";

    const actual1, const actual2 = Argument.parseIncDec(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseReg8 1" {
    const input = "B";
    const expected = Register8.B;

    const actual = Argument.parseReg8(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg8 2" {
    const input = "X";
    const expected: ?Register8 = null;

    const actual = Argument.parseReg8(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg16 1" {
    const input = "HL";
    const expected = Register16.HL;

    const actual = Argument.parseReg16(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg16 2" {
    const input = "XX";
    const expected: ?Register16 = null;

    const actual = Argument.parseReg16(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseCondition 1" {
    const input = "NC";
    const expected = Condition.NC;

    const actual = Argument.parseCondition(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseCondition 2" {
    const input = "XX";
    const expected: ?Condition = null;

    const actual = Argument.parseCondition(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseOffset 1" {
    const input = "A+3";
    const expected1: ?i8 = 3;
    const expected2 = "A";

    const actual1, const actual2 = Argument.parseOffset(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseOffset 2" {
    const input = "A-3";
    const expected1: ?i8 = -3;
    const expected2 = "A";

    const actual1, const actual2 = Argument.parseOffset(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseOffset 3" {
    const input = "-3";
    const expected1: ?i8 = null;
    const expected2 = "-3";

    const actual1, const actual2 = Argument.parseOffset(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseOffset 4" {
    const input = "+3";
    const expected1: ?i8 = null;
    const expected2 = "+3";

    const actual1, const actual2 = Argument.parseOffset(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseOffset 5" {
    const input = "A";
    const expected1: ?i8 = null;
    const expected2 = "A";

    const actual1, const actual2 = Argument.parseOffset(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseOffset 6" {
    const input = "A+999";
    const expected1: ?i8 = null;
    const expected2 = "A+999";

    const actual1, const actual2 = Argument.parseOffset(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseBase 1" {
    const input = "1234";
    const expected1: u8 = 10;
    const expected2 = "1234";

    const actual1, const actual2 = Argument.parseBase(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseBase 2" {
    const input = "0B1234";
    const expected1: u8 = 2;
    const expected2 = "1234";

    const actual1, const actual2 = Argument.parseBase(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseBase 3" {
    const input = "0X1234";
    const expected1: u8 = 16;
    const expected2 = "1234";

    const actual1, const actual2 = Argument.parseBase(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseBase 4" {
    const input = "0O1234";
    const expected1: u8 = 8;
    const expected2 = "1234";

    const actual1, const actual2 = Argument.parseBase(input);

    try std.testing.expectEqual(expected1, actual1);
    try std.testing.expectEqualSlices(u8, expected2, actual2);
}

test "parseImmediate 1" {
    const input = "0X100000000";
    const expected: ?i32 = null;

    const actual = Argument.parseImmediate(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 2" {
    const input = "-50000000000000000";
    const expected: ?i32 = null;

    const actual = Argument.parseImmediate(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 3" {
    const input = "0XFFFF";
    const expected: ?i32 = 0xFFFF;

    const actual = Argument.parseImmediate(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 4" {
    const input = "0XFFFF";
    const expected: ?i32 = 0xFFFF;

    const actual = Argument.parseImmediate(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 5" {
    const input = "0XFF";
    const expected: ?i32 = 0xFF;

    const actual = Argument.parseImmediate(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 6" {
    const input = "-100";
    const expected: ?i32 = -100;

    const actual = Argument.parseImmediate(input);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 7" {
    const input = "100";
    const expected: ?i32 = 100;

    const actual = Argument.parseImmediate(input);

    try std.testing.expectEqual(expected, actual);
}

test "formatArgument 1" {
    const arg = Argument{
        .arg = .{
            .Immediate = 0x1234,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try arg.format(writer);
    try std.testing.expectEqualStrings("0x1234", source.buffer.getWritten());
}

test "formatArgument 2" {
    const arg = Argument{
        .arg = .{
            .Immediate = 0x1234,
        },
        .indirect = true,
        .incDec = null,
        .offset = null,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try arg.format(writer);
    try std.testing.expectEqualStrings("(0x1234)", source.buffer.getWritten());
}

test "formatArgument 3" {
    const arg = Argument{
        .arg = .{
            .Reserved = "BC",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try arg.format(writer);
    try std.testing.expectEqualStrings("BC", source.buffer.getWritten());
}

test "formatArgument 4" {
    const arg = Argument{
        .arg = .{
            .Reserved = "BC",
        },
        .indirect = true,
        .incDec = null,
        .offset = null,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try arg.format(writer);
    try std.testing.expectEqualStrings("(BC)", source.buffer.getWritten());
}

test "formatArgument 5" {
    const arg = Argument{
        .arg = .{
            .Reserved = "BC",
        },
        .indirect = true,
        .incDec = true,
        .offset = null,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try arg.format(writer);
    try std.testing.expectEqualStrings("(BC+)", source.buffer.getWritten());
}

test "formatArgument 6" {
    const arg = Argument{
        .arg = .{
            .Reserved = "BC",
        },
        .indirect = true,
        .incDec = false,
        .offset = null,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try arg.format(writer);
    try std.testing.expectEqualStrings("(BC-)", source.buffer.getWritten());
}

test "formatArgument 7" {
    const arg = Argument{
        .arg = .{
            .Reserved = "BC",
        },
        .indirect = false,
        .incDec = null,
        .offset = -7,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try arg.format(writer);
    try std.testing.expectEqualStrings("BC-7", source.buffer.getWritten());
}

const Register8BitArgumentDefinition = union(enum) {
    Offset: usize,
    Register: Register8,
};

const Register16BitOffsetVariety = union(enum) {
    LikeLD: usize,
    LikePUSH: usize,
};

const Register16BitArgumentDefinition = union(enum) {
    Offset: Register16BitOffsetVariety,
    Register: Register16,
};

const DefinedArgumentType = enum {
    Immediate3Bit,
    Immediate8Bit,
    Immediate8BitSigned,
    Immediate16Bit,
    ImmediateBitIndex,
    Register8Bit,
    Register16Bit,
    Register16BitWithOffset,
    Condition,
    IndirectImmediate8Bit,
    IndirectImmediate16Bit,
    IndirectRegister8Bit,
    IndirectRegister16Bit,
    IndirectRegister16BitInc,
    IndirectRegister16BitDec,
};

const ArgumentData = union(DefinedArgumentType) {
    Immediate3Bit: u3,
    Immediate8Bit: u8,
    Immediate8BitSigned: i8,
    Immediate16Bit: u16,
    ImmediateBitIndex: u3,
    Register8Bit: Register8,
    Register16Bit: Register16,
    Register16BitWithOffset: struct { offset: i8, reg: Register16 },
    Condition: Condition,
    IndirectImmediate8Bit: u8,
    IndirectImmediate16Bit: u16,
    IndirectRegister8Bit: Register8,
    IndirectRegister16Bit: Register16,
    IndirectRegister16BitInc: Register16,
    IndirectRegister16BitDec: Register16,

    fn fromDefinition(parsed: Argument, definition: DefinedArgumentType) ?ArgumentData {
        switch (definition) {
            DefinedArgumentType.Immediate3Bit => {
                if (parsed.arg == BasicArgumentType.Immediate and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    const imm = parsed.arg.Immediate;
                    if (imm >= std.math.minInt(u3) and imm <= std.math.maxInt(u3)) {
                        return ArgumentData{
                            .Immediate3Bit = @intCast(imm),
                        };
                    }
                }
            },
            DefinedArgumentType.Immediate8Bit => {
                if (parsed.arg == BasicArgumentType.Immediate and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    const imm = parsed.arg.Immediate;
                    if (imm >= std.math.minInt(u8) and imm <= std.math.maxInt(u8)) {
                        return ArgumentData{
                            .Immediate8Bit = @intCast(imm),
                        };
                    }
                }
            },
            DefinedArgumentType.Immediate8BitSigned => {
                if (parsed.arg == BasicArgumentType.Immediate and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    const imm = parsed.arg.Immediate;
                    if (imm >= std.math.minInt(i8) and imm <= std.math.maxInt(i8)) {
                        return ArgumentData{
                            .Immediate8BitSigned = @intCast(imm),
                        };
                    }
                }
            },
            DefinedArgumentType.Immediate16Bit => {
                if (parsed.arg == BasicArgumentType.Immediate and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    const imm = parsed.arg.Immediate;
                    if (imm >= std.math.minInt(u16) and imm <= std.math.maxInt(u16)) {
                        return ArgumentData{
                            .Immediate16Bit = @intCast(imm),
                        };
                    }
                }
                if (parsed.arg == BasicArgumentType.Label and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    return ArgumentData{
                        .Immediate16Bit = 0xAAAA,
                    };
                }
            },
            DefinedArgumentType.ImmediateBitIndex => {
                if (parsed.arg == BasicArgumentType.Immediate and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    const imm = parsed.arg.Immediate;
                    if (imm >= std.math.minInt(u3) and imm <= std.math.maxInt(u3)) {
                        return ArgumentData{
                            .ImmediateBitIndex = @intCast(imm),
                        };
                    }
                }
            },
            DefinedArgumentType.Register8Bit => {
                if (parsed.arg == BasicArgumentType.Reserved and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    if (Argument.parseReg8(parsed.arg.Reserved)) |reg| {
                        return ArgumentData{
                            .Register8Bit = reg,
                        };
                    }
                }
            },
            DefinedArgumentType.Register16Bit => {
                if (parsed.arg == BasicArgumentType.Reserved and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    if (Argument.parseReg16(parsed.arg.Reserved)) |reg| {
                        return ArgumentData{
                            .Register16Bit = reg,
                        };
                    }
                }
            },
            DefinedArgumentType.Register16BitWithOffset => {
                if (parsed.arg == BasicArgumentType.Reserved and !parsed.indirect and parsed.incDec == null and parsed.offset != null) {
                    if (Argument.parseReg16(parsed.arg.Reserved)) |reg| {
                        return ArgumentData{
                            .Register16BitWithOffset = .{
                                .offset = parsed.offset.?,
                                .reg = reg,
                            },
                        };
                    }
                }
            },
            DefinedArgumentType.Condition => {
                if (parsed.arg == BasicArgumentType.Reserved and !parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    if (Argument.parseCondition(parsed.arg.Reserved)) |cond| {
                        return ArgumentData{
                            .Condition = cond,
                        };
                    }
                }
            },
            DefinedArgumentType.IndirectImmediate8Bit => {
                if (parsed.arg == BasicArgumentType.Immediate and parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    const imm = parsed.arg.Immediate;
                    if (imm >= std.math.minInt(u8) and imm <= std.math.maxInt(u8)) {
                        return ArgumentData{
                            .IndirectImmediate8Bit = @intCast(imm),
                        };
                    }
                }
            },
            DefinedArgumentType.IndirectImmediate16Bit => {
                if (parsed.arg == BasicArgumentType.Immediate and parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    const imm = parsed.arg.Immediate;
                    if (imm >= std.math.minInt(u16) and imm <= std.math.maxInt(u16)) {
                        return ArgumentData{
                            .IndirectImmediate16Bit = @intCast(imm),
                        };
                    }
                }
                if (parsed.arg == BasicArgumentType.Label and parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    return ArgumentData{
                        .IndirectImmediate16Bit = 0xAAAA,
                    };
                }
            },
            DefinedArgumentType.IndirectRegister8Bit => {
                if (parsed.arg == BasicArgumentType.Reserved and parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    if (Argument.parseReg8(parsed.arg.Reserved)) |reg| {
                        return ArgumentData{
                            .IndirectRegister8Bit = reg,
                        };
                    }
                }
            },
            DefinedArgumentType.IndirectRegister16Bit => {
                if (parsed.arg == BasicArgumentType.Reserved and parsed.indirect and parsed.incDec == null and parsed.offset == null) {
                    if (Argument.parseReg16(parsed.arg.Reserved)) |reg| {
                        return ArgumentData{
                            .IndirectRegister16Bit = reg,
                        };
                    }
                }
            },
            DefinedArgumentType.IndirectRegister16BitInc => {
                if (parsed.arg == BasicArgumentType.Reserved and parsed.indirect and parsed.incDec == true and parsed.offset == null) {
                    if (Argument.parseReg16(parsed.arg.Reserved)) |reg| {
                        return ArgumentData{
                            .IndirectRegister16BitInc = reg,
                        };
                    }
                }
            },
            DefinedArgumentType.IndirectRegister16BitDec => {
                if (parsed.arg == BasicArgumentType.Reserved and parsed.indirect and parsed.incDec == false and parsed.offset == null) {
                    if (Argument.parseReg16(parsed.arg.Reserved)) |reg| {
                        return ArgumentData{
                            .IndirectRegister16BitDec = reg,
                        };
                    }
                }
            },
        }
        return null;
    }
};

test "ArgumentData.fromDefinition Immediate3Bit" {
    const definition = DefinedArgumentType.Immediate3Bit;
    const argument = Argument{
        .arg = .{
            .Immediate = 2,
        },
        .incDec = null,
        .indirect = false,
        .offset = null,
    };
    const expected = ArgumentData{
        .Immediate3Bit = 2,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition Immediate8Bit" {
    const definition = DefinedArgumentType.Immediate8Bit;
    const argument = Argument{
        .arg = .{
            .Immediate = 42,
        },
        .incDec = null,
        .indirect = false,
        .offset = null,
    };
    const expected = ArgumentData{
        .Immediate8Bit = 42,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition Immediate8BitSigned" {
    const definition = DefinedArgumentType.Immediate8BitSigned;
    const argument = Argument{
        .arg = .{
            .Immediate = -42,
        },
        .incDec = null,
        .indirect = false,
        .offset = null,
    };
    const expected = ArgumentData{
        .Immediate8BitSigned = -42,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition Register16BitWithOffset" {
    const definition = DefinedArgumentType.Register16BitWithOffset;
    const argument = Argument{
        .arg = .{
            .Reserved = "SP",
        },
        .incDec = null,
        .indirect = false,
        .offset = -5,
    };
    const expected = ArgumentData{
        .Register16BitWithOffset = .{
            .offset = -5,
            .reg = Register16.SP,
        },
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition Immediate16Bit" {
    const definition = DefinedArgumentType.Immediate16Bit;
    const argument = Argument{
        .arg = .{
            .Immediate = 5,
        },
        .incDec = null,
        .indirect = false,
        .offset = null,
    };
    const expected = ArgumentData{
        .Immediate16Bit = 5,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition ImmediateBitIndex" {
    const definition = DefinedArgumentType.ImmediateBitIndex;
    const argument = Argument{
        .arg = .{
            .Immediate = 5,
        },
        .incDec = null,
        .indirect = false,
        .offset = null,
    };
    const expected = ArgumentData{
        .ImmediateBitIndex = 5,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition Register8Bit" {
    const definition = DefinedArgumentType.Register8Bit;
    const argument = Argument{
        .arg = .{
            .Reserved = "B",
        },
        .incDec = null,
        .indirect = false,
        .offset = null,
    };
    const expected = ArgumentData{ .Register8Bit = Register8.B };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition Register16Bit" {
    const definition = DefinedArgumentType.Register16Bit;
    const argument = Argument{
        .arg = .{
            .Reserved = "HL",
        },
        .incDec = null,
        .indirect = false,
        .offset = null,
    };
    const expected = ArgumentData{ .Register16Bit = Register16.HL };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition Condition" {
    const definition = DefinedArgumentType.Condition;
    const argument = Argument{
        .arg = .{
            .Reserved = "Z",
        },
        .incDec = null,
        .indirect = false,
        .offset = null,
    };
    const expected = ArgumentData{ .Condition = Condition.Z };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition IndirectImmediate8Bit" {
    const definition = DefinedArgumentType.IndirectImmediate8Bit;
    const argument = Argument{
        .arg = .{
            .Immediate = 42,
        },
        .incDec = null,
        .indirect = true,
        .offset = null,
    };
    const expected = ArgumentData{
        .IndirectImmediate8Bit = 42,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition IndirectImmediate16Bit" {
    const definition = DefinedArgumentType.IndirectImmediate16Bit;
    const argument = Argument{
        .arg = .{
            .Immediate = 42,
        },
        .incDec = null,
        .indirect = true,
        .offset = null,
    };
    const expected = ArgumentData{
        .IndirectImmediate16Bit = 42,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition IndirectRegister8Bit" {
    const definition = DefinedArgumentType.IndirectRegister8Bit;
    const argument = Argument{
        .arg = .{
            .Reserved = "B",
        },
        .incDec = null,
        .indirect = true,
        .offset = null,
    };
    const expected = ArgumentData{ .IndirectRegister8Bit = Register8.B };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition IndirectRegister16Bit" {
    const definition = DefinedArgumentType.IndirectRegister16Bit;
    const argument = Argument{
        .arg = .{
            .Reserved = "HL",
        },
        .incDec = null,
        .indirect = true,
        .offset = null,
    };
    const expected = ArgumentData{
        .IndirectRegister16Bit = Register16.HL,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition IndirectRegister16BitInc" {
    const definition = DefinedArgumentType.IndirectRegister16BitInc;
    const argument = Argument{
        .arg = .{
            .Reserved = "HL",
        },
        .incDec = true,
        .indirect = true,
        .offset = null,
    };
    const expected = ArgumentData{
        .IndirectRegister16BitInc = Register16.HL,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentData.fromDefinition IndirectRegister16BitDec" {
    const definition = DefinedArgumentType.IndirectRegister16BitDec;
    const argument = Argument{
        .arg = .{
            .Reserved = "HL",
        },
        .incDec = false,
        .indirect = true,
        .offset = null,
    };
    const expected = ArgumentData{
        .IndirectRegister16BitDec = Register16.HL,
    };

    const actual = ArgumentData.fromDefinition(argument, definition);

    try std.testing.expectEqualDeep(expected, actual);
}

const ArgumentDefinition = union(DefinedArgumentType) {
    Immediate3Bit,
    Immediate8Bit,
    Immediate8BitSigned,
    Immediate16Bit,
    ImmediateBitIndex,
    Register8Bit: Register8BitArgumentDefinition,
    Register16Bit: Register16BitArgumentDefinition,
    Register16BitWithOffset: Register16,
    Condition,
    IndirectImmediate8Bit,
    IndirectImmediate16Bit,
    IndirectRegister8Bit: Register8,
    IndirectRegister16Bit: Register16,
    IndirectRegister16BitInc: Register16,
    IndirectRegister16BitDec: Register16,

    fn get_mask(self: ArgumentDefinition) ?u8 {
        return switch (self) {
            .Immediate3Bit => 0b11_000_111,
            .Immediate8Bit => null,
            .Immediate8BitSigned => null,
            .Immediate16Bit => null,
            .ImmediateBitIndex => 0b11_000_111,
            .Register8Bit => |x| switch (x) {
                .Offset => |offset| ~(@as(u8, 0b111) << @intCast(offset)),
                .Register => null,
            },
            .Register16Bit => |x| switch (x) {
                .Offset => |variety| switch (variety) {
                    .LikeLD, .LikePUSH => |offset| ~(@as(u8, 0b11) << @intCast(offset)),
                },
                .Register => null,
            },
            .Register16BitWithOffset => null,
            .Condition => 0b111_00_111,
            .IndirectImmediate8Bit => null,
            .IndirectImmediate16Bit => null,
            .IndirectRegister8Bit => null,
            .IndirectRegister16Bit => null,
            .IndirectRegister16BitInc => null,
            .IndirectRegister16BitDec => null,
        };
    }

    fn decode(self: ArgumentDefinition, allocator: std.mem.Allocator, stream: []const u8, opcode: u8) !?struct { ?Argument, []const u8 } {
        switch (self) {
            .Immediate3Bit => {
                return .{
                    .{
                        .arg = .{
                            .Immediate = (opcode & 0b00_111_000) >> 3,
                        },
                        .indirect = false,
                        .incDec = null,
                        .offset = null,
                    },
                    stream,
                };
            },
            .Immediate8Bit => {
                return .{
                    .{
                        .arg = .{
                            .Immediate = stream[0],
                        },
                        .indirect = false,
                        .incDec = null,
                        .offset = null,
                    },
                    stream[1..],
                };
            },
            .Immediate8BitSigned => {
                return .{
                    .{
                        .arg = .{
                            .Immediate = @as(i8, @bitCast(stream[0])),
                        },
                        .indirect = false,
                        .incDec = null,
                        .offset = null,
                    },
                    stream[1..],
                };
            },
            .Immediate16Bit => {
                return .{
                    .{
                        .arg = .{ .Immediate = @as(u16, stream[0]) | (@as(u16, stream[1]) << 8) },
                        .indirect = false,
                        .incDec = null,
                        .offset = null,
                    },
                    stream[2..],
                };
            },
            .ImmediateBitIndex => {
                return .{
                    .{
                        .arg = .{
                            .Immediate = (opcode & 0b00_111_000) >> 3,
                        },
                        .indirect = false,
                        .incDec = null,
                        .offset = null,
                    },
                    stream,
                };
            },
            .Register8Bit => |x| {
                const reg = switch (x) {
                    .Offset => |offset| blk: {
                        const mask = @as(u8, 0b111) << @intCast(offset);
                        const code = (opcode & mask) >> @intCast(offset);
                        break :blk if (ArgumentDefinition.decodeRegister8(code)) |res| res else return null;
                    },
                    .Register => |register| register,
                };
                return .{
                    .{
                        .arg = .{
                            .Reserved = try allocator.dupe(u8, @tagName(reg)),
                        },
                        .indirect = false,
                        .incDec = null,
                        .offset = null,
                    },
                    stream,
                };
            },
            .Register16Bit => |x| {
                const reg = switch (x) {
                    .Offset => |offset_def| blk: {
                        const offset = switch (offset_def) {
                            .LikeLD, .LikePUSH => |o| o,
                        };
                        const mask = @as(u8, 0b11) << @intCast(offset);
                        const code = (opcode & mask) >> @intCast(offset);
                        break :blk if (switch (offset_def) {
                            .LikeLD => ArgumentDefinition.decodeRegister16_1(code),
                            .LikePUSH => ArgumentDefinition.decodeRegister16_2(code),
                        }) |res| res else return null;
                    },
                    .Register => |register| register,
                };
                return .{
                    .{
                        .arg = .{
                            .Reserved = try allocator.dupe(u8, @tagName(reg)),
                        },
                        .indirect = false,
                        .incDec = null,
                        .offset = null,
                    },
                    stream,
                };
            },
            .Register16BitWithOffset => |x| {
                return .{
                    .{
                        .arg = .{
                            .Reserved = try allocator.dupe(u8, @tagName(x)),
                        },
                        .indirect = false,
                        .incDec = null,
                        .offset = @bitCast(stream[0]),
                    },
                    stream[1..],
                };
            },
            .Condition => {
                const code = (opcode & 0b000_11_000) >> 3;
                const cond = if (ArgumentDefinition.decodeCondition(code)) |res| res else return null;
                return .{
                    .{
                        .arg = .{
                            .Reserved = try allocator.dupe(u8, @tagName(cond)),
                        },
                        .indirect = false,
                        .incDec = null,
                        .offset = null,
                    },
                    stream,
                };
            },
            .IndirectImmediate8Bit => {
                return .{
                    .{
                        .arg = .{
                            .Immediate = stream[0],
                        },
                        .indirect = true,
                        .incDec = null,
                        .offset = null,
                    },
                    stream[1..],
                };
            },
            .IndirectImmediate16Bit => {
                return .{
                    .{
                        .arg = .{ .Immediate = @as(u16, stream[0]) | (@as(u16, stream[1]) << 8) },
                        .indirect = true,
                        .incDec = null,
                        .offset = null,
                    },
                    stream[2..],
                };
            },
            .IndirectRegister8Bit => |register| {
                return .{
                    .{
                        .arg = .{
                            .Reserved = try allocator.dupe(u8, @tagName(register)),
                        },
                        .indirect = true,
                        .incDec = null,
                        .offset = null,
                    },
                    stream,
                };
            },
            .IndirectRegister16Bit => |register| {
                return .{
                    .{
                        .arg = .{
                            .Reserved = try allocator.dupe(u8, @tagName(register)),
                        },
                        .indirect = true,
                        .incDec = null,
                        .offset = null,
                    },
                    stream,
                };
            },
            .IndirectRegister16BitInc => |register| {
                return .{
                    .{
                        .arg = .{
                            .Reserved = try allocator.dupe(u8, @tagName(register)),
                        },
                        .indirect = true,
                        .incDec = true,
                        .offset = null,
                    },
                    stream,
                };
            },
            .IndirectRegister16BitDec => |register| {
                return .{
                    .{
                        .arg = .{
                            .Reserved = try allocator.dupe(u8, @tagName(register)),
                        },
                        .indirect = true,
                        .incDec = false,
                        .offset = null,
                    },
                    stream,
                };
            },
        }
    }

    fn encode(self: ArgumentDefinition, opcode: u8, arg: ArgumentData) !struct { u8, ?u8, ?u8 } {
        switch (self) {
            .Immediate3Bit => {
                const vector: u8 = @intCast(arg.Immediate3Bit);
                const new_opcode = opcode | (vector << 3);
                return .{ new_opcode, null, null };
            },
            .Immediate8Bit => {
                return .{ opcode, arg.Immediate8Bit, null };
            },
            .Immediate8BitSigned => {
                const byte: u8 = @bitCast(arg.Immediate8BitSigned);
                return .{ opcode, byte, null };
            },
            .Immediate16Bit => {
                const lsb: u8 = @intCast(arg.Immediate16Bit & 0xFF);
                const msb: u8 = @intCast((arg.Immediate16Bit & 0xFF00) >> 8);
                return .{ opcode, lsb, msb };
            },
            .ImmediateBitIndex => {
                const new_opcode = opcode | (@as(u8, arg.ImmediateBitIndex) << 3);
                return .{ new_opcode, null, null };
            },
            .Register8Bit => |x| {
                switch (x) {
                    .Offset => |offset| {
                        const reg_code = try ArgumentDefinition.encodeRegister8(arg.Register8Bit);
                        const new_opcode = opcode | (reg_code << @intCast(offset));
                        return .{ new_opcode, null, null };
                    },
                    .Register => |register| {
                        if (arg.Register8Bit != register) {
                            return AssemblerError.InvalidInstructionArguments;
                        }
                        return .{ opcode, null, null };
                    },
                }
            },
            .Register16Bit => |x| {
                switch (x) {
                    .Offset => |offset_def| {
                        const reg_code, const offset = switch (offset_def) {
                            .LikeLD => |off| .{ try ArgumentDefinition.encodeRegister16_1(arg.Register16Bit), off },
                            .LikePUSH => |off| .{ try ArgumentDefinition.encodeRegister16_2(arg.Register16Bit), off },
                        };
                        const new_opcode = opcode | (reg_code << @intCast(offset));
                        return .{ new_opcode, null, null };
                    },
                    .Register => |register| {
                        if (arg.Register16Bit != register) {
                            return AssemblerError.InvalidInstructionArguments;
                        }
                        return .{ opcode, null, null };
                    },
                }
            },
            .Register16BitWithOffset => |x| {
                if (x != arg.Register16BitWithOffset.reg) {
                    return AssemblerError.InvalidInstructionArguments;
                }
                const byte: u8 = @bitCast(arg.Register16BitWithOffset.offset);
                return .{ opcode, byte, null };
            },
            .Condition => {
                const cond = ArgumentDefinition.encodeCondition(arg.Condition);
                const new_opcode = opcode | (cond << 3);
                return .{ new_opcode, null, null };
            },
            .IndirectImmediate8Bit => {
                const byte: u8 = arg.IndirectImmediate8Bit;
                return .{ opcode, byte, null };
            },
            .IndirectImmediate16Bit => {
                const lsb: u8 = @intCast(arg.IndirectImmediate16Bit & 0xFF);
                const msb: u8 = @intCast((arg.IndirectImmediate16Bit & 0xFF00) >> 8);
                return .{ opcode, lsb, msb };
            },
            .IndirectRegister8Bit => |register| {
                if (arg.IndirectRegister8Bit != register) {
                    return AssemblerError.InvalidInstructionArguments;
                }
                return .{ opcode, null, null };
            },
            .IndirectRegister16Bit => |register| {
                if (arg.IndirectRegister16Bit != register) {
                    return AssemblerError.InvalidInstructionArguments;
                }
                return .{ opcode, null, null };
            },
            .IndirectRegister16BitInc => |register| {
                if (arg.IndirectRegister16BitInc != register) {
                    return AssemblerError.InvalidInstructionArguments;
                }
                return .{ opcode, null, null };
            },
            .IndirectRegister16BitDec => |register| {
                if (arg.IndirectRegister16BitDec != register) {
                    return AssemblerError.InvalidInstructionArguments;
                }
                return .{ opcode, null, null };
            },
        }
    }

    fn encodeRegister8(r: Register8) !u8 {
        return switch (r) {
            Register8.B => 0b000,
            Register8.C => 0b001,
            Register8.D => 0b010,
            Register8.E => 0b011,
            Register8.H => 0b100,
            Register8.L => 0b101,
            Register8.A => 0b111,
            else => {
                return AssemblerError.InvalidInstructionArguments;
            },
        };
    }

    fn decodeRegister8(r: u8) ?Register8 {
        return switch (r) {
            0b000 => Register8.B,
            0b001 => Register8.C,
            0b010 => Register8.D,
            0b011 => Register8.E,
            0b100 => Register8.H,
            0b101 => Register8.L,
            0b111 => Register8.A,
            else => null,
        };
    }

    fn encodeRegister16_1(r: Register16) !u8 {
        return switch (r) {
            Register16.BC => 0b00,
            Register16.DE => 0b01,
            Register16.HL => 0b10,
            Register16.SP => 0b11,
            else => {
                return AssemblerError.InvalidInstructionArguments;
            },
        };
    }

    fn decodeRegister16_1(r: u8) ?Register16 {
        return switch (r) {
            0b00 => Register16.BC,
            0b01 => Register16.DE,
            0b10 => Register16.HL,
            0b11 => Register16.SP,
            else => null,
        };
    }

    fn encodeRegister16_2(r: Register16) !u8 {
        return switch (r) {
            Register16.BC => 0b00,
            Register16.DE => 0b01,
            Register16.HL => 0b10,
            Register16.AF => 0b11,
            else => {
                return AssemblerError.InvalidInstructionArguments;
            },
        };
    }

    fn decodeRegister16_2(r: u8) ?Register16 {
        return switch (r) {
            0b00 => Register16.BC,
            0b01 => Register16.DE,
            0b10 => Register16.HL,
            0b11 => Register16.AF,
            else => null,
        };
    }

    fn encodeCondition(c: Condition) u8 {
        return switch (c) {
            Condition.NZ => 0b00,
            Condition.Z => 0b01,
            Condition.NC => 0b10,
            Condition.C => 0b11,
        };
    }

    fn decodeCondition(c: u8) ?Condition {
        return switch (c) {
            0b00 => Condition.NZ,
            0b01 => Condition.Z,
            0b10 => Condition.NC,
            0b11 => Condition.C,
            else => null,
        };
    }
};

test "ArgumentDefinition.encode Immediate8Bit" {
    const definition = ArgumentDefinition{
        .Immediate8Bit = {},
    };
    const argument = ArgumentData{
        .Immediate8Bit = 0xFF,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, 0xFF, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Immediate8BitSigned" {
    const definition = ArgumentDefinition{
        .Immediate8BitSigned = {},
    };
    const argument = ArgumentData{
        .Immediate8BitSigned = -1,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, 0xFF, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Register16BitWithOffset" {
    const definition = ArgumentDefinition{
        .Register16BitWithOffset = Register16.SP,
    };
    const argument = ArgumentData{
        .Register16BitWithOffset = .{
            .offset = -128,
            .reg = Register16.SP,
        },
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, 0x80, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Immediate16Bit" {
    const definition = ArgumentDefinition{
        .Immediate16Bit = {},
    };
    const argument = ArgumentData{
        .Immediate16Bit = 0xFFEE,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, 0xEE, 0xFF };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode ImmediateBitIndex" {
    const definition = ArgumentDefinition{
        .ImmediateBitIndex = {},
    };
    const argument = ArgumentData{
        .ImmediateBitIndex = 0b101,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0b00_101_000, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Register8Bit offset" {
    const definition = ArgumentDefinition{
        .Register8Bit = Register8BitArgumentDefinition{
            .Offset = 3,
        },
    };
    const argument = ArgumentData{
        .Register8Bit = Register8.A,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0b00111000, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Register8Bit register success" {
    const definition = ArgumentDefinition{
        .Register8Bit = Register8BitArgumentDefinition{
            .Register = Register8.A,
        },
    };
    const argument = ArgumentData{
        .Register8Bit = Register8.A,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Register8Bit register failure" {
    const definition = ArgumentDefinition{
        .Register8Bit = Register8BitArgumentDefinition{
            .Register = Register8.A,
        },
    };
    const argument = ArgumentData{
        .Register8Bit = Register8.B,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.encode Register16Bit offset 1" {
    const definition = ArgumentDefinition{
        .Register16Bit = Register16BitArgumentDefinition{
            .Offset = Register16BitOffsetVariety{
                .LikePUSH = 5,
            },
        },
    };
    const argument = ArgumentData{
        .Register16Bit = Register16.AF,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0b01100000, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Register16Bit offset 2" {
    const definition = ArgumentDefinition{
        .Register16Bit = Register16BitArgumentDefinition{
            .Offset = Register16BitOffsetVariety{
                .LikeLD = 5,
            },
        },
    };
    const argument = ArgumentData{
        .Register16Bit = Register16.SP,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0b01100000, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Register16Bit register success" {
    const definition = ArgumentDefinition{
        .Register16Bit = Register16BitArgumentDefinition{
            .Register = Register16.SP,
        },
    };
    const argument = ArgumentData{
        .Register16Bit = Register16.SP,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Register16Bit register failure" {
    const definition = ArgumentDefinition{
        .Register16Bit = Register16BitArgumentDefinition{
            .Register = Register16.SP,
        },
    };
    const argument = ArgumentData{
        .Register16Bit = Register16.BC,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.encode Condition" {
    const definition = ArgumentDefinition{
        .Condition = {},
    };
    const argument = ArgumentData{
        .Condition = Condition.C,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0b000_11_000, null, null };

    const actual = definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode IndirectImmediate8Bit" {
    const definition = ArgumentDefinition{
        .IndirectImmediate8Bit = {},
    };
    const argument = ArgumentData{
        .IndirectImmediate8Bit = 0xFF,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, 0xFF, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode IndirectImmediate16Bit" {
    const definition = ArgumentDefinition{
        .IndirectImmediate16Bit = {},
    };
    const argument = ArgumentData{
        .IndirectImmediate16Bit = 0xFFEE,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, 0xEE, 0xFF };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode IndirectRegister8Bit success" {
    const definition = ArgumentDefinition{
        .IndirectRegister8Bit = Register8.A,
    };
    const argument = ArgumentData{
        .IndirectRegister8Bit = Register8.A,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode IndirectRegister8Bit failure" {
    const definition = ArgumentDefinition{
        .IndirectRegister8Bit = Register8.A,
    };
    const argument = ArgumentData{
        .IndirectRegister8Bit = Register8.B,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.encode IndirectRegister16Bit success" {
    const definition = ArgumentDefinition{
        .IndirectRegister16Bit = Register16.BC,
    };
    const argument = ArgumentData{
        .IndirectRegister16Bit = Register16.BC,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode IndirectRegister16Bit failure" {
    const definition = ArgumentDefinition{
        .IndirectRegister16Bit = Register16.BC,
    };
    const argument = ArgumentData{
        .IndirectRegister16Bit = Register16.DE,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.encode IndirectRegister16BitInc success" {
    const definition = ArgumentDefinition{
        .IndirectRegister16BitInc = Register16.BC,
    };
    const argument = ArgumentData{
        .IndirectRegister16BitInc = Register16.BC,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode IndirectRegister16BitInc failure" {
    const definition = ArgumentDefinition{
        .IndirectRegister16BitInc = Register16.BC,
    };
    const argument = ArgumentData{
        .IndirectRegister16BitInc = Register16.DE,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.encode IndirectRegister16BitDec success" {
    const definition = ArgumentDefinition{
        .IndirectRegister16BitDec = Register16.BC,
    };
    const argument = ArgumentData{
        .IndirectRegister16BitDec = Register16.BC,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, null, null };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode IndirectRegister16BitDec failure" {
    const definition = ArgumentDefinition{
        .IndirectRegister16BitDec = Register16.BC,
    };
    const argument = ArgumentData{
        .IndirectRegister16BitDec = Register16.DE,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.decode Immediate3Bit" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0b11_101_001;

    const expected = Argument{
        .arg = .{
            .Immediate = 0b101,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Immediate3Bit = {},
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqual(expected, actual);
}

test "ArgumentDefinition.decode Immediate8Bit" {
    const stream = [_]u8{ 0xAA, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{
            .Immediate = 0xAA,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Immediate8Bit = {},
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[1..], rest);
    try std.testing.expectEqual(expected, actual);
}

test "ArgumentDefinition.decode Immediate8BitSigned" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{
            .Immediate = -1,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Immediate8BitSigned = {},
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[1..], rest);
    try std.testing.expectEqual(expected, actual);
}

test "ArgumentDefinition.decode Immediate16Bit" {
    const stream = [_]u8{ 0xCD, 0xAB, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{
            .Immediate = 0xABCD,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Immediate16Bit = {},
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[2..], rest);
    try std.testing.expectEqual(expected, actual);
}

test "ArgumentDefinition.decode ImmediateBitIndex" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0b11_010_111;

    const expected = Argument{
        .arg = .{
            .Immediate = 0b010,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .ImmediateBitIndex = {},
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqual(expected, actual);
}

test "ArgumentDefinition.decode Register8Bit Offset" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0b1111_000_1;

    const expected = Argument{
        .arg = .{
            .Reserved = "B",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Register8Bit = .{
            .Offset = 1,
        },
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode Register8Bit Register" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{
            .Reserved = "B",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Register8Bit = .{
            .Register = Register8.B,
        },
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode Register16Bit Offset LikeLD" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0b1111_11_11;

    const expected = Argument{
        .arg = .{
            .Reserved = "SP",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Register16Bit = .{
            .Offset = .{
                .LikeLD = 2,
            },
        },
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode Register16Bit Offset LikePUSH" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0b1111_11_11;

    const expected = Argument{
        .arg = .{
            .Reserved = "AF",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Register16Bit = .{
            .Offset = .{
                .LikePUSH = 2,
            },
        },
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode Register16Bit Register" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{
            .Reserved = "BC",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Register16Bit = .{
            .Register = Register16.BC,
        },
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode Register16BitWithOffset" {
    const stream = [_]u8{ 3, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{
            .Reserved = "BC",
        },
        .indirect = false,
        .incDec = null,
        .offset = 3,
    };

    const definition = ArgumentDefinition{
        .Register16BitWithOffset = Register16.BC,
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[1..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode Condition" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0b000_00_000;

    const expected = Argument{
        .arg = .{
            .Reserved = "NZ",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .Condition = {},
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode IndirectImmediate8Bit" {
    const stream = [_]u8{ 0xAB, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{
            .Immediate = 0xAB,
        },
        .indirect = true,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .IndirectImmediate8Bit = {},
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[1..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode IndirectRegister16Bit" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{ .Reserved = "DE" },
        .indirect = true,
        .incDec = null,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .IndirectRegister16Bit = Register16.DE,
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode IndirectRegister16BitInc" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{ .Reserved = "DE" },
        .indirect = true,
        .incDec = true,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .IndirectRegister16BitInc = Register16.DE,
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.decode IndirectRegister16BitDec" {
    const stream = [_]u8{ 0xFF, 0xFF, 0xFF };
    const opcode = 0x00;

    const expected = Argument{
        .arg = .{ .Reserved = "DE" },
        .indirect = true,
        .incDec = false,
        .offset = null,
    };

    const definition = ArgumentDefinition{
        .IndirectRegister16BitDec = Register16.DE,
    };

    const actual, const rest = (try definition.decode(std.testing.allocator, &stream, opcode)).?;
    defer if (actual) |a| {
        a.free(std.testing.allocator);
    };

    try std.testing.expectEqualSlices(u8, stream[0..], rest);
    try std.testing.expectEqualDeep(expected, actual);
}

const OpcodeDefinition = struct {
    instr: Instruction,
    arg1: ?ArgumentDefinition,
    arg2: ?ArgumentDefinition,
    base_opcode: u8,
    prefix: ?u8 = null,
};

const defined_opcodes =
    [_]OpcodeDefinition{
        OpcodeDefinition{
            .instr = Instruction.NOP,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00000000,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 3,
                },
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .base_opcode = 0b01_000_000,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 3,
                },
            },
            .arg2 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .base_opcode = 0b00_000_110,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 3,
                },
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .base_opcode = 0b01_000_110,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .base_opcode = 0b01_110_000,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .base_opcode = 0b00110110,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.BC,
            },
            .base_opcode = 0b00001010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.DE,
            },
            .base_opcode = 0b00011010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.BC,
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .base_opcode = 0b00000010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.DE,
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .base_opcode = 0b00010010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .arg2 = ArgumentDefinition{
                .IndirectImmediate16Bit = {},
            },
            .base_opcode = 0b11111010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .IndirectImmediate16Bit = {},
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .base_opcode = 0b11101010,
        },
        OpcodeDefinition{
            .instr = Instruction.LDH,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister8Bit = Register8.C,
            },
            .base_opcode = 0b11110010,
        },
        OpcodeDefinition{
            .instr = Instruction.LDH,
            .arg1 = ArgumentDefinition{
                .IndirectRegister8Bit = Register8.C,
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .base_opcode = 0b11100010,
        },
        OpcodeDefinition{
            .instr = Instruction.LDH,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .arg2 = ArgumentDefinition{
                .IndirectImmediate8Bit = {},
            },
            .base_opcode = 0b11110000,
        },
        OpcodeDefinition{
            .instr = Instruction.LDH,
            .arg1 = ArgumentDefinition{
                .IndirectImmediate8Bit = {},
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .base_opcode = 0b11100000,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister16BitDec = Register16.HL,
            },
            .base_opcode = 0b00111010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16BitDec = Register16.HL,
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .base_opcode = 0b00110010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister16BitInc = Register16.HL,
            },
            .base_opcode = 0b00101010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16BitInc = Register16.HL,
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Register = Register8.A,
                },
            },
            .base_opcode = 0b00100010,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Offset = Register16BitOffsetVariety{
                        .LikeLD = 4,
                    },
                },
            },
            .arg2 = ArgumentDefinition{
                .Immediate16Bit = {},
            },
            .base_opcode = 0b00_00_0001,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .IndirectImmediate16Bit = {},
            },
            .arg2 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Register = Register16.SP,
                },
            },
            .base_opcode = 0b00001000,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Register = Register16.SP,
                },
            },
            .arg2 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Register = Register16.HL,
                },
            },
            .base_opcode = 0b11111001,
        },
        OpcodeDefinition{
            .instr = Instruction.PUSH,
            .arg1 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Offset = Register16BitOffsetVariety{
                        .LikePUSH = 4,
                    },
                },
            },
            .arg2 = null,
            .base_opcode = 0b11_00_0101,
        },
        OpcodeDefinition{
            .instr = Instruction.POP,
            .arg1 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Offset = Register16BitOffsetVariety{
                        .LikePUSH = 4,
                    },
                },
            },
            .arg2 = null,
            .base_opcode = 0b11_00_0001,
        },
        OpcodeDefinition{
            .instr = Instruction.LD,
            .arg1 = ArgumentDefinition{
                .Register16Bit = .{
                    .Register = Register16.HL,
                },
            },
            .arg2 = ArgumentDefinition{
                .Register16BitWithOffset = Register16.SP,
            },
            .base_opcode = 0b11111000,
        },
        OpcodeDefinition{
            .instr = Instruction.ADD,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b10000_000,
        },
        OpcodeDefinition{
            .instr = Instruction.ADD,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b10000110,
        },
        OpcodeDefinition{
            .instr = Instruction.ADD,
            .arg1 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11000110,
        },
        OpcodeDefinition{
            .instr = Instruction.ADC,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b10001_000,
        },
        OpcodeDefinition{
            .instr = Instruction.ADC,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b10001110,
        },
        OpcodeDefinition{
            .instr = Instruction.ADC,
            .arg1 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11001110,
        },
        OpcodeDefinition{
            .instr = Instruction.SUB,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b10010_000,
        },
        OpcodeDefinition{
            .instr = Instruction.SUB,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b10010110,
        },
        OpcodeDefinition{
            .instr = Instruction.SUB,
            .arg1 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11010110,
        },
        OpcodeDefinition{
            .instr = Instruction.SBC,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b10011_000,
        },
        OpcodeDefinition{
            .instr = Instruction.SBC,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b10011110,
        },
        OpcodeDefinition{
            .instr = Instruction.SBC,
            .arg1 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11011110,
        },
        OpcodeDefinition{
            .instr = Instruction.CP,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b10111_000,
        },
        OpcodeDefinition{
            .instr = Instruction.CP,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b10011110,
        },
        OpcodeDefinition{
            .instr = Instruction.CP,
            .arg1 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11111110,
        },
        OpcodeDefinition{
            .instr = Instruction.INC,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 3,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00_000_100,
        },
        OpcodeDefinition{
            .instr = Instruction.INC,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00_110100,
        },
        OpcodeDefinition{
            .instr = Instruction.DEC,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 3,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00_000_101,
        },
        OpcodeDefinition{
            .instr = Instruction.DEC,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00_110101,
        },
        OpcodeDefinition{
            .instr = Instruction.AND,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b10100_000,
        },
        OpcodeDefinition{
            .instr = Instruction.AND,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b10100110,
        },
        OpcodeDefinition{
            .instr = Instruction.AND,
            .arg1 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11100110,
        },
        OpcodeDefinition{
            .instr = Instruction.OR,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b10110_000,
        },
        OpcodeDefinition{
            .instr = Instruction.OR,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b10110110,
        },
        OpcodeDefinition{
            .instr = Instruction.OR,
            .arg1 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11110110,
        },
        OpcodeDefinition{
            .instr = Instruction.XOR,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b10101_000,
        },
        OpcodeDefinition{
            .instr = Instruction.XOR,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b10101110,
        },
        OpcodeDefinition{
            .instr = Instruction.XOR,
            .arg1 = ArgumentDefinition{
                .Immediate8Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11101110,
        },
        OpcodeDefinition{
            .instr = Instruction.CCF,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00111111,
        },
        OpcodeDefinition{
            .instr = Instruction.SCF,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00110111,
        },
        OpcodeDefinition{
            .instr = Instruction.DAA,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00100111,
        },
        OpcodeDefinition{
            .instr = Instruction.CPL,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00101111,
        },
        OpcodeDefinition{
            .instr = Instruction.INC,
            .arg1 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Offset = Register16BitOffsetVariety{
                        .LikeLD = 4,
                    },
                },
            },
            .arg2 = null,
            .base_opcode = 0b00_00_0011,
        },
        OpcodeDefinition{
            .instr = Instruction.DEC,
            .arg1 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Offset = Register16BitOffsetVariety{
                        .LikeLD = 4,
                    },
                },
            },
            .arg2 = null,
            .base_opcode = 0b00_00_1011,
        },
        OpcodeDefinition{
            .instr = Instruction.ADD,
            .arg1 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Register = Register16.HL,
                },
            },
            .arg2 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Offset = Register16BitOffsetVariety{
                        .LikeLD = 4,
                    },
                },
            },
            .base_opcode = 0b00_00_1001,
        },
        OpcodeDefinition{
            .instr = Instruction.ADD,
            .arg1 = ArgumentDefinition{
                .Register16Bit = .{
                    .Register = Register16.SP,
                },
            },
            .arg2 = .{
                .Immediate8BitSigned = {},
            },
            .base_opcode = 0b11101000,
        },
        OpcodeDefinition{
            .instr = Instruction.RLCA,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00000111,
        },
        OpcodeDefinition{
            .instr = Instruction.RRCA,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00001111,
        },
        OpcodeDefinition{
            .instr = Instruction.RLA,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00010111,
        },
        OpcodeDefinition{
            .instr = Instruction.RRA,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b00011111,
        },
        OpcodeDefinition{
            .instr = Instruction.RLC,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00000_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RLC,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00000_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RRC,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00001_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RRC,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00001_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RL,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00010_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RL,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00010_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RR,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00011_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RR,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00011_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SLA,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00100_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SLA,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00100_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SRA,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00101_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SRA,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00101_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SWAP,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00110_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SWAP,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00110_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SRL,
            .arg1 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .arg2 = null,
            .base_opcode = 0b00111_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SRL,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b00111_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.BIT,
            .arg1 = ArgumentDefinition{
                .ImmediateBitIndex = {},
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .base_opcode = 0b01_000_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.BIT,
            .arg1 = ArgumentDefinition{
                .ImmediateBitIndex = {},
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .base_opcode = 0b01_000_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RES,
            .arg1 = ArgumentDefinition{
                .ImmediateBitIndex = {},
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .base_opcode = 0b10_000_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.RES,
            .arg1 = ArgumentDefinition{
                .ImmediateBitIndex = {},
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .base_opcode = 0b10_000_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SET,
            .arg1 = ArgumentDefinition{
                .ImmediateBitIndex = {},
            },
            .arg2 = ArgumentDefinition{
                .Register8Bit = Register8BitArgumentDefinition{
                    .Offset = 0,
                },
            },
            .base_opcode = 0b11_000_000,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.SET,
            .arg1 = ArgumentDefinition{
                .ImmediateBitIndex = {},
            },
            .arg2 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .base_opcode = 0b11_000_110,
            .prefix = 0xCB,
        },
        OpcodeDefinition{
            .instr = Instruction.JP,
            .arg1 = ArgumentDefinition{
                .Immediate16Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11000011,
        },
        OpcodeDefinition{
            .instr = Instruction.JP,
            .arg1 = ArgumentDefinition{
                .Register16Bit = Register16BitArgumentDefinition{
                    .Register = Register16.HL,
                },
            },
            .arg2 = null,
            .base_opcode = 0b11101001,
        },
        OpcodeDefinition{
            .instr = Instruction.JP,
            .arg1 = ArgumentDefinition{
                .IndirectRegister16Bit = Register16.HL,
            },
            .arg2 = null,
            .base_opcode = 0b11101001,
        },
        OpcodeDefinition{
            .instr = Instruction.JP,
            .arg1 = ArgumentDefinition{
                .Condition = {},
            },
            .arg2 = ArgumentDefinition{
                .Immediate16Bit = {},
            },
            .base_opcode = 0b110_00_010,
        },
        OpcodeDefinition{
            .instr = Instruction.JR,
            .arg1 = ArgumentDefinition{
                .Immediate8BitSigned = {},
            },
            .arg2 = null,
            .base_opcode = 0b00011000,
        },
        OpcodeDefinition{
            .instr = Instruction.JR,
            .arg1 = ArgumentDefinition{
                .Condition = {},
            },
            .arg2 = ArgumentDefinition{
                .Immediate8BitSigned = {},
            },
            .base_opcode = 0b001_00_000,
        },
        OpcodeDefinition{
            .instr = Instruction.CALL,
            .arg1 = ArgumentDefinition{
                .Immediate16Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11001101,
        },
        OpcodeDefinition{
            .instr = Instruction.CALL,
            .arg1 = ArgumentDefinition{
                .Condition = {},
            },
            .arg2 = ArgumentDefinition{
                .Immediate16Bit = {},
            },
            .base_opcode = 0b110_00_100,
        },
        OpcodeDefinition{
            .instr = Instruction.RET,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b11001001,
        },
        OpcodeDefinition{
            .instr = Instruction.RET,
            .arg1 = ArgumentDefinition{
                .Condition = {},
            },
            .arg2 = null,
            .base_opcode = 0b110_00_000,
        },
        OpcodeDefinition{
            .instr = Instruction.RETI,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0b11011001,
        },
        OpcodeDefinition{
            .instr = Instruction.RST,
            .arg1 = ArgumentDefinition{
                .Immediate3Bit = {},
            },
            .arg2 = null,
            .base_opcode = 0b11_000_111,
        },
        OpcodeDefinition{
            .instr = Instruction.HALT,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0x76,
        },
        OpcodeDefinition{
            .instr = Instruction.STOP,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0x10,
        },
        OpcodeDefinition{
            .instr = Instruction.DI,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0xF3,
        },
        OpcodeDefinition{
            .instr = Instruction.EI,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0xFB,
        },
        OpcodeDefinition{
            .instr = Instruction.NOP,
            .arg1 = null,
            .arg2 = null,
            .base_opcode = 0x00,
        },
    };

const Opcode = struct {
    line: usize,
    instr: Instruction,
    arg1: ?Argument,
    arg2: ?Argument,

    fn format(self: *const Opcode, writer: anytype) !void {
        try writer.writeAll(@tagName(self.instr));
        if (self.arg1) |arg| {
            try writer.writeByte(' ');
            try arg.format(writer);
        }
        if (self.arg2) |arg| {
            try writer.writeAll(", ");
            try arg.format(writer);
        }
    }

    fn fromToken(token: Token, allocator: std.mem.Allocator) !Opcode {
        const token_capital = try toUpper(token.source, allocator);
        defer allocator.free(token_capital);
        const instr = std.meta.stringToEnum(Instruction, token_capital);
        if (instr == null) {
            logger.err("Line {d}: Invalid instruction '{s}'", .{ token.line, token.source });
            return AssemblerError.InvalidInstruction;
        }
        return Opcode{
            .line = token.line,
            .instr = instr.?,
            .arg1 = null,
            .arg2 = null,
        };
    }

    fn addArgument(self: *Opcode, arg: Argument) !void {
        if (self.arg1 == null) {
            self.arg1 = arg;
            return;
        }
        if (self.arg2 == null) {
            self.arg2 = arg;
            return;
        }
        logger.err("Line {d}: Too many arguments for instruction '{s}'", .{ self.line, @tagName(self.instr) });
        return AssemblerError.InvalidArgumentCount;
    }

    fn free(self: *const Opcode, allocator: std.mem.Allocator) void {
        if (self.arg1 != null) {
            self.arg1.?.free(allocator);
        }
        if (self.arg2 != null) {
            self.arg2.?.free(allocator);
        }
    }

    fn encode(self: *const Opcode, stream: *std.ArrayList(u8)) ![]u8 {
        var latest_error: ?AssemblerError = null;
        for (defined_opcodes) |opcode_definition| {
            if (opcode_definition.instr != self.instr) {
                continue;
            }

            const opcode_arg1_null = self.arg1 == null;
            const opcode_arg2_null = self.arg2 == null;
            const definition_arg1_null = opcode_definition.arg1 == null;
            const definition_arg2_null = opcode_definition.arg2 == null;

            if (opcode_arg1_null != definition_arg1_null) {
                continue;
            }

            if (opcode_arg2_null != definition_arg2_null) {
                continue;
            }

            var arg1: ArgumentData = undefined;
            if (!opcode_arg1_null) {
                const arg = ArgumentData.fromDefinition(self.arg1.?, opcode_definition.arg1.?);
                if (arg == null) {
                    continue;
                }
                arg1 = arg.?;
            }

            var arg2: ArgumentData = undefined;
            if (!opcode_arg2_null) {
                const arg = ArgumentData.fromDefinition(self.arg2.?, opcode_definition.arg2.?);
                if (arg == null) {
                    continue;
                }
                arg2 = arg.?;
            }

            var opcode = opcode_definition.base_opcode;

            opcode, const imm1, const imm2 = if (opcode_definition.arg1) |arg|
                arg.encode(opcode, arg1) catch |e| {
                    latest_error = e;
                    continue;
                }
            else
                .{ opcode, null, null };

            opcode, const imm3, const imm4 = if (opcode_definition.arg2) |arg|
                arg.encode(opcode, arg2) catch |e| {
                    latest_error = e;
                    continue;
                }
            else
                .{ opcode, null, null };

            var size: usize = 0;
            if (opcode_definition.prefix != null) {
                try stream.append(opcode_definition.prefix.?);
                size += 1;
            }
            try stream.append(opcode);
            size += 1;
            if (imm1 != null) {
                try stream.append(imm1.?);
                size += 1;
            }
            if (imm2 != null) {
                try stream.append(imm2.?);
                size += 1;
            }
            if (imm3 != null) {
                try stream.append(imm3.?);
                size += 1;
            }
            if (imm4 != null) {
                try stream.append(imm4.?);
                size += 1;
            }

            return stream.items[stream.items.len - size ..];
        }

        logger.err("Line {d}: Invalid instruction/argument combination.", .{self.line});
        return latest_error orelse AssemblerError.InvalidInstructionArguments;
    }

    fn decode(allocator: std.mem.Allocator, stream: []const u8) !struct { Opcode, []const u8 } {
        const opcode, const with_prefix = if (stream[0] == 0xCB)
            .{ stream[1], true }
        else
            .{ stream[0], false };

        for (defined_opcodes) |opcode_definition| {
            var mask: u8 = 0xFF;
            if (opcode_definition.arg1) |arg| {
                if (arg.get_mask()) |m| {
                    mask = m;
                }
            }
            if (opcode_definition.arg2) |arg| {
                if (arg.get_mask()) |m| {
                    mask = m;
                }
            }

            if ((opcode_definition.prefix != null) != with_prefix) {
                continue;
            }
            if (with_prefix and opcode_definition.prefix.? != stream[0]) {
                continue;
            }

            const expected_opcode = opcode_definition.base_opcode & mask;
            if (opcode & mask != expected_opcode) {
                continue;
            }

            var arg1: ?Argument = null;
            var currentStream = stream[(if (with_prefix) 2 else 1)..];
            if (opcode_definition.arg1) |arg| {
                arg1, currentStream = if (try arg.decode(allocator, currentStream, stream[0])) |res|
                    res
                else
                    continue;
            }
            var arg2: ?Argument = null;
            if (opcode_definition.arg2) |arg| {
                arg2, currentStream = if (try arg.decode(allocator, currentStream, stream[0])) |res|
                    res
                else {
                    if (arg1) |a| {
                        a.free(allocator);
                    }
                    continue;
                };
            }

            return .{
                .{
                    .line = 0,
                    .instr = opcode_definition.instr,
                    .arg1 = arg1,
                    .arg2 = arg2,
                },
                currentStream,
            };
        }

        if (with_prefix) {
            logger.err("invalid instruction for decoding: 0xCB 0x{X}", .{opcode});
        } else {
            logger.err("invalid instruction for decoding: 0x{X}", .{opcode});
        }
        return AssemblerError.InvalidInstruction;
    }
};

test "Opcode.fromToken 1" {
    const input = Token{
        .line = 0,
        .source = "DAA",
        .which = TokenKind.Instruction,
    };
    const expected = Opcode{
        .line = 0,
        .instr = Instruction.DAA,
        .arg1 = null,
        .arg2 = null,
    };

    const actual = try Opcode.fromToken(input, std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Opcode.fromToken 2" {
    const input = Token{
        .line = 0,
        .source = "FOO",
        .which = TokenKind.Instruction,
    };
    const expected = AssemblerError.InvalidInstruction;

    const actual = Opcode.fromToken(input, std.testing.allocator);

    try std.testing.expectError(expected, actual);
}

test "addArgument 1" {
    var opcode = Opcode{
        .line = 0,
        .instr = Instruction.ADD,
        .arg1 = null,
        .arg2 = null,
    };
    const arg = Argument{
        .arg = .{
            .Reserved = "B",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    try opcode.addArgument(arg);

    try std.testing.expectEqual(arg, opcode.arg1);
    try std.testing.expectEqual(null, opcode.arg2);
}

test "addArgument 2" {
    const arg1 = Argument{
        .arg = .{
            .Reserved = "B",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };
    var opcode = Opcode{
        .line = 0,
        .instr = Instruction.ADD,
        .arg1 = arg1,
        .arg2 = null,
    };
    const arg = Argument{
        .arg = .{
            .Immediate = 0,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    try opcode.addArgument(arg);

    try std.testing.expectEqual(arg1, opcode.arg1);
    try std.testing.expectEqual(arg, opcode.arg2);
}

test "addArgument 3" {
    const arg1 = Argument{
        .arg = .{
            .Reserved = "B",
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };
    const arg2 = Argument{
        .arg = .{
            .Immediate = 0,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };
    var opcode = Opcode{
        .line = 0,
        .instr = Instruction.LD,
        .arg1 = arg1,
        .arg2 = arg2,
    };
    const arg = Argument{
        .arg = .{
            .Immediate = 0xFFFF,
        },
        .indirect = false,
        .incDec = null,
        .offset = null,
    };

    try std.testing.expectError(AssemblerError.InvalidArgumentCount, opcode.addArgument(arg));
}

test "encode immediate 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b00000001, 0xBB, 0xAA };
    const expected_slice = [_]u8{ 0b00000001, 0xBB, 0xAA };

    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "BC",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = Argument{
            .arg = .{
                .Immediate = 0xAABB,
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode immediate 8" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b00000110, 0xAA };
    const expected_slice = [_]u8{ 0b00000110, 0xAA };

    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "B",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = Argument{
            .arg = .{
                .Immediate = 0xAA,
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode immediate 8 as immediate 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b00000001, 0xAA, 0x00 };
    const expected_slice = [_]u8{ 0b00000001, 0xAA, 0x00 };

    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "BC",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = Argument{
            .arg = .{
                .Immediate = 0xAA,
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode label as immediate 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b00000001, 0xAA, 0xAA };
    const expected_slice = [_]u8{ 0b00000001, 0xAA, 0xAA };

    const label = try toUpper("foo", std.testing.allocator);
    defer std.testing.allocator.free(label);
    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "BC",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = Argument{
            .arg = .{
                .Label = label,
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode indirect 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b11111010, 0xBB, 0xAA };
    const expected_slice = [_]u8{ 0b11111010, 0xBB, 0xAA };

    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "A",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = Argument{
            .arg = .{
                .Immediate = 0xAABB,
            },
            .indirect = true,
            .incDec = null,
            .offset = null,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode indirect 8" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b11110000, 0xAA };
    const expected_slice = [_]u8{ 0b11110000, 0xAA };

    const opcode = Opcode{
        .instr = Instruction.LDH,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "A",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = Argument{
            .arg = .{
                .Immediate = 0xAA,
            },
            .indirect = true,
            .incDec = null,
            .offset = null,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode indirect 8 as indirect 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b11111010, 0xAA, 0x00 };
    const expected_slice = [_]u8{ 0b11111010, 0xAA, 0x00 };

    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "A",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = Argument{
            .arg = .{
                .Immediate = 0xAA,
            },
            .indirect = true,
            .incDec = null,
            .offset = null,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode label as indirect 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b11111010, 0xAA, 0xAA };
    const expected_slice = [_]u8{ 0b11111010, 0xAA, 0xAA };

    const label = try toUpper("foo", std.testing.allocator);
    defer std.testing.allocator.free(label);
    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "A",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = Argument{
            .arg = .{
                .Label = label,
            },
            .indirect = true,
            .incDec = null,
            .offset = null,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode reg16 offset" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b11111000, 0xFF };
    const expected_slice = [_]u8{ 0b11111000, 0xFF };

    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "HL",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = .{
            .arg = .{
                .Reserved = "SP",
            },
            .indirect = false,
            .incDec = null,
            .offset = -1,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode reg16 offset 2" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b11111000, 0x01 };
    const expected_slice = [_]u8{ 0b11111000, 0x01 };

    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = Argument{
            .arg = .{
                .Reserved = "HL",
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = .{
            .arg = .{
                .Reserved = "SP",
            },
            .indirect = false,
            .incDec = null,
            .offset = 1,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode failure" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    defer stream.deinit();

    const opcode = Opcode{
        .instr = Instruction.LD,
        .arg1 = null,
        .arg2 = null,
        .line = 0,
    };

    const actual = opcode.encode(&stream);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "formatOpcode 1" {
    const opcode = Opcode{
        .line = 0,
        .instr = .LD,
        .arg1 = .{
            .arg = .{
                .Immediate = 0x123,
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = .{
            .arg = .{
                .Immediate = 0x123,
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try opcode.format(writer);
    try std.testing.expectEqualStrings("LD 0x123, 0x123", source.buffer.getWritten());
}

test "formatOpcode 2" {
    const opcode = Opcode{
        .line = 0,
        .instr = .LD,
        .arg1 = .{
            .arg = .{
                .Immediate = 0x123,
            },
            .indirect = false,
            .incDec = null,
            .offset = null,
        },
        .arg2 = null,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try opcode.format(writer);
    try std.testing.expectEqualStrings("LD 0x123", source.buffer.getWritten());
}

test "formatOpcode 3" {
    const opcode = Opcode{
        .line = 0,
        .instr = .LD,
        .arg1 = null,
        .arg2 = null,
    };

    var buffer: [64]u8 = undefined;
    var source: std.io.StreamSource = .{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    try opcode.format(writer);
    try std.testing.expectEqualStrings("LD", source.buffer.getWritten());
}

pub fn parser(input: []const Token, allocator: std.mem.Allocator) !struct { std.StringHashMap(usize), []Opcode } {
    var opcodes = std.ArrayList(Opcode).init(allocator);
    defer opcodes.deinit();

    var labels = std.StringHashMap(usize).init(allocator);
    errdefer labels.deinit();

    errdefer for (opcodes.items) |*item| {
        item.free(allocator);
    };

    errdefer {
        var it = labels.iterator();
        while (it.next()) |item| {
            allocator.free(item.key_ptr.*);
        }
    }

    var current: ?Opcode = null;
    var idx: usize = 0;
    for (input) |token| {
        switch (token.which) {
            TokenKind.Label => {
                const upper = try toUpper(token.source, allocator);
                errdefer allocator.free(upper);

                try validateLabel(token.source, token.line);
                if (labels.contains(upper)) {
                    logger.err("Line {d}: Redefinition of label '{s}'", .{ token.line, token.source });
                    return AssemblerError.RedefinedLabel;
                }
                try labels.put(upper, idx);
            },
            TokenKind.Instruction => {
                var next = try Opcode.fromToken(token, allocator);
                errdefer next.free(allocator);

                if (current != null) {
                    try opcodes.append(current.?);
                }
                idx += 1;
                current = next;
            },
            TokenKind.Argument => {
                var arg = try Argument.fromToken(token, allocator);
                errdefer arg.free(allocator);
                try current.?.addArgument(arg);
            },
        }
    }

    if (current != null) {
        try opcodes.append(current.?);
    }

    return .{
        labels,
        try opcodes.toOwnedSlice(),
    };
}

test "parser succeed" {
    const input =
        \\ lbl1:
        \\   ld B 2
        \\ lbl2: ld C 3
        \\ lbl3:
    ;

    const tokens = try lexer(input, std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    var expected_labels = std.StringHashMap(usize).init(std.testing.allocator);
    defer expected_labels.deinit();

    try expected_labels.put("LBL1", 0);
    try expected_labels.put("LBL2", 1);
    try expected_labels.put("LBL3", 2);

    var expected_opcodes = [_]Opcode{
        Opcode{
            .line = 2,
            .instr = Instruction.LD,
            .arg1 = Argument{
                .arg = .{
                    .Reserved = "B",
                },
                .indirect = false,
                .incDec = null,
                .offset = null,
            },
            .arg2 = Argument{
                .arg = .{
                    .Immediate = 2,
                },
                .indirect = false,
                .incDec = null,
                .offset = null,
            },
        },
        Opcode{
            .line = 3,
            .instr = Instruction.LD,
            .arg1 = Argument{
                .arg = .{
                    .Reserved = "C",
                },
                .indirect = false,
                .incDec = null,
                .offset = null,
            },
            .arg2 = Argument{
                .arg = .{
                    .Immediate = 3,
                },
                .indirect = false,
                .incDec = null,
                .offset = null,
            },
        },
    };

    var labels, const opcodes = try parser(tokens, std.testing.allocator);
    defer std.testing.allocator.free(opcodes);
    defer std.testing.allocator.free(opcodes[0].arg1.?.arg.Reserved);
    defer std.testing.allocator.free(opcodes[1].arg1.?.arg.Reserved);
    defer labels.deinit();
    defer {
        var labels_it = labels.iterator();
        while (labels_it.next()) |item| {
            std.testing.allocator.free(item.key_ptr.*);
        }
    }

    // Apparently expectEqualSlices is comparing strings by pointer instead of as slices, so let's make them be the same pointer actually
    try std.testing.expectEqualSlices(u8, expected_opcodes[0].arg1.?.arg.Reserved, opcodes[0].arg1.?.arg.Reserved);
    try std.testing.expectEqualSlices(u8, expected_opcodes[1].arg1.?.arg.Reserved, opcodes[1].arg1.?.arg.Reserved);
    expected_opcodes[0].arg1.?.arg = opcodes[0].arg1.?.arg;
    expected_opcodes[1].arg1.?.arg = opcodes[1].arg1.?.arg;
    try std.testing.expectEqualSlices(Opcode, &expected_opcodes, opcodes);

    try std.testing.expectEqual(expected_labels.count(), labels.count());
    var expected_it = expected_labels.iterator();
    while (expected_it.next()) |expected_label_pair| {
        const expected_value = expected_label_pair.value_ptr.*;
        const actual_value = labels.get(expected_label_pair.key_ptr.*).?;
        try std.testing.expectEqual(expected_value, actual_value);
    }
}

pub fn assembler(input: []const Opcode, labelMap: *std.StringHashMap(usize), allocator: std.mem.Allocator) ![]u8 {
    var accumulatedOpcodeSize = try allocator.alloc(usize, input.len + 1);
    defer allocator.free(accumulatedOpcodeSize);

    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    var labelReferences = std.StringHashMap(std.ArrayList(usize)).init(allocator);
    defer labelReferences.deinit();
    defer {
        var it = labelReferences.iterator();
        while (it.next()) |pair| {
            pair.value_ptr.deinit();
        }
    }

    // Encode opcodes and remember label references
    accumulatedOpcodeSize[0] = 0;
    for (input, 0..) |opcode, idx| {
        const bytes: []u8 = try opcode.encode(&result);
        const accumulated: usize = accumulatedOpcodeSize[idx];
        accumulatedOpcodeSize[idx + 1] = accumulated + bytes.len;

        // Expect labels only on one of the arguments (there is no instruction that takes in multiple immediates)
        var lbl: ?[]const u8 = null;
        if (opcode.arg1 != null and opcode.arg1.?.arg == BasicArgumentType.Label) {
            lbl = opcode.arg1.?.arg.Label;
        }
        if (opcode.arg2 != null and opcode.arg2.?.arg == BasicArgumentType.Label) {
            lbl = opcode.arg2.?.arg.Label;
        }
        if (lbl != null) {
            const getOrPut = try labelReferences.getOrPut(lbl.?);
            if (!getOrPut.found_existing) {
                var newlist = std.ArrayList(usize).init(allocator);
                errdefer newlist.deinit();
                getOrPut.value_ptr.* = newlist;
            }
            try getOrPut.value_ptr.append(accumulated);
        }
    }

    // Update label pointers now that we know the size of each instruction
    {
        var it = labelMap.iterator();
        while (it.next()) |pair| {
            pair.value_ptr.* = accumulatedOpcodeSize[pair.value_ptr.*];
        }
    }

    // Insert label locations into byte stream
    {
        var it = labelReferences.iterator();
        while (it.next()) |pair| {
            const label = pair.key_ptr.*;
            const location = labelMap.get(label);
            if (location == null) {
                logger.err("Undefined label {s}", .{label});
                return AssemblerError.UndefinedLabel;
            }
            const lsb: u8 = @intCast(location.? & 0xFF);
            const msb: u8 = @intCast((location.? & 0xFF00) >> 8);
            for (pair.value_ptr.items) |idx| {
                result.items[idx + 1] = lsb;
                result.items[idx + 2] = msb;
            }
        }
    }

    return result.toOwnedSlice();
}

fn freeLabelMap(labelMap: *std.StringHashMap(usize), allocator: std.mem.Allocator) void {
    var it = labelMap.iterator();
    while (it.next()) |item| {
        allocator.free(item.key_ptr.*);
    }
}

fn freeOpcodes(opcodes: []Opcode, allocator: std.mem.Allocator) void {
    for (opcodes) |*op| {
        if (op.arg1 != null) {
            op.arg1.?.free(allocator);
        }
        if (op.arg2 != null) {
            op.arg2.?.free(allocator);
        }
    }
}

test "assembler" {
    const input =
        \\ lbl1:
        \\       ld BC lbl3
        \\ lbl2: ld BC lbl2
        \\       ld BC lbl1
        \\ lbl3:
    ;

    const tokens = try lexer(input, std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    var labelMap, const opcodes = try parser(tokens, std.testing.allocator);
    defer labelMap.deinit();
    defer std.testing.allocator.free(opcodes);
    defer freeLabelMap(&labelMap, std.testing.allocator);
    defer freeOpcodes(opcodes, std.testing.allocator);

    const expected = [_]u8{
        0b00000001, 0x09, 0x00,
        0b00000001, 0x03, 0x00,
        0b00000001, 0x00, 0x00,
    };

    const actual = try assembler(opcodes, &labelMap, std.testing.allocator);
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualSlices(u8, &expected, actual);
}

test "assembler bad argument" {
    const input = "add";

    const tokens = try lexer(input, std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    var labelMap, const opcodes = try parser(tokens, std.testing.allocator);
    defer labelMap.deinit();
    defer std.testing.allocator.free(opcodes);
    defer freeLabelMap(&labelMap, std.testing.allocator);
    defer freeOpcodes(opcodes, std.testing.allocator);

    const actual = assembler(opcodes, &labelMap, std.testing.allocator);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "assembler bad label" {
    const input = "lbl1: ld BC lbl2";

    const tokens = try lexer(input, std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    var labelMap, const opcodes = try parser(tokens, std.testing.allocator);
    defer labelMap.deinit();
    defer std.testing.allocator.free(opcodes);
    defer freeLabelMap(&labelMap, std.testing.allocator);
    defer freeOpcodes(opcodes, std.testing.allocator);

    const actual = assembler(opcodes, &labelMap, std.testing.allocator);

    try std.testing.expectError(AssemblerError.UndefinedLabel, actual);
}

pub fn translate(code: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const tokens = try lexer(code, allocator);
    defer allocator.free(tokens);

    var labelMap, const opcodes = try parser(tokens, allocator);
    defer labelMap.deinit();
    defer allocator.free(opcodes);
    defer freeLabelMap(&labelMap, allocator);
    defer freeOpcodes(opcodes, allocator);

    return assembler(opcodes, &labelMap, allocator);
}

pub fn formatNext(stream: []const u8, writer: anytype, allocator: std.mem.Allocator) !void {
    const opcode, _ = Opcode.decode(allocator, stream) catch {
        try writer.writeAll("<UNK>");
        return;
    };
    defer opcode.free(allocator);
    try opcode.format(writer);
}

test "translate" {
    const input =
        \\ foo:           ; 1
        \\ LD B, C        ; 2
        \\ LD B, 0        ; 3
        \\ LD B, 255      ; 4
        \\ LD B, (HL)     ; 5
        \\ LD (HL), B     ; 6
        \\ LD (HL), 0     ; 7
        \\ LD (HL), 255   ; 8
        \\ LD A, (BC)     ; 9
        \\ LD A, (DE)     ; 10
        \\ LD (BC), A     ; 11
        \\ LD (DE), A     ; 12
        \\ LD A, (0)      ; 13
        \\ LD A, (65535)  ; 14
        \\ LD A, (foo)    ; 15
        \\ LD (0), A      ; 16
        \\ LD (65535), A  ; 17
        \\ LD (foo), A    ; 18
        \\ LDH A, (C)     ; 19
        \\ LDH (C), A     ; 20
        \\ LDH A, (0)     ; 21
        \\ LDH A, (255)   ; 22
        \\ LDH (0), A     ; 23
        \\ LDH (255), A   ; 24
        \\ LD A, (HL-)    ; 25
        \\ LD (HL-), A    ; 26
        \\ LD A, (HL+)    ; 27
        \\ LD (HL+), A    ; 28
        \\ LD HL, 0       ; 29
        \\ LD HL, 65535   ; 30
        \\ LD HL, foo     ; 31
        \\ LD (0), SP     ; 32
        \\ LD (65535), SP ; 33
        \\ LD (foo), SP   ; 34
        \\ LD SP, HL      ; 35
        \\ PUSH HL        ; 36
        \\ POP HL         ; 37
        \\ LD HL, SP+127  ; 38
        \\ LD HL, SP+0    ; 39
        \\ LD HL, SP-128  ; 40
        \\ ADD B          ; 41
        \\ ADD (HL)       ; 42
        \\ ADD 0          ; 43
        \\ ADD 255        ; 44
        \\ ADC B          ; 45
        \\ ADC (HL)       ; 46
        \\ ADC 0          ; 47
        \\ ADC 255        ; 48
        \\ SUB B          ; 49
        \\ SUB (HL)       ; 50
        \\ SUB 0          ; 51
        \\ SUB 255        ; 52
        \\ SBC B          ; 53
        \\ SBC (HL)       ; 54
        \\ SBC 0          ; 55
        \\ SBC 255        ; 56
        \\ CP B           ; 57
        \\ CP (HL)        ; 58
        \\ CP 0           ; 59
        \\ CP 255         ; 60
        \\ INC B          ; 61
        \\ INC (HL)       ; 62
        \\ DEC B          ; 63
        \\ DEC (HL)       ; 64
        \\ AND B          ; 65
        \\ AND (HL)       ; 66
        \\ AND 0          ; 67
        \\ AND 255        ; 68
        \\ OR B           ; 69
        \\ OR (HL)        ; 70
        \\ OR 0           ; 71
        \\ OR 255         ; 72
        \\ XOR B          ; 73
        \\ XOR (HL)       ; 74
        \\ XOR 0          ; 75
        \\ XOR 255        ; 76
        \\ CCF            ; 77
        \\ SCF            ; 78
        \\ DAA            ; 79
        \\ CPL            ; 80
        \\ INC HL         ; 81
        \\ DEC HL         ; 82
        \\ ADD HL, BC     ; 83
        \\ ADD SP, 127    ; 84
        \\ ADD SP, 0      ; 85
        \\ ADD SP, -128   ; 86
        \\ RLCA           ; 87
        \\ RRCA           ; 88
        \\ RLA            ; 89
        \\ RRA            ; 90
        \\ RLC B          ; 91
        \\ RLC (HL)       ; 92
        \\ RRC B          ; 93
        \\ RRC (HL)       ; 94
        \\ RL B           ; 95
        \\ RL (HL)        ; 96
        \\ RR B           ; 97
        \\ RR (HL)        ; 98
        \\ SLA B          ; 99
        \\ SLA (HL)       ; 100
        \\ SRA B          ; 101
        \\ SRA (HL)       ; 102
        \\ SWAP B         ; 103
        \\ SWAP (HL)      ; 104
        \\ SRL B          ; 105
        \\ SRL (HL)       ; 106
        \\ BIT 0, B       ; 107
        \\ BIT 0, (HL)    ; 108
        \\ RES 0, B       ; 109
        \\ RES 0, (HL)    ; 110
        \\ SET 0, B       ; 111
        \\ SET 0, (HL)    ; 112
        \\ JP 0           ; 113
        \\ JP 65535       ; 114
        \\ JP HL          ; 115
        \\ JP (HL)        ; 116
        \\ JP NZ, 0       ; 117
        \\ JP Z, 65535    ; 118
        \\ JP NC, 0       ; 119
        \\ JP C, 65535    ; 120
        \\ JR -128        ; 121
        \\ JR 0           ; 122
        \\ JR 127         ; 123
        \\ JR NZ, -128    ; 124
        \\ JR Z, 0        ; 125
        \\ JR NC, 0       ; 126
        \\ JR C, 127      ; 127
        \\ CALL 0         ; 128
        \\ CALL 65535     ; 129
        \\ CALL NZ, 0     ; 130
        \\ CALL Z, 65535  ; 131
        \\ CALL NC, 0     ; 132
        \\ CALL C, 65535  ; 133
        \\ RET            ; 134
        \\ RET NZ         ; 135
        \\ RET Z          ; 136
        \\ RET NC         ; 137
        \\ RET C          ; 138
        \\ RETI           ; 139
        \\ RST 0          ; 140
        \\ RST 7          ; 141
        \\ HALT           ; 142
        \\ STOP           ; 143
        \\ DI             ; 144
        \\ EI             ; 145
        \\ NOP            ; 146
    ;

    const expected = [_]u8{
        0b01_000_001, // LD B, C
        0b00_000_110, 0x00, // LD B, 0
        0b00_000_110, 0xFF, // LD B, 255
        0b01_000_110, // LD B, (HL)
        0b01_110_000, // LD (HL), B
        0b00110110, 0x00, // LD (HL), 0
        0b00110110, 0xFF, // LD (HL), 255
        0b0000_1010, // LD A, (BC)
        0b0001_1010, // LD A, (DE)
        0b0000_0010, // LD (BC), A
        0b0001_0010, // LD (DE), A
        0b1111_1010, 0x00, 0x00, // LD A, (0)
        0b1111_1010, 0xFF, 0xFF, // LD A, (65535)
        0b1111_1010, 0x00, 0x00, // LD A, (foo)
        0b1110_1010, 0x00, 0x00, // LD (0), A
        0b1110_1010, 0xFF, 0xFF, // LD (65535), A
        0b1110_1010, 0x00, 0x00, // LD (foo), A
        0b1111_0010, // LDH A, (C)
        0b1110_0010, // LDH (C), A
        0b1111_0000, 0x00, // LDH A, (0)
        0b1111_0000, 0xFF, // LDH A, (255)
        0b1110_0000, 0x00, // LDH (0), A
        0b1110_0000, 0xFF, // LDH (255), A
        0b001_11_010, // LD A, (HL-)
        0b001_10_010, // LD (HL-), A
        0b001_01_010, // LD A, (HL+)
        0b001_00_010, // LD (HL+), A
        0b00_10_0001, 0x00, 0x00, // LD HL, 0
        0b00_10_0001, 0xFF, 0xFF, // LD HL, 65535
        0b00_10_0001, 0x00, 0x00, // LD HL, foo
        0b0000_1000, 0x00, 0x00, // LD (0), SP
        0b0000_1000, 0xFF, 0xFF, // LD (65535), SP
        0b0000_1000, 0x00, 0x00, // LD (foo), SP
        0b11111001, // LD SP, HL
        0b11_10_0101, // PUSH HL
        0b11_10_0001, // POP HL
        0b11111000, 0x7F, // LD HL, SP+127
        0b11111000, 0x00, // LD HL, SP+0
        0b11111000, 0x80, // LD HL, SP-128
        0b10000_000, // ADD B
        0b10000_110, // ADD (HL)
        0b11000_110, 0x00, // ADD 0
        0b11000_110, 0xFF, // ADD 255
        0b10001_000, // ADC B
        0b10001_110, // ADC (HL)
        0b11001_110, 0x00, // ADC 0
        0b11001_110, 0xFF, // ADC 255
        0b10010_000, // SUB B
        0b10010_110, // SUB (HL)
        0b11010_110, 0x00, // SUB 0
        0b11010_110, 0xFF, // SUB 255
        0b10011_000, // SBC B
        0b10011_110, // SBC (HL)
        0b11011_110, 0x00, // SBC 0
        0b11011_110, 0xFF, // SBC 255
        0b10111_000, // CP B
        0b10011_110, // CP (HL)
        0b11111_110, 0x00, // CP 0
        0b11111_110, 0xFF, // CP 255
        0b00_000_100, // INC B
        0b00_110_100, // INC (HL)
        0b00_000_101, // DEC B
        0b00_110_101, // DEC (HL)
        0b10100_000, // AND B
        0b10100_110, // AND (HL)
        0b11100_110, 0x00, // AND 0
        0b11100_110, 0xFF, // AND 255
        0b10110_000, // OR B
        0b10110_110, // OR (HL)
        0b11110_110, 0x00, // OR 0
        0b11110_110, 0xFF, // OR 255
        0b10101_000, // XOR B
        0b10101_110, // XOR (HL)
        0b11101_110, 0x00, // XOR 0
        0b11101_110, 0xFF, // XOR 255
        0b00111111, // CCF
        0b00110111, // SCF
        0b00100111, // DAA
        0b00101111, // CPL
        0b00_10_0011, // INC HL
        0b00_10_1011, // DEC HL
        0b00_00_1001, // ADD HL, BC
        0b11101000, 0x7F, // ADD SP, 127
        0b11101000, 0x00, // ADD SP, 0
        0b11101000, 0x80, // ADD SP, -128
        0b00000111, // RLCA
        0b00001111, // RRCA
        0b00010111, // RLA
        0b00011111, // RRA
        0xCB, 0b00000000, // RLC B
        0xCB, 0b00000110, // RLC (HL)
        0xCB, 0b00001000, // RRC B
        0xCB, 0b00001110, // RRC (HL)
        0xCB, 0b00010000, // RL B
        0xCB, 0b00010110, // RL (HL)
        0xCB, 0b00011000, // RR B
        0xCB, 0b00011110, // RR (HL)
        0xCB, 0b00100000, // SLA B
        0xCB, 0b00100110, // SLA (HL)
        0xCB, 0b00101000, // SRA B
        0xCB, 0b00101110, // SRA (HL)
        0xCB, 0b00110000, // SWAP B
        0xCB, 0b00110110, // SWAP (HL)
        0xCB, 0b00111000, // SRL B
        0xCB, 0b00111110, // SRL (HL)
        0xCB, 0b01000000, // BIT 0, B
        0xCB, 0b01000110, // BIT 0, (HL)
        0xCB, 0b10000000, // RES 0, B
        0xCB, 0b10000110, // RES 0, (HL)
        0xCB, 0b11000000, // SET 0, B
        0xCB, 0b11000110, // SET 0, (HL)
        0b11000011, 0x00, 0x00, // JP 0
        0b11000011, 0xFF, 0xFF, // JP 65535
        0b11101001, // JP HL
        0b11101001, // JP (HL)
        0b110_00_010, 0x00, 0x00, // JP NZ, 0
        0b110_01_010, 0xFF, 0xFF, // JP Z, 65535
        0b110_10_010, 0x00, 0x00, // JP NC, 0
        0b110_11_010, 0xFF, 0xFF, // JP C, 65535
        0b00011000, 0x80, // JR -128
        0b00011000, 0x00, // JR 0
        0b00011000, 0x7F, // JR 127
        0b001_00_000, 0x80, // JR NZ, -128
        0b001_01_000, 0x00, // JR Z, 0
        0b001_10_000, 0x00, // JR NC, 0
        0b001_11_000, 0x7F, // JR C, 127
        0b11001101, 0x00, 0x00, // CALL 0
        0b11001101, 0xFF, 0xFF, // CALL 65535
        0b110_00_100, 0x00, 0x00, // CALL NZ, 0
        0b110_01_100, 0xFF, 0xFF, // CALL Z, 65535
        0b110_10_100, 0x00, 0x00, // CALL NC, 0
        0b110_11_100, 0xFF, 0xFF, // CALL C, 65535
        0b11001001, // RET
        0b110_00_000, // RET NZ
        0b110_01_000, // RET Z
        0b110_10_000, // RET NC
        0b110_11_000, // RET C
        0b11011001, // RETI
        0b11_000_111, // RST 0
        0b11_111_111, // RST 7
        0x76, // HALT
        0x10, // STOP
        0xF3, // DI
        0xFB, // EI
        0x00, // NOP
    };

    const actual = try translate(input, std.testing.allocator);
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualSlices(u8, &expected, actual);
}

test "disassemble" {
    const stream = [_]u8{
        0b01_000_001, // LD B, C
        0b00_000_110, 0x00, // LD B, 0
        0b00_000_110, 0xFF, // LD B, 255
        0b01_000_110, // LD B, (HL)
        0b01_110_000, // LD (HL), B
        0b00110110, 0x00, // LD (HL), 0
        0b00110110, 0xFF, // LD (HL), 255
        0b0000_1010, // LD A, (BC)
        0b0001_1010, // LD A, (DE)
        0b0000_0010, // LD (BC), A
        0b0001_0010, // LD (DE), A
        0b1111_1010, 0x00, 0x00, // LD A, (0)
        0b1111_1010, 0xFF, 0xFF, // LD A, (65535)
        0b1111_1010, 0x00, 0x00, // LD A, (foo)
        0b1110_1010, 0x00, 0x00, // LD (0), A
        0b1110_1010, 0xFF, 0xFF, // LD (65535), A
        0b1110_1010, 0x00, 0x00, // LD (foo), A
        0b1111_0010, // LDH A, (C)
        0b1110_0010, // LDH (C), A
        0b1111_0000, 0x00, // LDH A, (0)
        0b1111_0000, 0xFF, // LDH A, (255)
        0b1110_0000, 0x00, // LDH (0), A
        0b1110_0000, 0xFF, // LDH (255), A
        0b001_11_010, // LD A, (HL-)
        0b001_10_010, // LD (HL-), A
        0b001_01_010, // LD A, (HL+)
        0b001_00_010, // LD (HL+), A
        0b00_10_0001, 0x00, 0x00, // LD HL, 0
        0b00_10_0001, 0xFF, 0xFF, // LD HL, 65535
        0b00_10_0001, 0x00, 0x00, // LD HL, foo
        0b0000_1000, 0x00, 0x00, // LD (0), SP
        0b0000_1000, 0xFF, 0xFF, // LD (65535), SP
        0b0000_1000, 0x00, 0x00, // LD (foo), SP
        0b11111001, // LD SP, HL
        0b11_10_0101, // PUSH HL
        0b11_10_0001, // POP HL
        0b11111000, 0x7F, // LD HL, SP+127
        0b11111000, 0x00, // LD HL, SP+0
        0b11111000, 0x80, // LD HL, SP-128
        0b10000_000, // ADD B
        0b10000_110, // ADD (HL)
        0b11000_110, 0x00, // ADD 0
        0b11000_110, 0xFF, // ADD 255
        0b10001_000, // ADC B
        0b10001_110, // ADC (HL)
        0b11001_110, 0x00, // ADC 0
        0b11001_110, 0xFF, // ADC 255
        0b10010_000, // SUB B
        0b10010_110, // SUB (HL)
        0b11010_110, 0x00, // SUB 0
        0b11010_110, 0xFF, // SUB 255
        0b10011_000, // SBC B
        0b10011_110, // SBC (HL)
        0b11011_110, 0x00, // SBC 0
        0b11011_110, 0xFF, // SBC 255
        0b10111_000, // CP B
        0b10011_110, // CP (HL)
        0b11111_110, 0x00, // CP 0
        0b11111_110, 0xFF, // CP 255
        0b00_000_100, // INC B
        0b00_110_100, // INC (HL)
        0b00_000_101, // DEC B
        0b00_110_101, // DEC (HL)
        0b10100_000, // AND B
        0b10100_110, // AND (HL)
        0b11100_110, 0x00, // AND 0
        0b11100_110, 0xFF, // AND 255
        0b10110_000, // OR B
        0b10110_110, // OR (HL)
        0b11110_110, 0x00, // OR 0
        0b11110_110, 0xFF, // OR 255
        0b10101_000, // XOR B
        0b10101_110, // XOR (HL)
        0b11101_110, 0x00, // XOR 0
        0b11101_110, 0xFF, // XOR 255
        0b00111111, // CCF
        0b00110111, // SCF
        0b00100111, // DAA
        0b00101111, // CPL
        0b00_10_0011, // INC HL
        0b00_10_1011, // DEC HL
        0b00_00_1001, // ADD HL, BC
        0b11101000, 0x7F, // ADD SP, 127
        0b11101000, 0x00, // ADD SP, 0
        0b11101000, 0x80, // ADD SP, -128
        0b00000111, // RLCA
        0b00001111, // RRCA
        0b00010111, // RLA
        0b00011111, // RRA
        0xCB, 0b00000000, // RLC B
        0xCB, 0b00000110, // RLC (HL)
        0xCB, 0b00001000, // RRC B
        0xCB, 0b00001110, // RRC (HL)
        0xCB, 0b00010000, // RL B
        0xCB, 0b00010110, // RL (HL)
        0xCB, 0b00011000, // RR B
        0xCB, 0b00011110, // RR (HL)
        0xCB, 0b00100000, // SLA B
        0xCB, 0b00100110, // SLA (HL)
        0xCB, 0b00101000, // SRA B
        0xCB, 0b00101110, // SRA (HL)
        0xCB, 0b00110000, // SWAP B
        0xCB, 0b00110110, // SWAP (HL)
        0xCB, 0b00111000, // SRL B
        0xCB, 0b00111110, // SRL (HL)
        0xCB, 0b01000000, // BIT 0, B
        0xCB, 0b01000110, // BIT 0, (HL)
        0xCB, 0b10000000, // RES 0, B
        0xCB, 0b10000110, // RES 0, (HL)
        0xCB, 0b11000000, // SET 0, B
        0xCB, 0b11000110, // SET 0, (HL)
        0b11000011, 0x00, 0x00, // JP 0
        0b11000011, 0xFF, 0xFF, // JP 65535
        0b11101001, // JP HL
        0b11101001, // JP (HL)
        0b110_00_010, 0x00, 0x00, // JP NZ, 0
        0b110_01_010, 0xFF, 0xFF, // JP Z, 65535
        0b110_10_010, 0x00, 0x00, // JP NC, 0
        0b110_11_010, 0xFF, 0xFF, // JP C, 65535
        0b00011000, 0x80, // JR -128
        0b00011000, 0x00, // JR 0
        0b00011000, 0x7F, // JR 127
        0b001_00_000, 0x80, // JR NZ, -128
        0b001_01_000, 0x00, // JR Z, 0
        0b001_10_000, 0x00, // JR NC, 0
        0b001_11_000, 0x7F, // JR C, 127
        0b11001101, 0x00, 0x00, // CALL 0
        0b11001101, 0xFF, 0xFF, // CALL 65535
        0b110_00_100, 0x00, 0x00, // CALL NZ, 0
        0b110_01_100, 0xFF, 0xFF, // CALL Z, 65535
        0b110_10_100, 0x00, 0x00, // CALL NC, 0
        0b110_11_100, 0xFF, 0xFF, // CALL C, 65535
        0b11001001, // RET
        0b110_00_000, // RET NZ
        0b110_01_000, // RET Z
        0b110_10_000, // RET NC
        0b110_11_000, // RET C
        0b11011001, // RETI
        0b11_000_111, // RST 0
        0b11_111_111, // RST 7
        0x76, // HALT
        0x10, // STOP
        0xF3, // DI
        0xFB, // EI
        0x00, // NOP
    };

    const expected =
        \\LD B, C
        \\LD B, 0x0
        \\LD B, 0xFF
        \\LD B, (HL)
        \\LD (HL), B
        \\LD (HL), 0x0
        \\LD (HL), 0xFF
        \\LD A, (BC)
        \\LD A, (DE)
        \\LD (BC), A
        \\LD (DE), A
        \\LD A, (0x0)
        \\LD A, (0xFFFF)
        \\LD A, (0x0)
        \\LD (0x0), A
        \\LD (0xFFFF), A
        \\LD (0x0), A
        \\LDH A, (C)
        \\LDH (C), A
        \\LDH A, (0x0)
        \\LDH A, (0xFF)
        \\LDH (0x0), A
        \\LDH (0xFF), A
        \\LD A, (HL-)
        \\LD (HL-), A
        \\LD A, (HL+)
        \\LD (HL+), A
        \\LD HL, 0x0
        \\LD HL, 0xFFFF
        \\LD HL, 0x0
        \\LD (0x0), SP
        \\LD (0xFFFF), SP
        \\LD (0x0), SP
        \\LD SP, HL
        \\PUSH HL
        \\POP HL
        \\LD HL, SP+127
        \\LD HL, SP+0
        \\LD HL, SP-128
        \\ADD B
        \\ADD (HL)
        \\ADD 0x0
        \\ADD 0xFF
        \\ADC B
        \\ADC (HL)
        \\ADC 0x0
        \\ADC 0xFF
        \\SUB B
        \\SUB (HL)
        \\SUB 0x0
        \\SUB 0xFF
        \\SBC B
        \\SBC (HL)
        \\SBC 0x0
        \\SBC 0xFF
        \\CP B
        \\SBC (HL)
        \\CP 0x0
        \\CP 0xFF
        \\INC B
        \\INC (HL)
        \\DEC B
        \\DEC (HL)
        \\AND B
        \\AND (HL)
        \\AND 0x0
        \\AND 0xFF
        \\OR B
        \\OR (HL)
        \\OR 0x0
        \\OR 0xFF
        \\XOR B
        \\XOR (HL)
        \\XOR 0x0
        \\XOR 0xFF
        \\CCF
        \\SCF
        \\DAA
        \\CPL
        \\INC HL
        \\DEC HL
        \\ADD HL, BC
        \\ADD SP, 0x7F
        \\ADD SP, 0x0
        \\ADD SP, -128
        \\RLCA
        \\RRCA
        \\RLA
        \\RRA
        \\RLC E
        \\RLC E
        \\RRC E
        \\RRC E
        \\RL E
        \\RL E
        \\RR E
        \\RR E
        \\SLA E
        \\SLA E
        \\SRA E
        \\SRA E
        \\SWAP E
        \\SWAP E
        \\SRL E
        \\SRL E
        \\BIT 0x1, E
        \\BIT 0x1, E
        \\RES 0x1, E
        \\RES 0x1, E
        \\SET 0x1, E
        \\SET 0x1, E
        \\JP 0x0
        \\JP 0xFFFF
        \\JP HL
        \\JP HL
        \\JP NZ, 0x0
        \\JP Z, 0xFFFF
        \\JP NC, 0x0
        \\JP C, 0xFFFF
        \\JR -128
        \\JR 0x0
        \\JR 0x7F
        \\JR NZ, -128
        \\JR Z, 0x0
        \\JR NC, 0x0
        \\JR C, 0x7F
        \\CALL 0x0
        \\CALL 0xFFFF
        \\CALL NZ, 0x0
        \\CALL Z, 0xFFFF
        \\CALL NC, 0x0
        \\CALL C, 0xFFFF
        \\RET
        \\RET NZ
        \\RET Z
        \\RET NC
        \\RET C
        \\RETI
        \\RST 0x0
        \\RST 0x7
        \\HALT
        \\STOP
        \\DI
        \\EI
        \\NOP
        \\
    ;

    var buffer: [0x1000]u8 = undefined;
    var source = std.io.StreamSource{ .buffer = std.io.fixedBufferStream(&buffer) };
    const writer = source.writer();

    var slice: []const u8 = &stream;
    while (slice.len > 0) {
        const opcode, slice = try Opcode.decode(std.testing.allocator, slice);
        defer opcode.free(std.testing.allocator);
        try opcode.format(writer);
        try writer.writeByte('\n');
    }

    try std.testing.expectEqualStrings(expected, source.buffer.getWritten());
}
