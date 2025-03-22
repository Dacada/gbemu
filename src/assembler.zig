const std = @import("std");
const builtin = @import("builtin");

const logger = std.log.scoped(.the_module);

fn logError(comptime fmt: []const u8, args: anytype) void {
    if (!builtin.is_test) {
        logger.err(fmt, args);
    } else {
        logger.warn(fmt, args);
    }
}

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

fn validateLabel(label: *const Token) !void {
    if (label.source[0] != '_' and !std.ascii.isAlphabetic(label.source[0])) {
        logError("Line {d}: Invalid label '{s}'.", .{ label.line, label.source });
        return AssemblerError.InvalidLabel;
    }
    for (label.source) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') {
            logError("Line {d}: Invalid label '{s}'.", .{ label.line, label.source });
            return AssemblerError.InvalidLabel;
        }
    }
}

test "validateLabel incorrect 1" {
    const value = Token{
        .line = 0,
        .source = "-0x1",
        .which = TokenKind.Label,
    };
    const expected = AssemblerError.InvalidLabel;

    const actual = validateLabel(&value);

    try std.testing.expectError(expected, actual);
}

test "validateLabel incorrect 2" {
    const value = Token{
        .line = 0,
        .source = "_0$1",
        .which = TokenKind.Label,
    };
    const expected = AssemblerError.InvalidLabel;

    const actual = validateLabel(&value);

    try std.testing.expectError(expected, actual);
}

test "validateLabel correct" {
    const value = Token{
        .line = 0,
        .source = "_01",
        .which = TokenKind.Label,
    };
    try validateLabel(&value);
}

const Instruction = enum { TESTINGONLY, LD, LDH, PUSH, POP, ADD, ADC, SUB, SBC, CP, INC, DEC, AND, OR, XOR, CCF, SCF, DAA, CPL, JMP };
const Register8 = enum { A, B, C, D, E, F, H, L };
const Register16 = enum { AF, BC, DE, HL, SP };
const ArgumentType = enum {
    Immediate8BitUnsigned,
    Immediate8BitSigned,
    Immediate16Bit,
    Register8Bit,
    Register16Bit,
    IndirectImmediate8Bit,
    IndirectImmediate16Bit,
    IndirectRegister8Bit,
    IndirectRegister16Bit,
    IndirectRegister16BitInc,
    IndirectRegister16BitDec,
    Label,
};

const Argument = union(ArgumentType) {
    Immediate8BitUnsigned: u8,
    Immediate8BitSigned: i8,
    Immediate16Bit: u16,
    Register8Bit: Register8,
    Register16Bit: Register16,
    IndirectImmediate8Bit: u8,
    IndirectImmediate16Bit: u16,
    IndirectRegister8Bit: Register8,
    IndirectRegister16Bit: Register16,
    IndirectRegister16BitInc: Register16,
    IndirectRegister16BitDec: Register16,
    Label: []u8,

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

    fn parseReg8(input: []const u8, indirect: bool) !?Argument {
        const reg8 = std.meta.stringToEnum(Register8, input);
        if (reg8 != null) {
            if (indirect) {
                return Argument{
                    .IndirectRegister8Bit = reg8.?,
                };
            }
            return Argument{
                .Register8Bit = reg8.?,
            };
        }

        return null;
    }

    fn parseReg16(input: []const u8, indirect: bool) !?Argument {
        var incdec: ?bool = null;
        var inputForReg16: []const u8 = undefined;
        if (input[input.len - 1] == '+') {
            incdec = true;
            inputForReg16 = input[0..(input.len - 1)];
        } else if (input[input.len - 1] == '-') {
            incdec = false;
            inputForReg16 = input[0..(input.len - 1)];
        } else {
            inputForReg16 = input;
        }

        const reg16 = std.meta.stringToEnum(Register16, inputForReg16);
        if (reg16 != null) {
            if (incdec == null) {
                if (indirect) {
                    return Argument{
                        .IndirectRegister16Bit = reg16.?,
                    };
                } else {
                    return Argument{
                        .Register16Bit = reg16.?,
                    };
                }
            }
            if (!indirect) {
                return AssemblerError.InvalidArgument;
            }
            if (incdec.?) {
                return Argument{
                    .IndirectRegister16BitInc = reg16.?,
                };
            } else {
                return Argument{
                    .IndirectRegister16BitDec = reg16.?,
                };
            }
        }

        return null;
    }

    fn parseImmediate(input: []const u8, indirect: bool) !?Argument {
        var base: u8 = 10;
        var start: usize = 0;
        if (input[0] == '0') {
            if (input[1] == 'B') {
                base = 2;
            } else if (input[1] == 'X') {
                base = 16;
            } else if (input[1] == 'O') {
                base = 8;
            }
            start = 2;
        }

        const number = std.fmt.parseInt(i32, input[start..], base) catch null;
        if (number != null) {
            if (number.? > 0xFFFF or number.? < -128) {
                return AssemblerError.InvalidArgument;
            }
            if (number.? > 0xFF) {
                if (indirect) {
                    return Argument{
                        .IndirectImmediate16Bit = @intCast(number.?),
                    };
                } else {
                    return Argument{
                        .Immediate16Bit = @intCast(number.?),
                    };
                }
            }
            if (number.? < 0) {
                return Argument{
                    .Immediate8BitSigned = @intCast(number.?),
                };
            }
            if (indirect) {
                return Argument{
                    .IndirectImmediate8Bit = @intCast(number.?),
                };
            }
            return Argument{
                .Immediate8BitUnsigned = @intCast(number.?),
            };
        }

        return null;
    }

    fn fromToken(token: *const Token, allocator: std.mem.Allocator) !Argument {
        const indirect, const source = Argument.parseIndirect(token.source) catch |e| {
            logError("Line {d}: Missmatched parenthesis '{s}'", .{ token.line, token.source });
            return e;
        };

        var upper: ?[]u8 = try toUpper(source, allocator);
        defer if (upper != null) allocator.free(upper.?);

        if (Argument.parseReg8(upper.?, indirect) catch |e| {
            logError("Line {d}: Invalid argument: '{s}' Cannot have an indirect 8bit register.", .{ token.line, token.source });
            return e;
        }) |res| {
            return res;
        }

        if (Argument.parseReg16(upper.?, indirect) catch |e| {
            logError("Line {d}: Invalid register '{s}'", .{ token.line, token.source });
            return e;
        }) |res| {
            return res;
        }

        if (Argument.parseImmediate(upper.?, indirect) catch |e| {
            logError("Line {d}: Invalid integer '{s}'", .{ token.line, token.source });
            return e;
        }) |res| {
            return res;
        }

        if (indirect) {
            logError("Line {d}: Invalid label '{s}'", .{ token.line, token.source });
            return AssemblerError.InvalidArgument;
        }

        try validateLabel(token);

        const res = upper.?;
        upper = null;
        return Argument{
            .Label = res,
        };
    }

    fn free(self: *Argument, allocator: std.mem.Allocator) void {
        switch (self.*) {
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

test "parseReg8 1" {
    const input = "B";
    const indirect = false;
    const expected = Argument{
        .Register8Bit = Register8.B,
    };

    const actual = try Argument.parseReg8(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg8 2" {
    const input = "B";
    const indirect = true;
    const expected = Argument{
        .IndirectRegister8Bit = Register8.B,
    };

    const actual = Argument.parseReg8(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg8 3" {
    const input = "X";
    const indirect = false;
    const expected = null;

    const actual = try Argument.parseReg8(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg16 1" {
    const input = "HL";
    const indirect = true;
    const expected = Argument{
        .IndirectRegister16Bit = Register16.HL,
    };

    const actual = try Argument.parseReg16(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg16 2" {
    const input = "HL";
    const indirect = false;
    const expected = Argument{
        .Register16Bit = Register16.HL,
    };

    const actual = try Argument.parseReg16(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg16 3" {
    const input = "HL+";
    const indirect = false;
    const expected = AssemblerError.InvalidArgument;

    const actual = Argument.parseReg16(input, indirect);

    try std.testing.expectError(expected, actual);
}

test "parseReg16 4" {
    const input = "HL+";
    const indirect = true;
    const expected = Argument{
        .IndirectRegister16BitInc = Register16.HL,
    };

    const actual = try Argument.parseReg16(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseReg16 5" {
    const input = "HL-";
    const indirect = true;
    const expected = Argument{
        .IndirectRegister16BitDec = Register16.HL,
    };

    const actual = try Argument.parseReg16(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 1" {
    const input = "0X10000";
    const indirect = false;
    const expected = AssemblerError.InvalidArgument;

    const actual = Argument.parseImmediate(input, indirect);

    try std.testing.expectError(expected, actual);
}

test "parseImmediate 2" {
    const input = "-50000";
    const indirect = false;
    const expected = AssemblerError.InvalidArgument;

    const actual = Argument.parseImmediate(input, indirect);

    try std.testing.expectError(expected, actual);
}

test "parseImmediate 3" {
    const input = "0XFFFF";
    const indirect = true;
    const expected = Argument{
        .IndirectImmediate16Bit = 0xFFFF,
    };

    const actual = Argument.parseImmediate(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 4" {
    const input = "0XFFFF";
    const indirect = false;
    const expected = Argument{
        .Immediate16Bit = 0xFFFF,
    };

    const actual = Argument.parseImmediate(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 5" {
    const input = "0XFF";
    const indirect = true;
    const expected = Argument{
        .IndirectImmediate8Bit = 0xFF,
    };

    const actual = Argument.parseImmediate(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 6" {
    const input = "-100";
    const indirect = false;
    const expected = Argument{
        .Immediate8BitSigned = -100,
    };

    const actual = Argument.parseImmediate(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "parseImmediate 7" {
    const input = "100";
    const indirect = false;
    const expected = Argument{
        .Immediate8BitUnsigned = 100,
    };

    const actual = Argument.parseImmediate(input, indirect);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken 0xFF" {
    const input = Token{
        .line = 0,
        .source = "0XFF",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .Immediate8BitUnsigned = 0xFF,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken -100" {
    const input = Token{
        .line = 0,
        .source = "-100",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .Immediate8BitSigned = -100,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken 0xFFFF" {
    const input = Token{
        .line = 0,
        .source = "0XFFFF",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .Immediate16Bit = 0xFFFF,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken (0xFFFF)" {
    const input = Token{
        .line = 0,
        .source = "(0XFFFF)",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .IndirectImmediate16Bit = 0xFFFF,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken B" {
    const input = Token{
        .line = 0,
        .source = "B",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .Register8Bit = Register8.B,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken BC" {
    const input = Token{
        .line = 0,
        .source = "BC",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .Register16Bit = Register16.BC,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken (BC)" {
    const input = Token{
        .line = 0,
        .source = "(BC)",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .IndirectRegister16Bit = Register16.BC,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken (B)" {
    const input = Token{
        .line = 0,
        .source = "(B)",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .IndirectRegister8Bit = Register8.B,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken (BC+)" {
    const input = Token{
        .line = 0,
        .source = "(BC+)",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .IndirectRegister16BitInc = Register16.BC,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken (BC-)" {
    const input = Token{
        .line = 0,
        .source = "(BC-)",
        .which = TokenKind.Argument,
    };
    const expected = Argument{
        .IndirectRegister16BitDec = Register16.BC,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Argument.fromToken foo" {
    const input = Token{
        .line = 0,
        .source = "foo",
        .which = TokenKind.Argument,
    };
    const label = try toUpper("foo", std.testing.allocator);
    defer std.testing.allocator.free(label);
    const expected = Argument{
        .Label = label,
    };

    var actual = try Argument.fromToken(&input, std.testing.allocator);
    defer actual.free(std.testing.allocator);

    try std.testing.expectEqualDeep(expected, actual);
}

test "Argument.fromToken (BC" {
    const input = Token{
        .line = 0,
        .source = "(BC",
        .which = TokenKind.Argument,
    };
    const expected = AssemblerError.MissmatchedParenthesis;

    const actual = Argument.fromToken(&input, std.testing.allocator);

    try std.testing.expectError(expected, actual);
}

test "Argument.fromToken (B+)" {
    const input = Token{
        .line = 0,
        .source = "(B+)",
        .which = TokenKind.Argument,
    };
    const expected = AssemblerError.InvalidArgument;

    const actual = Argument.fromToken(&input, std.testing.allocator);

    try std.testing.expectError(expected, actual);
}

test "Argument.fromToken 0xFFFFFFFF" {
    const input = Token{
        .line = 0,
        .source = "0XFFFFFFFF",
        .which = TokenKind.Argument,
    };
    const expected = AssemblerError.InvalidLabel;

    const actual = Argument.fromToken(&input, std.testing.allocator);

    try std.testing.expectError(expected, actual);
}

test "Argument.fromToken -99999999" {
    const input = Token{
        .line = 0,
        .source = "-99999999",
        .which = TokenKind.Argument,
    };
    const expected = AssemblerError.InvalidArgument;

    const actual = Argument.fromToken(&input, std.testing.allocator);

    try std.testing.expectError(expected, actual);
}

test "Argument.fromToken -0x1" {
    const input = Token{
        .line = 0,
        .source = "-0X1",
        .which = TokenKind.Argument,
    };
    const expected = AssemblerError.InvalidLabel;

    const actual = Argument.fromToken(&input, std.testing.allocator);

    try std.testing.expectError(expected, actual);
}

const Register8BitArgumentDefinition = union(enum) {
    Offset: usize,
    Register: Register8,
};

const Register16BitArgumentDefinition = union(enum) {
    Offset: usize,
    Register: Register16,
};

const ArgumentDefinition = union(ArgumentType) {
    Immediate8BitUnsigned,
    Immediate8BitSigned,
    Immediate16Bit,
    Register8Bit: Register8BitArgumentDefinition,
    Register16Bit: Register16BitArgumentDefinition,
    IndirectImmediate8Bit,
    IndirectImmediate16Bit,
    IndirectRegister8Bit: Register8,
    IndirectRegister16Bit: Register16,
    IndirectRegister16BitInc: Register16,
    IndirectRegister16BitDec: Register16,
    Label,

    fn encode(self: ArgumentDefinition, opcode: u8, arg: anytype) !struct { u8, ?u8, ?u8 } {
        switch (self) {
            .Immediate8BitUnsigned => {
                return .{ opcode, arg.Immediate8BitUnsigned, null };
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
            .Register8Bit => |x| {
                switch (x) {
                    .Offset => |offset| {
                        const reg_code = try ArgumentDefinition.encodeRegister8(arg.Register8Bit);
                        const new_opcode = opcode | (reg_code << @intCast(offset));
                        return .{ new_opcode, null, null };
                    },
                    .Register => |register| {
                        if (arg.Register8Bit != register) {
                            logError("Unexpected register.", .{});
                            return AssemblerError.InvalidInstructionArguments;
                        }
                        return .{ opcode, null, null };
                    },
                }
            },
            .Register16Bit => |x| {
                switch (x) {
                    .Offset => |offset| {
                        const reg_code = try ArgumentDefinition.encodeRegister16(arg.Register16Bit);
                        const new_opcode = opcode | (reg_code << @intCast(offset));
                        return .{ new_opcode, null, null };
                    },
                    .Register => |register| {
                        if (arg.Register16Bit != register) {
                            logError("Unexpected register.", .{});
                            return AssemblerError.InvalidInstructionArguments;
                        }
                        return .{ opcode, null, null };
                    },
                }
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
                    logError("Unexpected register.", .{});
                    return AssemblerError.InvalidInstructionArguments;
                }
                return .{ opcode, null, null };
            },
            .IndirectRegister16Bit => |register| {
                if (arg.IndirectRegister16Bit != register) {
                    logError("Unexpected register.", .{});
                    return AssemblerError.InvalidInstructionArguments;
                }
                return .{ opcode, null, null };
            },
            .IndirectRegister16BitInc => |register| {
                if (arg.IndirectRegister16BitInc != register) {
                    logError("Unexpected register.", .{});
                    return AssemblerError.InvalidInstructionArguments;
                }
                return .{ opcode, null, null };
            },
            .IndirectRegister16BitDec => |register| {
                if (arg.IndirectRegister16BitDec != register) {
                    logError("Unexpected register.", .{});
                    return AssemblerError.InvalidInstructionArguments;
                }
                return .{ opcode, null, null };
            },
            .Label => {
                if (builtin.is_test) {
                    return .{ opcode, 0xAA, 0xAA };
                } else {
                    return .{ opcode, 0, 0 };
                }
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
                logError("Unexpected register.", .{});
                return AssemblerError.InvalidInstructionArguments;
            },
        };
    }

    fn encodeRegister16(r: Register16) !u8 {
        return switch (r) {
            Register16.BC => 0b00,
            Register16.DE => 0b01,
            Register16.HL => 0b10,
            Register16.SP => 0b11,
            else => {
                logError("Unexpected register.", .{});
                return AssemblerError.InvalidInstructionArguments;
            },
        };
    }
};

test "ArgumentDefinition.encode Immediate8BitUnsigned" {
    const definition = ArgumentDefinition{
        .Immediate8BitUnsigned = {},
    };
    const argument = Argument{
        .Immediate8BitUnsigned = 0xFF,
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
    const argument = Argument{
        .Immediate8BitSigned = -128,
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
    const argument = Argument{
        .Immediate16Bit = 0xFFEE,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, 0xEE, 0xFF };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

test "ArgumentDefinition.encode Register8Bit offset" {
    const definition = ArgumentDefinition{
        .Register8Bit = Register8BitArgumentDefinition{
            .Offset = 3,
        },
    };
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
        .Register8Bit = Register8.B,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.encode Register16Bit offset" {
    const definition = ArgumentDefinition{
        .Register16Bit = Register16BitArgumentDefinition{
            .Offset = 5,
        },
    };
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
        .Register16Bit = Register16.BC,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.encode IndirectImmediate8Bit" {
    const definition = ArgumentDefinition{
        .IndirectImmediate8Bit = {},
    };
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
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
    const argument = Argument{
        .IndirectRegister16BitDec = Register16.DE,
    };
    const opcode = 0x00;

    const actual = definition.encode(opcode, argument);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
}

test "ArgumentDefinition.encode Label" {
    const definition = ArgumentDefinition{
        .Label = {},
    };
    const lbl = try toUpper("foo", std.testing.allocator);
    defer std.testing.allocator.free(lbl);
    const argument = Argument{
        .Label = lbl,
    };
    const opcode = 0x00;
    const expected: struct { u8, ?u8, ?u8 } = .{ 0x00, 0xAA, 0xAA };

    const actual = try definition.encode(opcode, argument);

    try std.testing.expectEqualDeep(expected, actual);
}

const OpcodeDefinition = struct {
    instr: Instruction,
    arg1: ?ArgumentDefinition,
    arg2: ?ArgumentDefinition,
    base_opcode: u8,
};

const defined_opcodes =
    [_]OpcodeDefinition{
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = ArgumentDefinition{
            .Immediate16Bit = {},
        },
        .arg2 = ArgumentDefinition{
            .Immediate16Bit = {},
        },
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = ArgumentDefinition{
            .Immediate8BitUnsigned = {},
        },
        .arg2 = ArgumentDefinition{
            .Immediate16Bit = {},
        },
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = ArgumentDefinition{
            .Immediate16Bit = {},
        },
        .arg2 = ArgumentDefinition{
            .Immediate8BitUnsigned = {},
        },
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = ArgumentDefinition{
            .Immediate8BitUnsigned = {},
        },
        .arg2 = ArgumentDefinition{
            .Immediate8BitUnsigned = {},
        },
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = null,
        .arg2 = ArgumentDefinition{
            .Immediate16Bit = {},
        },
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = null,
        .arg2 = ArgumentDefinition{
            .Immediate8BitUnsigned = {},
        },
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = ArgumentDefinition{
            .Immediate16Bit = {},
        },
        .arg2 = null,
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = ArgumentDefinition{
            .Immediate8BitUnsigned = {},
        },
        .arg2 = null,
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = ArgumentDefinition{
            .Label = {},
        },
        .arg2 = null,
        .base_opcode = 0b01010101,
    },
    OpcodeDefinition{
        .instr = Instruction.TESTINGONLY,
        .arg1 = null,
        .arg2 = null,
        .base_opcode = 0b01010101,
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
            .Immediate8BitUnsigned = {},
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
            .Immediate8BitUnsigned = {},
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
                .Offset = 4,
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
            .Register16Bit = Register16BitArgumentDefinition{
                .Offset = 4,
            },
        },
        .arg2 = ArgumentDefinition{
            .Label = {},
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
                .Offset = 4,
            },
        },
        .arg2 = null,
        .base_opcode = 0b11_00_0101,
    },
    OpcodeDefinition{
        .instr = Instruction.POP,
        .arg1 = ArgumentDefinition{
            .Register16Bit = Register16BitArgumentDefinition{
                .Offset = 4,
            },
        },
        .arg2 = null,
        .base_opcode = 0b11_00_0001,
    },
    OpcodeDefinition{
        .instr = Instruction.LD,
        .arg1 = ArgumentDefinition{
            .Register16Bit = Register16BitArgumentDefinition{
                .Register = Register16.SP,
            },
        },
        .arg2 = ArgumentDefinition{
            .Immediate8BitUnsigned = {},
        },
        .base_opcode = 0b11111000,
    },
    OpcodeDefinition{
        .instr = Instruction.LD,
        .arg1 = ArgumentDefinition{
            .Register16Bit = Register16BitArgumentDefinition{
                .Register = Register16.SP,
            },
        },
        .arg2 = ArgumentDefinition{
            .Immediate8BitSigned = {},
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
            .Immediate8BitUnsigned = {},
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
            .Immediate8BitUnsigned = {},
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
            .Immediate8BitUnsigned = {},
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
            .Immediate8BitUnsigned = {},
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
            .Immediate8BitUnsigned = {},
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
            .Immediate8BitUnsigned = {},
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
            .Immediate8BitUnsigned = {},
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
            .Immediate8BitUnsigned = {},
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
};

const Opcode = struct {
    line: usize,
    instr: Instruction,
    arg1: ?Argument,
    arg2: ?Argument,

    fn fromToken(token: *const Token, allocator: std.mem.Allocator) !Opcode {
        const token_capital = try toUpper(token.source, allocator);
        defer allocator.free(token_capital);
        const instr = std.meta.stringToEnum(Instruction, token_capital);
        if (instr == null) {
            logError("Line {d}: Invalid instruction '{s}'", .{ token.line, token.source });
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
        logError("Line {d}: Too many arguments for instruction '{s}'", .{ self.line, @tagName(self.instr) });
        return AssemblerError.InvalidArgumentCount;
    }

    fn free(self: *Opcode, allocator: std.mem.Allocator) void {
        if (self.arg1 != null) {
            self.arg1.?.free(allocator);
        }
        if (self.arg2 != null) {
            self.arg2.?.free(allocator);
        }
    }

    fn encode(self: *const Opcode, stream: *std.ArrayList(u8)) ![]u8 {
        for (defined_opcodes) |opcode_definition| {
            if (opcode_definition.instr != self.instr) {
                continue;
            }

            if ((opcode_definition.arg1 != null and self.arg1 == null) or (opcode_definition.arg1 == null and self.arg1 != null)) {
                continue;
            }

            if ((opcode_definition.arg2 != null and self.arg2 == null) or (opcode_definition.arg2 == null and self.arg2 != null)) {
                continue;
            }

            if (opcode_definition.arg1 != null and self.arg1 != null and @as(ArgumentType, opcode_definition.arg1.?) != @as(ArgumentType, self.arg1.?)) {
                continue;
            }

            if (opcode_definition.arg2 != null and self.arg2 != null and @as(ArgumentType, opcode_definition.arg2.?) != @as(ArgumentType, self.arg2.?)) {
                continue;
            }

            var opcode = opcode_definition.base_opcode;
            opcode, const imm1, const imm2 = if (opcode_definition.arg1) |arg| arg.encode(opcode, self.arg1.?) catch |e| {
                logError("Line {d}. Invalid argument.", .{self.line});
                return e;
            } else .{ opcode, null, null };
            opcode, const imm3, const imm4 = if (opcode_definition.arg2) |arg| arg.encode(opcode, self.arg2.?) catch |e| {
                logError("Line {d}. Invalid argument.", .{self.line});
                return e;
            } else .{ opcode, null, null };

            var size: usize = 0;
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

        logError("Line {d}: Invalid instruction/argument combination.", .{self.line});
        return AssemblerError.InvalidInstructionArguments;
    }
};

test "Opcode.fromToken 1" {
    const input = Token{
        .line = 0,
        .source = "TESTINGONLY",
        .which = TokenKind.Instruction,
    };
    const expected = Opcode{
        .line = 0,
        .instr = Instruction.TESTINGONLY,
        .arg1 = null,
        .arg2 = null,
    };

    const actual = try Opcode.fromToken(&input, std.testing.allocator);

    try std.testing.expectEqual(expected, actual);
}

test "Opcode.fromToken 2" {
    const input = Token{
        .line = 0,
        .source = "FOO",
        .which = TokenKind.Instruction,
    };
    const expected = AssemblerError.InvalidInstruction;

    const actual = Opcode.fromToken(&input, std.testing.allocator);

    try std.testing.expectError(expected, actual);
}

test "addArgument 1" {
    var opcode = Opcode{
        .line = 0,
        .instr = Instruction.TESTINGONLY,
        .arg1 = null,
        .arg2 = null,
    };
    const arg = Argument{
        .Register8Bit = Register8.B,
    };

    try opcode.addArgument(arg);

    try std.testing.expectEqual(arg, opcode.arg1);
    try std.testing.expectEqual(null, opcode.arg2);
}

test "addArgument 2" {
    const arg1 = Argument{
        .Register8Bit = Register8.B,
    };
    var opcode = Opcode{
        .line = 0,
        .instr = Instruction.TESTINGONLY,
        .arg1 = arg1,
        .arg2 = null,
    };
    const arg = Argument{
        .Immediate8BitUnsigned = 0,
    };

    try opcode.addArgument(arg);

    try std.testing.expectEqual(arg1, opcode.arg1);
    try std.testing.expectEqual(arg, opcode.arg2);
}

test "addArgument 3" {
    const arg1 = Argument{
        .Register8Bit = Register8.B,
    };
    const arg2 = Argument{
        .Immediate8BitUnsigned = 0,
    };
    var opcode = Opcode{
        .line = 0,
        .instr = Instruction.TESTINGONLY,
        .arg1 = arg1,
        .arg2 = arg2,
    };
    const arg = Argument{
        .Immediate16Bit = 0xFFFF,
    };

    try std.testing.expectError(AssemblerError.InvalidArgumentCount, opcode.addArgument(arg));
}

test "encode 16 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101, 0xBB, 0xAA, 0xDD, 0xCC };
    const expected_slice = [_]u8{ 0b01010101, 0xBB, 0xAA, 0xDD, 0xCC };

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = Argument{
            .Immediate16Bit = 0xAABB,
        },
        .arg2 = Argument{
            .Immediate16Bit = 0xCCDD,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode 16 8" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101, 0xBB, 0xAA, 0xCC };
    const expected_slice = [_]u8{ 0b01010101, 0xBB, 0xAA, 0xCC };

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = Argument{
            .Immediate16Bit = 0xAABB,
        },
        .arg2 = Argument{
            .Immediate8BitUnsigned = 0xCC,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode 16 null" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101, 0xBB, 0xAA };
    const expected_slice = [_]u8{ 0b01010101, 0xBB, 0xAA };

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = Argument{
            .Immediate16Bit = 0xAABB,
        },
        .arg2 = null,
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode 8 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101, 0xAA, 0xCC, 0xBB };
    const expected_slice = [_]u8{ 0b01010101, 0xAA, 0xCC, 0xBB };

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = Argument{
            .Immediate8BitUnsigned = 0xAA,
        },
        .arg2 = Argument{
            .Immediate16Bit = 0xBBCC,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode 8 8" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101, 0xAA, 0xBB };
    const expected_slice = [_]u8{ 0b01010101, 0xAA, 0xBB };

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = Argument{
            .Immediate8BitUnsigned = 0xAA,
        },
        .arg2 = Argument{
            .Immediate8BitUnsigned = 0xBB,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode 8 null" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101, 0xAA };
    const expected_slice = [_]u8{ 0b01010101, 0xAA };

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = Argument{
            .Immediate8BitUnsigned = 0xAA,
        },
        .arg2 = null,
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode null 16" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101, 0xBB, 0xAA };
    const expected_slice = [_]u8{ 0b01010101, 0xBB, 0xAA };

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = null,
        .arg2 = Argument{
            .Immediate16Bit = 0xAABB,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode null 8" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101, 0xAA };
    const expected_slice = [_]u8{ 0b01010101, 0xAA };

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = null,
        .arg2 = Argument{
            .Immediate8BitUnsigned = 0xAA,
        },
        .line = 0,
    };

    const actual = try opcode.encode(&stream);

    try std.testing.expectEqualSlices(u8, &expected_stream, stream.items);
    try std.testing.expectEqualSlices(u8, &expected_slice, actual);
}

test "encode null null" {
    var stream = std.ArrayList(u8).init(std.testing.allocator);
    try stream.append(0x11);
    defer stream.deinit();

    const expected_stream = [_]u8{ 0x11, 0b01010101 };
    const expected_slice = [_]u8{0b01010101};

    const opcode = Opcode{
        .instr = Instruction.TESTINGONLY,
        .arg1 = null,
        .arg2 = null,
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
        .instr = Instruction.ADD,
        .arg1 = null,
        .arg2 = null,
        .line = 0,
    };

    const actual = opcode.encode(&stream);

    try std.testing.expectError(AssemblerError.InvalidInstructionArguments, actual);
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

                try validateLabel(&token);
                if (labels.contains(upper)) {
                    logError("Line {d}: Redefinition of label '{s}'", .{ token.line, token.source });
                    return AssemblerError.RedefinedLabel;
                }
                try labels.put(upper, idx);
            },
            TokenKind.Instruction => {
                var next = try Opcode.fromToken(&token, allocator);
                errdefer next.free(allocator);

                if (current != null) {
                    try opcodes.append(current.?);
                }
                idx += 1;
                current = next;
            },
            TokenKind.Argument => {
                var arg = try Argument.fromToken(&token, allocator);
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
        \\   testingonly B 2
        \\ lbl2: testingonly C 3
        \\ lbl3:
    ;

    const tokens = try lexer(input, std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    var expected_labels = std.StringHashMap(usize).init(std.testing.allocator);
    defer expected_labels.deinit();

    try expected_labels.put("LBL1", 0);
    try expected_labels.put("LBL2", 1);
    try expected_labels.put("LBL3", 2);

    const expected_opcodes = [_]Opcode{
        Opcode{
            .line = 2,
            .instr = Instruction.TESTINGONLY,
            .arg1 = Argument{
                .Register8Bit = Register8.B,
            },
            .arg2 = Argument{
                .Immediate8BitUnsigned = 2,
            },
        },
        Opcode{
            .line = 3,
            .instr = Instruction.TESTINGONLY,
            .arg1 = Argument{
                .Register8Bit = Register8.C,
            },
            .arg2 = Argument{
                .Immediate8BitUnsigned = 3,
            },
        },
    };

    var labels, const opcodes = try parser(tokens, std.testing.allocator);
    defer std.testing.allocator.free(opcodes);
    defer labels.deinit();
    defer {
        var labels_it = labels.iterator();
        while (labels_it.next()) |item| {
            std.testing.allocator.free(item.key_ptr.*);
        }
    }

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

        if (opcode.arg1 != null) {
            // Expect labels only on the first argument
            switch (opcode.arg1.?) {
                .Label => |lbl| {
                    const getOrPut = try labelReferences.getOrPut(lbl);
                    if (!getOrPut.found_existing) {
                        var newlist = std.ArrayList(usize).init(allocator);
                        errdefer newlist.deinit();
                        getOrPut.value_ptr.* = newlist;
                    }
                    try getOrPut.value_ptr.append(accumulated);
                },
                else => {},
            }
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
                logError("Undefined label {s}", .{label});
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
        \\       testingonly
        \\       testingonly 0xAA 0xBBCC
        \\       testingonly lbl3
        \\ lbl2: testingonly lbl2
        \\       testingonly lbl1
        \\       testingonly 0xFF 0xEEDD
        \\       testingonly
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
        0x55, // testingonly
        0x55, 0xAA, 0xCC, 0xBB, // testingonly AA BBCC
        0x55, 0x13, 0x00, // testingonly lbl3
        0x55, 0x08, 0x00, // testingonly lbl2
        0x55, 0x00, 0x00, // testingonly lbl1
        0x55, 0xFF, 0xDD, 0xEE, // testingonly DD EEDD
        0x55, // testingonly
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
    const input = "lbl1: testingonly lbl2";

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
