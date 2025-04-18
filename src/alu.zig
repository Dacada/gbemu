const std = @import("std");

pub const RegisterFlags = packed struct {
    Z: u1,
    N: u1,
    H: u1,
    C: u1,
    rest: u4,

    pub fn all(reg: *const RegisterFlags) u8 {
        return (@as(u8, reg.Z) << 7) | (@as(u8, reg.N) << 6) | (@as(u8, reg.H) << 5) | (@as(u8, reg.C) << 4) | reg.rest;
    }

    pub fn setAll(reg: *RegisterFlags, val: u8) void {
        reg.Z = @intCast((val & 0b1000_0000) >> 7);
        reg.N = @intCast((val & 0b0100_0000) >> 6);
        reg.H = @intCast((val & 0b0010_0000) >> 5);
        reg.C = @intCast((val & 0b0001_0000) >> 4);
        reg.rest = @intCast(val & 0b0000_1111);
    }
};

test "register flags" {
    var reg: RegisterFlags = undefined;

    reg.setAll(0xAA);
    try std.testing.expectEqual(1, reg.Z);
    try std.testing.expectEqual(0, reg.N);
    try std.testing.expectEqual(1, reg.H);
    try std.testing.expectEqual(0, reg.C);
    try std.testing.expectEqual(0xA, reg.rest);

    reg.Z = 0;
    reg.N = 1;
    reg.H = 0;
    reg.C = 1;
    reg.rest = 0x5;
    try std.testing.expectEqual(0x55, reg.all());
}

pub const RegisterWithHalves = packed struct {
    Hi: u8,
    Lo: u8,

    pub fn all(reg: *const RegisterWithHalves) u16 {
        return (@as(u16, reg.Hi) << 8) | reg.Lo;
    }

    pub fn setAll(reg: *RegisterWithHalves, val: u16) void {
        reg.Hi = @intCast((val & 0xFF00) >> 8);
        reg.Lo = @intCast(val & 0x00FF);
    }

    pub fn inc(reg: *RegisterWithHalves) void {
        reg.Lo, const carry = @addWithOverflow(reg.Lo, 1);
        reg.Hi, _ = @addWithOverflow(reg.Hi, carry);
    }

    pub fn dec(reg: *RegisterWithHalves) void {
        reg.Lo, const carry = @subWithOverflow(reg.Lo, 1);
        reg.Hi, _ = @subWithOverflow(reg.Hi, carry);
    }
};

test "register with halves" {
    var reg: RegisterWithHalves = undefined;

    reg.setAll(0xABCD);
    try std.testing.expectEqual(0xAB, reg.Hi);
    try std.testing.expectEqual(0xCD, reg.Lo);

    reg.Hi = 0x12;
    reg.Lo = 0x34;
    try std.testing.expectEqual(0x1234, reg.all());
}

pub const AluRegister = packed struct {
    Hi: u8,
    Lo: RegisterFlags,

    pub fn all(reg: *const AluRegister) u16 {
        return (@as(u16, reg.Hi) << 8) | reg.Lo.all();
    }

    pub fn setAll(reg: *AluRegister, val: u16) void {
        reg.Hi = @intCast((val & 0xFF00) >> 8);
        reg.Lo.setAll(@intCast(val & 0x00FF));
    }

    fn adder_u4(val1: u4, val2: u4, carry_in: u1) struct { u4, u1 } {
        const tmp, const carry_out_1 = @addWithOverflow(val1, carry_in);
        const result, const carry_out_2 = @addWithOverflow(tmp, val2);
        const carry_out = carry_out_1 | carry_out_2;
        return .{ result, carry_out };
    }

    fn add_values(self: *AluRegister, val1: u8, val2: u8, with_carry: u1) u8 {
        const val1_lo: u4 = @intCast(val1 & 0x0F);
        const val2_lo: u4 = @intCast(val2 & 0x0F);

        const val1_hi: u4 = @intCast((val1 & 0xF0) >> 4);
        const val2_hi: u4 = @intCast((val2 & 0xF0) >> 4);

        const carry_in = self.Lo.C & with_carry;

        const res_lo, const halfcarry_out = AluRegister.adder_u4(val1_lo, val2_lo, carry_in);
        const res_hi, const carry_out = AluRegister.adder_u4(val1_hi, val2_hi, halfcarry_out);

        const result: u8 = (@as(u8, res_hi) << 4) | res_lo;
        const zero = @intFromBool(result == 0);

        self.Lo.C = carry_out;
        self.Lo.H = halfcarry_out;
        self.Lo.Z = zero;
        self.Lo.N = 0;
        return result;
    }

    pub fn add(self: *AluRegister, summand: u8, with_carry: u1) void {
        self.Hi = self.add_values(self.Hi, summand, with_carry);
    }

    pub fn sub(self: *AluRegister, subtrahend: u8, with_carry: u1) void {
        self.Lo.C = ~(with_carry & self.Lo.C);
        self.Hi = self.add_values(self.Hi, ~subtrahend, 1);
        self.Lo.N = 1;
        self.Lo.C = ~self.Lo.C;
        self.Lo.H = ~self.Lo.H;
    }

    pub fn inc(self: *AluRegister, val: u8) u8 {
        var tmp = self.*;
        tmp.Hi = val;
        tmp.add(1, 0);

        self.Lo.H = tmp.Lo.H;
        self.Lo.N = tmp.Lo.N;
        self.Lo.Z = tmp.Lo.Z;

        return tmp.Hi;
    }

    pub fn dec(self: *AluRegister, val: u8) u8 {
        var tmp = self.*;
        tmp.Hi = val;
        tmp.sub(1, 0);

        self.Lo.H = tmp.Lo.H;
        self.Lo.N = tmp.Lo.N;
        self.Lo.Z = tmp.Lo.Z;

        return tmp.Hi;
    }

    pub fn and_(self: *AluRegister, arg: u8) void {
        self.Hi &= arg;
        self.Lo.Z = @intFromBool(self.Hi == 0);
        self.Lo.N = 0;
        self.Lo.C = 0;
        self.Lo.H = 1;
    }

    pub fn or_(self: *AluRegister, arg: u8) void {
        self.Hi |= arg;
        self.Lo.Z = @intFromBool(self.Hi == 0);
        self.Lo.N = 0;
        self.Lo.C = 0;
        self.Lo.H = 0;
    }

    pub fn xor(self: *AluRegister, arg: u8) void {
        self.Hi ^= arg;
        self.Lo.Z = @intFromBool(self.Hi == 0);
        self.Lo.N = 0;
        self.Lo.C = 0;
        self.Lo.H = 0;
    }

    pub fn ccf(self: *AluRegister) void {
        self.Lo.C = ~self.Lo.C;
        self.Lo.N = 0;
        self.Lo.H = 0;
    }

    pub fn scf(self: *AluRegister) void {
        self.Lo.C = 1;
        self.Lo.N = 0;
        self.Lo.H = 0;
    }

    pub fn cpl(self: *AluRegister) void {
        self.Hi = ~self.Hi;
        self.Lo.N = 1;
        self.Lo.H = 1;
    }

    pub fn daa(self: *AluRegister) void {
        var adj: u8 = 0;
        var carry: u1 = 0;
        if ((self.Lo.N == 0 and self.Hi & 0x0F > 0x09) or self.Lo.H == 1) {
            adj |= 0x06;
        }
        if ((self.Lo.N == 0 and self.Hi > 0x99) or self.Lo.C == 1) {
            adj |= 0x60;
            carry = 1;
        }

        self.Hi, _ = if (self.Lo.N == 0)
            @addWithOverflow(self.Hi, adj)
        else
            @subWithOverflow(self.Hi, adj);

        self.Lo.Z = @intFromBool(self.Hi == 0);
        self.Lo.C = carry;
        self.Lo.H = 0;
    }

    pub fn add_return(self: *AluRegister, op1: u8, op2: u8, with_carry: u1, z: u1) u8 {
        const result = self.add_values(op1, op2, with_carry);
        self.Lo.Z = z;
        return result;
    }

    pub fn add_adj(self: *const AluRegister, op: u8, prev: u8) u8 {
        const adj: u8 = if ((prev & 0b1000_0000) >> 7 == 1) 0xFF else 0x00;
        const tmp, _ = @addWithOverflow(op, adj);
        const result, _ = @addWithOverflow(tmp, self.Lo.C);
        return result;
    }

    fn shift(self: *AluRegister, val: u8, shift_in: u1, comptime right: bool) u8 {
        const carry: u1 = if (right)
            @intCast(val & 0x01)
        else
            @intCast((val & 0x80) >> 7);

        const result = if (right)
            (val >> 1) | (@as(u8, shift_in) << 7)
        else
            (val << 1) | shift_in;

        self.Lo.C = carry;
        self.Lo.Z = @intFromBool(result == 0);
        self.Lo.H = 0;
        self.Lo.N = 0;

        return result;
    }

    pub fn rlc(self: *AluRegister, val: u8) u8 {
        return self.shift(val, @intCast((val & 0x80) >> 7), false);
    }

    pub fn rrc(self: *AluRegister, val: u8) u8 {
        return self.shift(val, @intCast(val & 0x01), true);
    }

    pub fn rl(self: *AluRegister, val: u8) u8 {
        return self.shift(val, self.Lo.C, false);
    }

    pub fn rr(self: *AluRegister, val: u8) u8 {
        return self.shift(val, self.Lo.C, true);
    }

    pub fn sla(self: *AluRegister, val: u8) u8 {
        return self.shift(val, 0, false);
    }

    pub fn sra(self: *AluRegister, val: u8) u8 {
        return self.shift(val, @intCast((val & 0x80) >> 7), true);
    }

    pub fn srl(self: *AluRegister, val: u8) u8 {
        return self.shift(val, 0, true);
    }

    pub fn swap(self: *AluRegister, val: u8) u8 {
        self.Lo.Z = @intFromBool(val == 0);
        self.Lo.C = 0;
        self.Lo.H = 0;
        self.Lo.N = 0;
        return ((val & 0x0F) << 4) | ((val & 0xF0) >> 4);
    }

    fn mask(idx: u3) u8 {
        return @as(u8, 1) << idx;
    }

    pub fn bit(self: *AluRegister, val: u8, idx: u3) void {
        self.Lo.Z = @intCast((val & AluRegister.mask(idx)) >> idx);
        self.Lo.N = 0;
        self.Lo.H = 1;
    }

    pub fn res(val: u8, idx: u3) u8 {
        return val & ~AluRegister.mask(idx);
    }

    pub fn set(val: u8, idx: u3) u8 {
        return val | AluRegister.mask(idx);
    }
};

test "0b00000001 + 0b00000001" {
    const use_carry: u1 = 0;
    const operand: u8 = 0b00000001;
    var reg = AluRegister{
        .Hi = 0b00000001,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00000010,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.add(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b00000001 + 0b00000001 + carry-in 1" {
    const use_carry: u1 = 1;
    const operand: u8 = 0b00000001;
    var reg = AluRegister{
        .Hi = 0b00000001,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00000011,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.add(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b11111111 + 0b00000001 (Zero flag case)" {
    const use_carry: u1 = 0;
    const operand: u8 = 0b00000001;
    var reg = AluRegister{
        .Hi = 0b11111111,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00000000,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 1,
            .N = 0,
            .Z = 1,
            .rest = 0,
        },
    };

    reg.add(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b10000000 + 0b10000000 (Carry-out from MSB)" {
    const use_carry: u1 = 0;
    const operand: u8 = 0b10000000;
    var reg = AluRegister{
        .Hi = 0b10000000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00000000,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 1,
            .rest = 0,
        },
    };

    reg.add(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b11111111 + 0b11111111 + carry-in 1 (Full overflow)" {
    const use_carry: u1 = 1;
    const operand: u8 = 0b11111111;
    var reg = AluRegister{
        .Hi = 0b11111111,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b11111111,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.add(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b00001111 + 0b00000001 (Half-carry set)" {
    const use_carry: u1 = 0;
    const operand: u8 = 0b00000001;
    var reg = AluRegister{
        .Hi = 0b00001111,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00010000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.add(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b00000010 - 0b00000001" {
    const use_carry: u1 = 0;
    const operand: u8 = 0b00000001;
    var reg = AluRegister{
        .Hi = 0b00000010,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00000001,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.sub(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b00000001 - 0b00000001" {
    const use_carry: u1 = 0;
    const operand: u8 = 0b00000001;
    var reg = AluRegister{
        .Hi = 0b00000001,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00000000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 1,
            .Z = 1,
            .rest = 0,
        },
    };

    reg.sub(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b11110000 & 0b11001100" {
    const operand: u8 = 0b11001100;
    var reg = AluRegister{
        .Hi = 0b11110000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b11000000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.and_(operand);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b11110000 | 0b11001100" {
    const operand: u8 = 0b11001100;
    var reg = AluRegister{
        .Hi = 0b11110000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b11111100,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.or_(operand);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b11110000 ^ 0b11001100" {
    const operand: u8 = 0b11001100;
    var reg = AluRegister{
        .Hi = 0b11110000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00111100,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.xor(operand);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "CCF (Complement Carry Flag)" {
    var reg = AluRegister{
        .Hi = 0b00000000,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 1,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00000000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.ccf();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "SCF (Set Carry Flag)" {
    var reg = AluRegister{
        .Hi = 0b00000000,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b00000000,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.scf();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "CPL (Complement Accumulator)" {
    var reg = AluRegister{
        .Hi = 0b10101010,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0b01010101,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.cpl();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x00 n=0 h=0 c=0" {
    var reg = AluRegister{
        .Hi = 0x00,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x00,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 1,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x09, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x09,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x09,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x0A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x0A,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x10,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x19, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x19,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x19,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x1A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x1A,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x20,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x29, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x29,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x29,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x2A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x2A,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x30,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x39, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x39,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x39,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x3A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x3A,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x40,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x40, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x40,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x40,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x45, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x45,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x45,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x99, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x99,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x99,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x9A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0x9A,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x00,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 1,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0xA0, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0xA0,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x00,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 1,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0xA5, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0xA5,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x05,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0xFF, n=0, h=0, c=0" {
    var reg = AluRegister{
        .Hi = 0xFF,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x65,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x05, n=0, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x05,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x0B,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x15, n=0, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x15,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x1B,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x25, n=0, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x25,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x2B,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x35, n=0, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x35,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x3B,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x3F, n=0, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x3F,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x45,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x9A, n=0, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x9A,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x00,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 0,
            .Z = 1,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x2F, n=1, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x2F,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x29,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x42, n=1, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x42,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x3C,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x9A, n=1, h=1, c=0" {
    var reg = AluRegister{
        .Hi = 0x9A,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x94,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x99, n=1, h=1, c=1" {
    var reg = AluRegister{
        .Hi = 0x99,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 1,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x33,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0xFF, n=1, h=1, c=1" {
    var reg = AluRegister{
        .Hi = 0xFF,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 1,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x99,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "add return" {
    var reg = AluRegister{
        .Hi = 0x00,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 1,
            .Z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0x00,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 1,
            .N = 0,
            .Z = 1,
            .rest = 0,
        },
    };
    const op1: u8 = 0xFF;
    const op2: u8 = 0x01;
    const expected: u8 = 0x00;

    const actual = reg.add_return(op1, op2, 1, 1);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "add_adj" {
    var reg = AluRegister{
        .Hi = 0x00,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 0,
            .N = 0,
            .Z = 0,
            .rest = 0,
        },
    };

    const op1: u16 = 0x0A01;
    const op2: i8 = -10;
    const op1_hi: u8 = @intCast((op1 & 0xFF00) >> 8);
    const op1_lo: u8 = @intCast(op1 & 0xFF);
    const unsigned_op2: u8 = @bitCast(op2);
    const expected_result_signed, _ = @addWithOverflow(@as(i16, op1), op2);
    const expected_result: u16 = @intCast(expected_result_signed);
    _, const expected_h = @addWithOverflow(@as(u4, op1 & 0x000F), @as(u4, @intCast(unsigned_op2 & 0x0F)));
    _, const expected_c = @addWithOverflow(@as(u8, op1 & 0x00FF), unsigned_op2);

    const actual_lo = reg.add_values(op1_lo, unsigned_op2, 0);
    const actual_hi = reg.add_adj(op1_hi, unsigned_op2);
    const actual = (@as(u16, actual_hi) << 8) | actual_lo;

    try std.testing.expectEqual(expected_result, actual);
    try std.testing.expectEqual(expected_h, reg.Lo.H);
    try std.testing.expectEqual(expected_c, reg.Lo.C);
}

test "rlc" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{
            .C = 0,
            .H = 1,
            .Z = 1,
            .N = 1,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{
            .C = 1,
            .H = 0,
            .Z = 0,
            .N = 0,
            .rest = 0,
        },
    };
    const input = 0xAA;
    const expected = 0b01010101;

    const actual = reg.rlc(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rrc" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 0, .H = 1, .Z = 1, .N = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 0, .N = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b10000000;

    const actual = reg.rrc(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rl with carry = 0" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 0, .H = 1, .Z = 0, .N = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 1, .N = 0, .rest = 0 },
    };
    const input = 0b10000000;
    const expected = 0b00000000;

    const actual = reg.rl(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rl with carry = 1" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 0, .N = 0, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 0, .N = 0, .rest = 0 },
    };
    const input = 0b10000000;
    const expected = 0b00000001;

    const actual = reg.rl(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rr with carry = 0" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 0, .H = 1, .Z = 0, .N = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 1, .N = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b00000000;

    const actual = reg.rr(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rr with carry = 1" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 0, .N = 0, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 0, .N = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b10000000;

    const actual = reg.rr(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "sla" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 0, .H = 1, .Z = 1, .N = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 0, .N = 0, .rest = 0 },
    };
    const input = 0b10000001;
    const expected = 0b00000010;

    const actual = reg.sla(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "sra" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 0, .H = 1, .Z = 0, .N = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 1, .N = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b00000000;

    const actual = reg.sra(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "srl" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 0, .H = 1, .Z = 0, .N = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 1, .N = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b00000000;

    const actual = reg.srl(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "swap" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 1, .Z = 1, .N = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 0, .H = 0, .Z = 0, .N = 0, .rest = 0 },
    };
    const input = 0xAB;
    const expected = 0xBA;

    const actual = reg.swap(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "bit" {
    var reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 0, .Z = 1, .N = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .Hi = 0,
        .Lo = RegisterFlags{ .C = 1, .H = 1, .Z = 0, .N = 0, .rest = 0 },
    };
    const input = 0b00000000;
    const bit_index = 3;

    reg.bit(input, bit_index);

    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "res" {
    const input = 0b11111111;
    const bit_index = 3;
    const expected = 0b11110111;

    const actual = AluRegister.res(input, bit_index);

    try std.testing.expectEqual(expected, actual);
}

test "set" {
    const input = 0b00000000;
    const bit_index = 3;
    const expected = 0b00001000;

    const actual = AluRegister.set(input, bit_index);

    try std.testing.expectEqual(expected, actual);
}
