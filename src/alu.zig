const std = @import("std");

pub const RegisterFlags = packed struct {
    z: u1,
    n: u1,
    h: u1,
    c: u1,
    rest: u4,

    pub fn all(reg: *const RegisterFlags) u8 {
        return (@as(u8, reg.z) << 7) | (@as(u8, reg.n) << 6) | (@as(u8, reg.h) << 5) | (@as(u8, reg.c) << 4) | reg.rest;
    }

    pub fn setAll(reg: *RegisterFlags, val: u8) void {
        reg.z = @intCast((val & 0b1000_0000) >> 7);
        reg.n = @intCast((val & 0b0100_0000) >> 6);
        reg.h = @intCast((val & 0b0010_0000) >> 5);
        reg.c = @intCast((val & 0b0001_0000) >> 4);
        reg.rest = @intCast(val & 0b0000_1111);
    }
};

test "register flags" {
    var reg: RegisterFlags = undefined;

    reg.setAll(0xAA);
    try std.testing.expectEqual(1, reg.z);
    try std.testing.expectEqual(0, reg.n);
    try std.testing.expectEqual(1, reg.h);
    try std.testing.expectEqual(0, reg.c);
    try std.testing.expectEqual(0xA, reg.rest);

    reg.z = 0;
    reg.n = 1;
    reg.h = 0;
    reg.c = 1;
    reg.rest = 0x5;
    try std.testing.expectEqual(0x55, reg.all());
}

pub const RegisterWithHalves = packed struct {
    hi: u8,
    lo: u8,

    pub fn all(reg: *const RegisterWithHalves) u16 {
        return (@as(u16, reg.hi) << 8) | reg.lo;
    }

    pub fn setAll(reg: *RegisterWithHalves, val: u16) void {
        reg.hi = @intCast((val & 0xFF00) >> 8);
        reg.lo = @intCast(val & 0x00FF);
    }

    pub fn inc(reg: *RegisterWithHalves) void {
        reg.lo, const carry = @addWithOverflow(reg.lo, 1);
        reg.hi, _ = @addWithOverflow(reg.hi, carry);
    }

    pub fn dec(reg: *RegisterWithHalves) void {
        reg.lo, const carry = @subWithOverflow(reg.lo, 1);
        reg.hi, _ = @subWithOverflow(reg.hi, carry);
    }
};

test "register with halves" {
    var reg: RegisterWithHalves = undefined;

    reg.setAll(0xABCD);
    try std.testing.expectEqual(0xAB, reg.hi);
    try std.testing.expectEqual(0xCD, reg.lo);

    reg.hi = 0x12;
    reg.lo = 0x34;
    try std.testing.expectEqual(0x1234, reg.all());
}

pub const AluRegister = packed struct {
    hi: u8,
    lo: RegisterFlags,

    pub fn all(reg: *const AluRegister) u16 {
        return (@as(u16, reg.hi) << 8) | reg.lo.all();
    }

    pub fn setAll(reg: *AluRegister, val: u16) void {
        reg.hi = @intCast((val & 0xFF00) >> 8);
        reg.lo.setAll(@intCast(val & 0x00FF));
    }

    fn adderU4(val1: u4, val2: u4, carry_in: u1) struct { u4, u1 } {
        const tmp, const carry_out_1 = @addWithOverflow(val1, carry_in);
        const result, const carry_out_2 = @addWithOverflow(tmp, val2);
        const carry_out = carry_out_1 | carry_out_2;
        return .{ result, carry_out };
    }

    fn addValues(self: *AluRegister, val1: u8, val2: u8, with_carry: u1) u8 {
        const val1_lo: u4 = @intCast(val1 & 0x0F);
        const val2_lo: u4 = @intCast(val2 & 0x0F);

        const val1_hi: u4 = @intCast((val1 & 0xF0) >> 4);
        const val2_hi: u4 = @intCast((val2 & 0xF0) >> 4);

        const carry_in = self.lo.c & with_carry;

        const res_lo, const halfcarry_out = AluRegister.adderU4(val1_lo, val2_lo, carry_in);
        const res_hi, const carry_out = AluRegister.adderU4(val1_hi, val2_hi, halfcarry_out);

        const result: u8 = (@as(u8, res_hi) << 4) | res_lo;
        const zero = @intFromBool(result == 0);

        self.lo.c = carry_out;
        self.lo.h = halfcarry_out;
        self.lo.z = zero;
        self.lo.n = 0;
        return result;
    }

    pub fn add(self: *AluRegister, summand: u8, with_carry: u1) void {
        self.hi = self.addValues(self.hi, summand, with_carry);
    }

    pub fn sub(self: *AluRegister, subtrahend: u8, with_carry: u1) void {
        self.lo.c = ~(with_carry & self.lo.c);
        self.hi = self.addValues(self.hi, ~subtrahend, 1);
        self.lo.n = 1;
        self.lo.c = ~self.lo.c;
        self.lo.h = ~self.lo.h;
    }

    pub fn inc(self: *AluRegister, val: u8) u8 {
        var tmp = self.*;
        tmp.hi = val;
        tmp.add(1, 0);

        self.lo.h = tmp.lo.h;
        self.lo.n = tmp.lo.n;
        self.lo.z = tmp.lo.z;

        return tmp.hi;
    }

    pub fn dec(self: *AluRegister, val: u8) u8 {
        var tmp = self.*;
        tmp.hi = val;
        tmp.sub(1, 0);

        self.lo.h = tmp.lo.h;
        self.lo.n = tmp.lo.n;
        self.lo.z = tmp.lo.z;

        return tmp.hi;
    }

    pub fn and_(self: *AluRegister, arg: u8) void {
        self.hi &= arg;
        self.lo.z = @intFromBool(self.hi == 0);
        self.lo.n = 0;
        self.lo.c = 0;
        self.lo.h = 1;
    }

    pub fn or_(self: *AluRegister, arg: u8) void {
        self.hi |= arg;
        self.lo.z = @intFromBool(self.hi == 0);
        self.lo.n = 0;
        self.lo.c = 0;
        self.lo.h = 0;
    }

    pub fn xor(self: *AluRegister, arg: u8) void {
        self.hi ^= arg;
        self.lo.z = @intFromBool(self.hi == 0);
        self.lo.n = 0;
        self.lo.c = 0;
        self.lo.h = 0;
    }

    pub fn ccf(self: *AluRegister) void {
        self.lo.c = ~self.lo.c;
        self.lo.n = 0;
        self.lo.h = 0;
    }

    pub fn scf(self: *AluRegister) void {
        self.lo.c = 1;
        self.lo.n = 0;
        self.lo.h = 0;
    }

    pub fn cpl(self: *AluRegister) void {
        self.hi = ~self.hi;
        self.lo.n = 1;
        self.lo.h = 1;
    }

    pub fn daa(self: *AluRegister) void {
        var adj: u8 = 0;
        var carry: u1 = 0;
        if ((self.lo.n == 0 and self.hi & 0x0F > 0x09) or self.lo.h == 1) {
            adj |= 0x06;
        }
        if ((self.lo.n == 0 and self.hi > 0x99) or self.lo.c == 1) {
            adj |= 0x60;
            carry = 1;
        }

        self.hi, _ = if (self.lo.n == 0)
            @addWithOverflow(self.hi, adj)
        else
            @subWithOverflow(self.hi, adj);

        self.lo.z = @intFromBool(self.hi == 0);
        self.lo.c = carry;
        self.lo.h = 0;
    }

    pub fn addReturn(self: *AluRegister, op1: u8, op2: u8, with_carry: u1, z: u1) u8 {
        const result = self.addValues(op1, op2, with_carry);
        self.lo.z = z;
        return result;
    }

    pub fn addAdj(self: *const AluRegister, op: u8, prev: u8) u8 {
        const adj: u8 = if ((prev & 0b1000_0000) >> 7 == 1) 0xFF else 0x00;
        const tmp, _ = @addWithOverflow(op, adj);
        const result, _ = @addWithOverflow(tmp, self.lo.c);
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

        self.lo.c = carry;
        self.lo.z = @intFromBool(result == 0);
        self.lo.h = 0;
        self.lo.n = 0;

        return result;
    }

    pub fn rlc(self: *AluRegister, val: u8) u8 {
        return self.shift(val, @intCast((val & 0x80) >> 7), false);
    }

    pub fn rrc(self: *AluRegister, val: u8) u8 {
        return self.shift(val, @intCast(val & 0x01), true);
    }

    pub fn rl(self: *AluRegister, val: u8) u8 {
        return self.shift(val, self.lo.c, false);
    }

    pub fn rr(self: *AluRegister, val: u8) u8 {
        return self.shift(val, self.lo.c, true);
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
        self.lo.z = @intFromBool(val == 0);
        self.lo.c = 0;
        self.lo.h = 0;
        self.lo.n = 0;
        return ((val & 0x0F) << 4) | ((val & 0xF0) >> 4);
    }

    fn mask(idx: u3) u8 {
        return @as(u8, 1) << idx;
    }

    pub fn bit(self: *AluRegister, val: u8, idx: u3) void {
        self.lo.z = @intCast((val & AluRegister.mask(idx)) >> idx);
        self.lo.n = 0;
        self.lo.h = 1;
    }

    pub fn res(val: u8, idx: u3) u8 {
        return val & ~AluRegister.mask(idx);
    }

    pub fn set(val: u8, idx: u3) u8 {
        return val | AluRegister.mask(idx);
    }

    pub fn cond(self: *const AluRegister, which: u2) bool {
        return switch (which) {
            0b00 => self.lo.z == 0,
            0b01 => self.lo.z == 1,
            0b10 => self.lo.c == 0,
            0b11 => self.lo.c == 1,
        };
    }

    pub fn addInterpretSignedNoFlags(op: u16, e: u8) u16 {
        const z_sign: u1 = @intCast((e & 0b1000_0000) >> 7);
        const result, const carry = @addWithOverflow(@as(u8, @intCast(op & 0x00FF)), e);
        const lo = result;
        const adj: u8 = if (carry == 1 and z_sign == 0)
            0x01
        else if (carry == 0 and z_sign == 1)
            0xFF
        else
            0x00;
        const hi, _ = @addWithOverflow(@as(u8, @intCast((op & 0xFF00) >> 8)), adj);
        return (@as(u16, hi) << 8) | lo;
    }
};

test "0b00000001 + 0b00000001" {
    const use_carry: u1 = 0;
    const operand: u8 = 0b00000001;
    var reg = AluRegister{
        .hi = 0b00000001,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00000010,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
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
        .hi = 0b00000001,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00000011,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
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
        .hi = 0b11111111,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00000000,
        .lo = RegisterFlags{
            .c = 1,
            .h = 1,
            .n = 0,
            .z = 1,
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
        .hi = 0b10000000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00000000,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 1,
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
        .hi = 0b11111111,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b11111111,
        .lo = RegisterFlags{
            .c = 1,
            .h = 1,
            .n = 0,
            .z = 0,
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
        .hi = 0b00001111,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00010000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 0,
            .z = 0,
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
        .hi = 0b00000010,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00000001,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 1,
            .z = 0,
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
        .hi = 0b00000001,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00000000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 1,
            .z = 1,
            .rest = 0,
        },
    };

    reg.sub(operand, use_carry);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b11110000 & 0b11001100" {
    const operand: u8 = 0b11001100;
    var reg = AluRegister{
        .hi = 0b11110000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b11000000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.and_(operand);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b11110000 | 0b11001100" {
    const operand: u8 = 0b11001100;
    var reg = AluRegister{
        .hi = 0b11110000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b11111100,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.or_(operand);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "0b11110000 ^ 0b11001100" {
    const operand: u8 = 0b11001100;
    var reg = AluRegister{
        .hi = 0b11110000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00111100,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.xor(operand);

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "CCF (Complement Carry Flag)" {
    var reg = AluRegister{
        .hi = 0b00000000,
        .lo = RegisterFlags{
            .c = 1,
            .h = 1,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00000000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.ccf();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "SCF (Set Carry Flag)" {
    var reg = AluRegister{
        .hi = 0b00000000,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b00000000,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.scf();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "CPL (Complement Accumulator)" {
    var reg = AluRegister{
        .hi = 0b10101010,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0b01010101,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };

    reg.cpl();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x00 n=0 h=0 c=0" {
    var reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 1,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x09, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x09,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x09,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x0A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x0A,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x10,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x19, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x19,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x19,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x1A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x1A,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x20,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x29, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x29,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x29,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x2A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x2A,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x30,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x39, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x39,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x39,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x3A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x3A,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x40,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x40, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x40,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x40,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x45, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x45,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x45,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x99, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x99,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x99,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x9A, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0x9A,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 1,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0xA0, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0xA0,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 1,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0xA5, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0xA5,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x05,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0xFF, n=0, h=0, c=0" {
    var reg = AluRegister{
        .hi = 0xFF,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x65,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x05, n=0, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x05,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x0B,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x15, n=0, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x15,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x1B,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x25, n=0, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x25,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x2B,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x35, n=0, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x35,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x3B,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x3F, n=0, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x3F,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x45,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x9A, n=0, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x9A,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 1,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x2F, n=1, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x2F,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x29,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x42, n=1, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x42,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x3C,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x9A, n=1, h=1, c=0" {
    var reg = AluRegister{
        .hi = 0x9A,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x94,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0x99, n=1, h=1, c=1" {
    var reg = AluRegister{
        .hi = 0x99,
        .lo = RegisterFlags{
            .c = 1,
            .h = 1,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x33,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "DAA a=0xFF, n=1, h=1, c=1" {
    var reg = AluRegister{
        .hi = 0xFF,
        .lo = RegisterFlags{
            .c = 1,
            .h = 1,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x99,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };

    reg.daa();

    try std.testing.expectEqual(expected_reg.all(), reg.all());
}

test "add return" {
    var reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 1,
            .z = 0,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 1,
            .h = 1,
            .n = 0,
            .z = 1,
            .rest = 0,
        },
    };
    const op1: u8 = 0xFF;
    const op2: u8 = 0x01;
    const expected: u8 = 0x00;

    const actual = reg.addReturn(op1, op2, 1, 1);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "add_adj" {
    var reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
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

    const actual_lo = reg.addValues(op1_lo, unsigned_op2, 0);
    const actual_hi = reg.addAdj(op1_hi, unsigned_op2);
    const actual = (@as(u16, actual_hi) << 8) | actual_lo;

    try std.testing.expectEqual(expected_result, actual);
    try std.testing.expectEqual(expected_h, reg.lo.h);
    try std.testing.expectEqual(expected_c, reg.lo.c);
}

test "rlc" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{
            .c = 0,
            .h = 1,
            .z = 1,
            .n = 1,
            .rest = 0,
        },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .z = 0,
            .n = 0,
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
        .hi = 0,
        .lo = RegisterFlags{ .c = 0, .h = 1, .z = 1, .n = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 0, .n = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b10000000;

    const actual = reg.rrc(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rl with carry = 0" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 0, .h = 1, .z = 0, .n = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 1, .n = 0, .rest = 0 },
    };
    const input = 0b10000000;
    const expected = 0b00000000;

    const actual = reg.rl(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rl with carry = 1" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 0, .n = 0, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 0, .n = 0, .rest = 0 },
    };
    const input = 0b10000000;
    const expected = 0b00000001;

    const actual = reg.rl(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rr with carry = 0" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 0, .h = 1, .z = 0, .n = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 1, .n = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b00000000;

    const actual = reg.rr(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "rr with carry = 1" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 0, .n = 0, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 0, .n = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b10000000;

    const actual = reg.rr(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "sla" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 0, .h = 1, .z = 1, .n = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 0, .n = 0, .rest = 0 },
    };
    const input = 0b10000001;
    const expected = 0b00000010;

    const actual = reg.sla(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "sra" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 0, .h = 1, .z = 0, .n = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 1, .n = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b00000000;

    const actual = reg.sra(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "srl" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 0, .h = 1, .z = 0, .n = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 1, .n = 0, .rest = 0 },
    };
    const input = 0b00000001;
    const expected = 0b00000000;

    const actual = reg.srl(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "swap" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 1, .z = 1, .n = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 0, .h = 0, .z = 0, .n = 0, .rest = 0 },
    };
    const input = 0xAB;
    const expected = 0xBA;

    const actual = reg.swap(input);

    try std.testing.expectEqual(expected, actual);
    try std.testing.expectEqualDeep(expected_reg, reg);
}

test "bit" {
    var reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 0, .z = 1, .n = 1, .rest = 0 },
    };
    const expected_reg = AluRegister{
        .hi = 0,
        .lo = RegisterFlags{ .c = 1, .h = 1, .z = 0, .n = 0, .rest = 0 },
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

test "cond Z=0 C=0" {
    var reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    try std.testing.expect(reg.cond(0b00));
    try std.testing.expect(!reg.cond(0b01));
    try std.testing.expect(reg.cond(0b10));
    try std.testing.expect(!reg.cond(0b11));
}

test "cond Z=0 C=1" {
    var reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 0,
            .rest = 0,
        },
    };

    try std.testing.expect(reg.cond(0b00));
    try std.testing.expect(!reg.cond(0b01));
    try std.testing.expect(!reg.cond(0b10));
    try std.testing.expect(reg.cond(0b11));
}

test "cond Z=1 C=0" {
    var reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 0,
            .h = 0,
            .n = 0,
            .z = 1,
            .rest = 0,
        },
    };

    try std.testing.expect(!reg.cond(0b00));
    try std.testing.expect(reg.cond(0b01));
    try std.testing.expect(reg.cond(0b10));
    try std.testing.expect(!reg.cond(0b11));
}

test "cond Z=1 C=1" {
    var reg = AluRegister{
        .hi = 0x00,
        .lo = RegisterFlags{
            .c = 1,
            .h = 0,
            .n = 0,
            .z = 1,
            .rest = 0,
        },
    };

    try std.testing.expect(!reg.cond(0b00));
    try std.testing.expect(reg.cond(0b01));
    try std.testing.expect(!reg.cond(0b10));
    try std.testing.expect(reg.cond(0b11));
}

test "add interpret signed no flags" {
    try std.testing.expectEqual(0xFFFF, AluRegister.addInterpretSignedNoFlags(0x0000, 0xFF));
    try std.testing.expectEqual(0x0001, AluRegister.addInterpretSignedNoFlags(0x0000, 0x01));
    try std.testing.expectEqual(0x0000, AluRegister.addInterpretSignedNoFlags(0x0000, 0x00));
    try std.testing.expectEqual(0xFFFE, AluRegister.addInterpretSignedNoFlags(0xFFFF, 0xFF));
    try std.testing.expectEqual(0x0000, AluRegister.addInterpretSignedNoFlags(0xFFFF, 0x01));
    try std.testing.expectEqual(0xFFFF, AluRegister.addInterpretSignedNoFlags(0xFFFF, 0x00));
}
