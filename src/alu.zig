const std = @import("std");

pub const AluOp8Bit = packed struct {
    result: u8,
    zero: u1,
    carry: u1,
    halfcarry: u1,
    subtraction: u1,

    pub fn add(op1: u8, op2: u8, c: u1) AluOp8Bit {
        const op1_lo: u4 = @intCast(op1 & 0x0F);
        const op2_lo: u4 = @intCast(op2 & 0x0F);

        const op1_hi: u4 = @intCast((op1 & 0xF0) >> 4);
        const op2_hi: u4 = @intCast((op2 & 0xF0) >> 4);

        const res_lo_tmp: u4, const halfcarry_1: u1 = @addWithOverflow(op1_lo, c);
        const res_lo: u4, const halfcarry_2: u1 = @addWithOverflow(res_lo_tmp, op2_lo);
        const halfcarry = halfcarry_1 | halfcarry_2;

        const res_hi_tmp: u4, const carry_1: u1 = @addWithOverflow(op1_hi, halfcarry);
        const res_hi: u4, const carry_2: u1 = @addWithOverflow(res_hi_tmp, op2_hi);

        const res: u8 = (@as(u8, res_hi) << 4) | res_lo;
        const zero: u1 = @intFromBool(res == 0);
        const carry: u1 = carry_1 | carry_2;

        return AluOp8Bit{
            .result = res,
            .zero = zero,
            .carry = carry,
            .halfcarry = halfcarry,
            .subtraction = 0,
        };
    }

    pub fn sub(op1: u8, op2: u8, c: u1) AluOp8Bit {
        var res = AluOp8Bit.add(op1, ~op2, ~c);
        res.subtraction = 1;
        res.carry = ~res.carry;
        res.halfcarry = ~res.halfcarry;
        return res;
    }

    pub fn and_(op1: u8, op2: u8) AluOp8Bit {
        const res = op1 & op2;
        return AluOp8Bit{
            .result = res,
            .zero = @intFromBool(res == 0),
            .subtraction = 0,
            .halfcarry = 1,
            .carry = 0,
        };
    }

    pub fn or_(op1: u8, op2: u8) AluOp8Bit {
        const res = op1 | op2;
        return AluOp8Bit{
            .result = res,
            .zero = @intFromBool(res == 0),
            .subtraction = 0,
            .halfcarry = 0,
            .carry = 0,
        };
    }

    pub fn xor_(op1: u8, op2: u8) AluOp8Bit {
        const res = op1 ^ op2;
        return AluOp8Bit{
            .result = res,
            .zero = @intFromBool(res == 0),
            .subtraction = 0,
            .halfcarry = 0,
            .carry = 0,
        };
    }

    pub fn cpl(op: u8) AluOp8Bit {
        const res = ~op;
        return AluOp8Bit{
            .result = res,
            .zero = undefined,
            .carry = undefined,
            .subtraction = 1,
            .halfcarry = 1,
        };
    }

    pub fn daa(op: u8, c: u1, h: u1, n: u1) AluOp8Bit {
        var adj: u8 = 0;
        var carry: u1 = 0;
        if ((n == 0 and op & 0x0F > 0x09) or h == 1) {
            adj |= 0x06;
        }
        if ((n == 0 and op > 0x99) or c == 1) {
            adj |= 0x60;
            carry = 1;
        }

        const res, _ = if (n == 0)
            @addWithOverflow(op, adj)
        else
            @subWithOverflow(op, adj);

        return AluOp8Bit{
            .result = res,
            .zero = @intFromBool(res == 0),
            .carry = carry,
            .halfcarry = 0,
            .subtraction = n,
        };
    }
};

test "0b00000001 + 0b00000001" {
    const res = AluOp8Bit.add(0b00000001, 0b00000001, 0);
    try std.testing.expectEqual(0b00000010, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b00000001 + 0b00000001 + carry-in 1" {
    const res = AluOp8Bit.add(0b00000001, 0b00000001, 1);
    try std.testing.expectEqual(0b00000011, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b11111111 + 0b00000001 (Zero flag case)" {
    const res = AluOp8Bit.add(0b11111111, 0b00000001, 0);
    try std.testing.expectEqual(0b00000000, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b10000000 + 0b10000000 (Carry-out from MSB)" {
    const res = AluOp8Bit.add(0b10000000, 0b10000000, 0);
    try std.testing.expectEqual(0b00000000, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b11111111 + 0b11111111 + carry-in 1 (Full overflow)" {
    const res = AluOp8Bit.add(0b11111111, 0b11111111, 1);
    try std.testing.expectEqual(0b11111111, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b00001111 + 0b00000001 (Half-carry set)" {
    const res = AluOp8Bit.add(0b00001111, 0b00000001, 0);
    try std.testing.expectEqual(0b00010000, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b11110000 + 0b00001111 (No half-carry)" {
    const res = AluOp8Bit.add(0b11110000, 0b00001111, 0);
    try std.testing.expectEqual(0b11111111, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b10101010 + 0b01010101 (Alternating bits)" {
    const res = AluOp8Bit.add(0b10101010, 0b01010101, 0);
    try std.testing.expectEqual(0b11111111, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b11111111 + 0b00000000 + carry-in 1 (Carry-in causes carry-out)" {
    const res = AluOp8Bit.add(0b11111111, 0b00000000, 1);
    try std.testing.expectEqual(0b00000000, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b00000010 - 0b00000001" {
    const res = AluOp8Bit.sub(0b00000010, 0b00000001, 0);
    try std.testing.expectEqual(0b00000001, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b00000001 - 0b00000001" {
    const res = AluOp8Bit.sub(0b00000001, 0b00000001, 0);
    try std.testing.expectEqual(0b00000000, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b00000000 - 0b00000001 (underflow)" {
    const res = AluOp8Bit.sub(0b00000000, 0b00000001, 0);
    try std.testing.expectEqual(0b11111111, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b00010000 - 0b00000001 (half carry borrow)" {
    const res = AluOp8Bit.sub(0b00010000, 0b00000001, 0);
    try std.testing.expectEqual(0b00001111, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b00010000 - 0b00001000 (half carry triggered)" {
    const res = AluOp8Bit.sub(0b00010000, 0b00001000, 0);
    try std.testing.expectEqual(0b00001000, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b11111111 - 0b00000001 (no carry, decrement by 1)" {
    const res = AluOp8Bit.sub(0b11111111, 0b00000001, 0);
    try std.testing.expectEqual(0b11111110, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b00000000 - 0b00000000 (zero result, no carry)" {
    const res = AluOp8Bit.sub(0b00000000, 0b00000000, 0);
    try std.testing.expectEqual(0b00000000, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b00000001 - 0b00000001 with carry in" {
    const res = AluOp8Bit.sub(0b00000001, 0b00000001, 1);
    try std.testing.expectEqual(0b11111111, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b10000000 - 0b00000001 (no carry, regular subtraction)" {
    const res = AluOp8Bit.sub(0b10000000, 0b00000001, 0);
    try std.testing.expectEqual(0b01111111, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b10000000 - 0b10000000 (zero result, no carry)" {
    const res = AluOp8Bit.sub(0b10000000, 0b10000000, 0);
    try std.testing.expectEqual(0b00000000, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "0b11110000 & 0b11001100" {
    const res = AluOp8Bit.and_(0b11110000, 0b11001100);
    try std.testing.expectEqual(0b11000000, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(1, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b11110000 | 0b11001100" {
    const res = AluOp8Bit.or_(0b11110000, 0b11001100);
    try std.testing.expectEqual(0b11111100, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "0b11110000 ^ 0b11001100" {
    const res = AluOp8Bit.xor_(0b11110000, 0b11001100);
    try std.testing.expectEqual(0b00111100, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x00 n=0 h=0 c=0" {
    const res = AluOp8Bit.daa(0x00, 0, 0, 0);
    try std.testing.expectEqual(0x00, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x09, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x09, 0, 0, 0);
    try std.testing.expectEqual(0x09, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x0A, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x0A, 0, 0, 0);
    try std.testing.expectEqual(0x10, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x19, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x19, 0, 0, 0);
    try std.testing.expectEqual(0x19, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x1A, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x1A, 0, 0, 0);
    try std.testing.expectEqual(0x20, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x29, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x29, 0, 0, 0);
    try std.testing.expectEqual(0x29, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x2A, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x2A, 0, 0, 0);
    try std.testing.expectEqual(0x30, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x39, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x39, 0, 0, 0);
    try std.testing.expectEqual(0x39, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x3A, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x3A, 0, 0, 0);
    try std.testing.expectEqual(0x40, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x40, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x40, 0, 0, 0);
    try std.testing.expectEqual(0x40, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x45, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x45, 0, 0, 0);
    try std.testing.expectEqual(0x45, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x99, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x99, 0, 0, 0);
    try std.testing.expectEqual(0x99, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x9A, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0x9A, 0, 0, 0);
    try std.testing.expectEqual(0x00, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0xA0, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0xA0, 0, 0, 0);
    try std.testing.expectEqual(0x00, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0xA5, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0xA5, 0, 0, 0);
    try std.testing.expectEqual(0x05, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0xFF, n=0, h=0, c=0" {
    const res = AluOp8Bit.daa(0xFF, 0, 0, 0);
    try std.testing.expectEqual(0x65, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x05, n=0, h=1, c=0" {
    const res = AluOp8Bit.daa(0x05, 0, 1, 0);
    try std.testing.expectEqual(0x0B, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x15, n=0, h=1, c=0" {
    const res = AluOp8Bit.daa(0x15, 0, 1, 0);
    try std.testing.expectEqual(0x1B, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x25, n=0, h=1, c=0" {
    const res = AluOp8Bit.daa(0x25, 0, 1, 0);
    try std.testing.expectEqual(0x2B, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x35, n=0, h=1, c=0" {
    const res = AluOp8Bit.daa(0x35, 0, 1, 0);
    try std.testing.expectEqual(0x3B, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x3F, n=0, h=1, c=0" {
    const res = AluOp8Bit.daa(0x3F, 0, 1, 0);
    try std.testing.expectEqual(0x45, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x9A, n=0, h=1, c=0" {
    const res = AluOp8Bit.daa(0x9A, 0, 1, 0);
    try std.testing.expectEqual(0x00, res.result);
    try std.testing.expectEqual(1, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(0, res.subtraction);
}

test "DAA a=0x2F, n=1, h=1, c=0" {
    const res = AluOp8Bit.daa(0x2F, 0, 1, 1);
    try std.testing.expectEqual(0x29, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "DAA a=0x42, n=1, h=1, c=0" {
    const res = AluOp8Bit.daa(0x42, 0, 1, 1);
    try std.testing.expectEqual(0x3C, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "DAA a=0x9A, n=1, h=1, c=0" {
    const res = AluOp8Bit.daa(0x9A, 0, 1, 1);
    try std.testing.expectEqual(0x94, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(0, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "DAA a=0x99, n=1, h=1, c=1" {
    const res = AluOp8Bit.daa(0x99, 1, 1, 1);
    try std.testing.expectEqual(0x33, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}

test "DAA a=0xFF, n=1, h=1, c=1" {
    const res = AluOp8Bit.daa(0xFF, 1, 1, 1);
    try std.testing.expectEqual(0x99, res.result);
    try std.testing.expectEqual(0, res.zero);
    try std.testing.expectEqual(1, res.carry);
    try std.testing.expectEqual(0, res.halfcarry);
    try std.testing.expectEqual(1, res.subtraction);
}
