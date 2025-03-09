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
