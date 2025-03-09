pub const Add8BitUnsigned = packed struct {
    result: u8,
    zero: u1,
    carry: u1,
    halfcarry: u1,

    pub fn apply(op1: u8, op2: u8) Add8BitUnsigned {
        const op1_lo: u4 = @intCast(op1 & 0x0F);
        const op2_lo: u4 = @intCast(op2 & 0x0F);

        const op1_hi: u4 = @intCast((op1 & 0xF0) >> 4);
        const op2_hi: u4 = @intCast((op2 & 0xF0) >> 4);

        const res_lo: u4, const halfcarry: u1 = @addWithOverflow(op1_lo, op2_lo);
        const res_hi_tmp: u4, const carry_1: u1 = @addWithOverflow(op1_hi, halfcarry);
        const res_hi: u4, const carry_2: u1 = @addWithOverflow(res_hi_tmp, op2_hi);

        const res: u8 = (@as(u8, res_hi) << 4) | res_lo;
        const zero: u1 = @intFromBool(res == 0);
        const carry: u1 = carry_1 | carry_2;

        return Add8BitUnsigned{
            .result = res,
            .zero = zero,
            .carry = carry,
            .halfcarry = halfcarry,
        };
    }
};
