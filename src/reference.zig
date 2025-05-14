const std = @import("std");
const Mmu = @import("mmu.zig").Mmu;

pub const DelayedReferenceMmu = struct {
    addr: u16,
    mmu: *Mmu,
};

pub const DelayedReference = union(enum) {
    mmuRef: DelayedReferenceMmu,
    ptrRef: *u8,

    pub fn fromMmu(mmu: *Mmu, addr: u16) DelayedReference {
        return DelayedReference{
            .mmuRef = DelayedReferenceMmu{
                .addr = addr,
                .mmu = mmu,
            },
        };
    }

    pub fn fromPointer(ptr: *u8) DelayedReference {
        return DelayedReference{
            .ptrRef = ptr,
        };
    }

    pub fn read(self: DelayedReference) u8 {
        return switch (self) {
            .mmuRef => |mmuRef| mmuRef.mmu.read(mmuRef.addr),
            .ptrRef => |ptrRef| ptrRef.*,
        };
    }

    pub fn write(self: DelayedReference, val: u8) void {
        switch (self) {
            .mmuRef => |mmuRef| mmuRef.mmu.write(mmuRef.addr, val),
            .ptrRef => |ptrRef| ptrRef.* = val,
        }
    }
};

test "mmu reference" {
    var mmu = Mmu.init();
    mmu.zeroize();

    const ref = DelayedReference.fromMmu(&mmu, 0xA000);
    ref.write(123);
    try std.testing.expectEqual(123, ref.read());
}

test "ptr reference" {
    var val: u8 = 0;

    const ref = DelayedReference.fromPointer(&val);
    ref.write(123);
    try std.testing.expectEqual(123, val);
}
