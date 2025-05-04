const Mmu = @import("mmu.zig").Mmu;

pub const DelayedReferenceMmu = struct {
    addr: u16,
    mmu: *Mmu,

    pub fn read(self: *DelayedReferenceMmu) u8 {
        return self.mmu.read(self.addr);
    }

    pub fn write(self: *DelayedReferenceMmu, val: u8) void {
        self.mmu.write(self.addr, val);
    }
};

pub const DelayedReference = union {
    mmuRef: DelayedReferenceMmu,

    pub fn fromMmu(mmu: *Mmu, addr: u16) DelayedReference {
        return DelayedReference{
            .mmuRef = DelayedReferenceMmu{
                .addr = addr,
                .mmu = mmu,
            },
        };
    }

    pub fn read(self: *DelayedReference) u8 {
        return switch (self.*) {
            .mmuRef => |*x| x.read(),
        };
    }

    pub fn write(self: *DelayedReference, val: u8) u8 {
        switch (self.*) {
            .mmuRef => |*x| x.write(val),
        }
    }
};
