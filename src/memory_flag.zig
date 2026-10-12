pub const MemoryFlag = struct {
    illegal: bool = false,
    uninitialized: bool = false,

    pub fn any(self: MemoryFlag) bool {
        return self.illegal or self.uninitialized;
    }
};
