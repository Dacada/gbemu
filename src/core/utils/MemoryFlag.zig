const This = @This();

illegal: bool = false,
uninitialized: bool = false,

pub fn any(self: This) bool {
    return self.illegal or self.uninitialized;
}
