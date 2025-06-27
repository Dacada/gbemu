pub const MemoryFlag = packed struct {
    illegal: bool = false,
    uninitialized: bool = false,

    pub fn any(self: MemoryFlag) bool {
        return self.illegal or self.uninitialized;
    }
};

pub const Memory = struct {
    ctx: *anyopaque,
    read_cb: *const fn (*anyopaque, u16) struct { ?MemoryFlag, u8 },
    write_cb: *const fn (*anyopaque, u16, u8) ?MemoryFlag,
    peek_cb: *const fn (*anyopaque, u16) u8,
    poke_cb: *const fn (*anyopaque, u16, u8) void,

    flags: MemoryFlag = .{},

    pub fn read(self: *Memory, addr: u16) u8 {
        const flags, const val = self.read_cb(self.ctx, addr);
        if (flags) |f| {
            self.flags = f;
        } else {
            self.flags = .{};
        }
        return val;
    }

    pub fn write(self: *Memory, addr: u16, val: u8) void {
        const flags = self.write_cb(self.ctx, addr, val);
        if (flags) |f| {
            self.flags = f;
        } else {
            self.flags = .{};
        }
    }

    pub fn peek(self: Memory, addr: u16) u8 {
        return self.peek_cb(self.ctx, addr);
    }

    pub fn poke(self: Memory, addr: u16, val: u8) void {
        self.poke_cb(self.ctx, addr, val);
    }

    pub fn dumpMemory(self: Memory, start: u16, buff: []u8) void {
        for (0..buff.len) |i| {
            buff[i] = self.peek(start + @as(u16, @intCast(i)));
        }
    }
};

pub fn SimpleMemory(
    readOnly: bool,
    memoryArray: []u8,
    initArray: ?[]bool,
) type {
    return struct {
        fn read_cb(_: *anyopaque, addr: u16) struct { ?MemoryFlag, u8 } {
            var flags = MemoryFlag{};
            if (initArray) |init| {
                if (!init[addr]) {
                    flags.uninitialized = true;
                }
            }

            return .{ flags, memoryArray[addr] };
        }

        fn write_cb(_: *anyopaque, addr: u16, val: u8) ?MemoryFlag {
            if (readOnly) {
                return .{
                    .illegal = true,
                };
            }

            if (initArray) |init| {
                init[addr] = true;
            }

            memoryArray[addr] = val;
            return null;
        }

        fn peek_cb(_: *anyopaque, addr: u16) u8 {
            return memoryArray[addr];
        }

        fn poke_cb(_: *anyopaque, addr: u16, val: u8) void {
            // Poke forces a write
            memoryArray[addr] = val;
        }

        pub fn memory() Memory {
            return Memory{
                .ctx = undefined,
                .read_cb = read_cb,
                .write_cb = write_cb,
                .peek_cb = peek_cb,
                .poke_cb = poke_cb,
            };
        }
    };
}

const std = @import("std");
var mem_array = [_]u8{0} ** 256;
var init_array = [_]bool{false} ** 256;

test "SimpleMemory basic read and write" {
    @memset(mem_array[0..], 0);
    @memset(init_array[0..], false);

    const MemType = SimpleMemory(false, mem_array[0..], init_array[0..]);
    var memory = MemType.memory();

    memory.write(10, 42);
    const read_val = memory.read(10);

    try std.testing.expectEqual(@as(u8, 42), read_val);
    try std.testing.expect(!memory.flags.any());
    try std.testing.expect(init_array[10]); // Should now be marked as initialized.
}

test "SimpleMemory read from uninitialized memory" {
    @memset(mem_array[0..], 0);
    @memset(init_array[0..], false);

    const MemType = SimpleMemory(false, mem_array[0..], init_array[0..]);
    var memory = MemType.memory();

    _ = memory.read(20);

    try std.testing.expect(memory.flags.uninitialized);
    try std.testing.expect(!memory.flags.illegal);
}

test "SimpleMemory write to read-only memory" {
    @memset(mem_array[0..], 0);

    const MemType = SimpleMemory(true, mem_array[0..], null);
    var memory = MemType.memory();

    memory.write(15, 99);

    try std.testing.expect(memory.flags.illegal);
    try std.testing.expectEqual(@as(u8, 0), memory.read(15)); // Should remain unchanged.
}

test "SimpleMemory peek and poke" {
    @memset(mem_array[0..], 0);

    const MemType = SimpleMemory(true, mem_array[0..], null);
    var memory = MemType.memory();

    memory.poke(50, 123);
    const peek_val = memory.peek(50);

    try std.testing.expectEqual(@as(u8, 123), peek_val);
    try std.testing.expectEqual(@as(u8, 123), memory.read(50)); // Should correctly read value.
    try std.testing.expect(!memory.flags.any());
}

test "SimpleMemory without initArray" {
    @memset(mem_array[0..], 0);

    const MemType = SimpleMemory(false, mem_array[0..], null);
    var memory = MemType.memory();

    memory.write(30, 77);
    const read_val = memory.read(30);

    try std.testing.expectEqual(@as(u8, 77), read_val);
    try std.testing.expect(!memory.flags.any());
}
