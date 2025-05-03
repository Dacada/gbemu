const std = @import("std");

// Read-only pointers do not use const pointers. This is because the read-only semantics extend only to the presence or
// absence of a write method. The emulated hardware may decide that a read is a write, and this instrumented pointer
// does not disallow this.

pub const PointerUsage = enum { ReadOnly, ReadWrite };

pub fn InstrumentedPointer(comptime usage: PointerUsage) type {
    return struct {
        const _RawType = struct {
            const Self = @This();

            ptr: *u8,

            pub inline fn read(self: *Self) u8 {
                return self.ptr.*;
            }

            pub inline fn write(self: *Self, val: u8) void {
                if (usage == .ReadOnly) @compileError("Cannot use write method of read-only instrumented pointer.");
                self.ptr.* = val;
            }
        };

        const _HookedType = struct {
            const Self = @This();

            ptr: *u8,
            ctx: *anyopaque,
            on_read: ?*const fn (*Self) u8,
            on_write: if (usage == .ReadWrite) ?*const fn (*Self, u8) void else void,

            pub fn read(self: *Self) u8 {
                if (self.on_read) |hook| {
                    return hook(self);
                } else {
                    return self.ptr.*;
                }
            }

            pub fn write(self: *Self, val: u8) void {
                if (usage == .ReadOnly) @compileError("Cannot use write method of read-only instrumented pointer.");
                if (self.on_write) |hook| {
                    hook(self, val);
                } else {
                    self.ptr.* = val;
                }
            }
        };
        const InstrumentedPointer = union(enum) {
            const Self = @This();

            raw: _RawType,
            hooked: _HookedType,

            pub fn read(self: *Self) u8 {
                return switch (self.*) {
                    .raw => |*p| p.read(),
                    .hooked => |*p| p.read(),
                };
            }

            pub fn write(self: *Self, value: u8) void {
                if (usage == .ReadOnly) @compileError("Cannot use write method of read-only instrumented pointer.");
                switch (self.*) {
                    .raw => |*p| p.write(value),
                    .hooked => |*p| p.write(value),
                }
            }
        };
    };
}

test "rw raw" {
    var value: u8 = 123;
    var ptr = InstrumentedPointer(.ReadWrite).InstrumentedPointer{
        .raw = .{
            .ptr = &value,
        },
    };

    try std.testing.expectEqual(123, ptr.read());

    ptr.write(42);

    try std.testing.expectEqual(42, ptr.read());
    try std.testing.expectEqual(42, value);
}

test "ro raw" {
    var value: u8 = 123;
    var ptr = InstrumentedPointer(.ReadOnly).InstrumentedPointer{
        .raw = .{
            .ptr = &value,
        },
    };

    try std.testing.expectEqual(123, ptr.read());

    // Uncommenting this will result in a compile error
    // ptr.write(42);
}

const MyCtx = struct {
    called_read: bool,
    called_write: bool,
    called_write_with: u8,
};

fn onread(ptr: *InstrumentedPointer(.ReadWrite)._HookedType) u8 {
    var ctx: *MyCtx = @ptrCast(ptr.ctx);
    ctx.called_read = true;
    return 0;
}

fn onread_ro(ptr: *InstrumentedPointer(.ReadOnly)._HookedType) u8 {
    var ctx: *MyCtx = @ptrCast(ptr.ctx);
    ctx.called_read = true;
    return 0;
}

fn onwrite(ptr: *InstrumentedPointer(.ReadWrite)._HookedType, val: u8) void {
    var ctx: *MyCtx = @ptrCast(ptr.ctx);
    ctx.called_write = true;
    ctx.called_write_with = val;
}

test "rw hooked" {
    var myctx = MyCtx{
        .called_read = false,
        .called_write = false,
        .called_write_with = 0,
    };

    var value: u8 = 123;
    var ptr = InstrumentedPointer(.ReadWrite).InstrumentedPointer{
        .hooked = .{
            .ctx = &myctx,
            .on_read = onread,
            .on_write = onwrite,
            .ptr = &value,
        },
    };

    try std.testing.expect(!myctx.called_read);
    try std.testing.expectEqual(0, ptr.read());
    try std.testing.expect(myctx.called_read);

    try std.testing.expect(!myctx.called_write);
    ptr.write(42);
    try std.testing.expect(myctx.called_write);
    try std.testing.expectEqual(42, myctx.called_write_with);

    try std.testing.expectEqual(123, value);
}

test "ro hooked" {
    var myctx = MyCtx{
        .called_read = false,
        .called_write = false,
        .called_write_with = 0,
    };

    var value: u8 = 123;
    var ptr = InstrumentedPointer(.ReadOnly).InstrumentedPointer{
        .hooked = .{
            .ctx = &myctx,
            .on_read = onread_ro,
            .on_write = {},
            .ptr = &value,
        },
    };

    try std.testing.expect(!myctx.called_read);
    try std.testing.expectEqual(0, ptr.read());
    try std.testing.expect(myctx.called_read);

    // Uncommenting this will result in a compile error
    //ptr.write(42);
}
