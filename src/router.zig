const MemoryFlag = @import("memoryFlag.zig").MemoryFlag;

pub fn Range(Target: type) type {
    return struct { start: u16, end: u16, target: Target };
}

pub fn TargetField(Target: type) type {
    return struct { target: Target, field: ?[]const u8, namespace: type };
}

pub fn Router(
    Target: type,
    ranges: []const Range(Target),
    nomatch: struct { addr: u16, target: Target },
    targetFields: []const TargetField(Target),
) type {
    return struct {
        const Operation = enum {
            read,
            write,
            peek,
            poke,
        };

        inline fn decode(addr: u16) struct { u16, Target } {
            inline for (ranges) |range| {
                if (addr >= range.start and addr <= range.end) {
                    return .{ addr - range.start, range.target };
                }
            }
            return .{ nomatch.addr, nomatch.target };
        }

        pub inline fn dispatch(parent: anytype, addr: u16, comptime operation: Operation, value: u8) struct { MemoryFlag, u8 } {
            const resolved_address, const target = decode(addr);
            inline for (targetFields) |targetField| {
                if (target == targetField.target) {
                    return make_call(
                        if (targetField.field) |field| @field(parent, field) else parent,
                        targetField.namespace,
                        operation,
                        resolved_address,
                        value,
                    );
                }
            }
            unreachable;
        }

        inline fn make_call(instance: anytype, comptime namespace: type, comptime operation: Operation, addr: u16, value: u8) struct { MemoryFlag, u8 } {
            const opname = @tagName(operation);
            switch (operation) {
                .peek => return .{ undefined, @field(namespace, opname)(instance, addr) },
                .poke => {
                    @field(namespace, opname)(instance, addr, value);
                    return undefined;
                },
                .read => return @field(namespace, opname)(instance, addr),
                .write => return .{ @field(namespace, opname)(instance, addr, value), undefined },
            }
        }
    };
}
