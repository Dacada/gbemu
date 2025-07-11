const std = @import("std");
const Order = std.math.Order;

pub const FixedSizeHeapError = error{
    HeapFull,
    HeapEmpty,
};

pub fn FixedSizeHeap(Element: type, Capacity: comptime_int, comp: fn (a: Element, b: Element) Order, should_panic: bool) type {
    return struct {
        const This = @This();

        elements: [Capacity]Element,
        count: usize,

        pub inline fn init() This {
            return This{
                .elements = undefined,
                .count = 0,
            };
        }

        pub fn push(self: *This, element: Element) if (should_panic) void else (FixedSizeHeapError!void) {
            if (self.count >= Capacity) {
                if (should_panic) {
                    @panic("Bug: Heap is full!");
                } else {
                    return FixedSizeHeapError.HeapFull;
                }
            }

            self.elements[self.count] = element;
            self.count += 1;
            self.sift_up();
        }

        pub fn pop(self: *This) if (should_panic) Element else (FixedSizeHeapError!Element) {
            if (self.count == 0) {
                if (should_panic) {
                    @panic("Bug: Heap is empty!");
                } else {
                    return FixedSizeHeapError.HeapEmpty;
                }
            }

            const top = self.elements[0];
            self.count -= 1;
            self.elements[0] = self.elements[self.count];
            self.sift_down();
            return top;
        }

        pub fn peek(self: *This) ?Element {
            if (self.count == 0) {
                return null;
            }
            return self.elements[0];
        }

        inline fn parent(i: usize) usize {
            return (i - 1) / 2;
        }

        inline fn left_child(i: usize) usize {
            return 2 * i + 1;
        }

        inline fn right_child(i: usize) usize {
            return 2 * i + 2;
        }

        inline fn swap(self: *This, a: usize, b: usize) void {
            const tmp = self.elements[a];
            self.elements[a] = self.elements[b];
            self.elements[b] = tmp;
        }

        fn sift_up(self: *This) void {
            var i = self.count - 1;
            while (i > 0) {
                const p = parent(i);
                if (comp(self.elements[i], self.elements[p]) != Order.lt) break;
                self.swap(i, p);
                i = p;
            }
        }

        fn sift_down(self: *This) void {
            var i: usize = 0;
            while (true) {
                const a = left_child(i);
                const b = right_child(i);
                var smallest = i;

                if (a < self.count and comp(self.elements[a], self.elements[smallest]) == Order.lt) {
                    smallest = a;
                }
                if (b < self.count and comp(self.elements[b], self.elements[smallest]) == Order.lt) {
                    smallest = b;
                }
                if (smallest == i) {
                    break;
                }
                self.swap(i, smallest);
                i = smallest;
            }
        }
    };
}

const IntHeap = FixedSizeHeap(u32, 4, int_comp, false);

fn int_comp(a: u32, b: u32) Order {
    return std.math.order(a, b);
}

test "push and pop maintains min-heap order" {
    var heap = IntHeap.init();

    try heap.push(3);
    try heap.push(1);
    try heap.push(4);
    try heap.push(2);

    const a = try heap.pop();
    try std.testing.expectEqual(@as(u32, 1), a);

    const b = try heap.pop();
    try std.testing.expectEqual(@as(u32, 2), b);

    const c = try heap.pop();
    try std.testing.expectEqual(@as(u32, 3), c);

    const d = try heap.pop();
    try std.testing.expectEqual(@as(u32, 4), d);
}

test "peek returns smallest element without removal" {
    var heap = IntHeap.init();

    try heap.push(5);
    try heap.push(2);
    try heap.push(8);

    const peeked = heap.peek() orelse unreachable;
    try std.testing.expectEqual(@as(u32, 2), peeked);

    const popped = try heap.pop();
    try std.testing.expectEqual(@as(u32, 2), popped);
}

test "pop on empty heap returns HeapEmpty error" {
    var heap = IntHeap.init();
    const result = heap.pop();
    try std.testing.expectError(FixedSizeHeapError.HeapEmpty, result);
}

test "push on full heap returns HeapFull error" {
    var heap = IntHeap.init();

    try heap.push(1);
    try heap.push(2);
    try heap.push(3);
    try heap.push(4);

    const result = heap.push(5);
    try std.testing.expectError(FixedSizeHeapError.HeapFull, result);
}

test "peek returns null when heap is empty" {
    var heap = IntHeap.init();
    try std.testing.expectEqual(@as(?u32, null), heap.peek());
}
