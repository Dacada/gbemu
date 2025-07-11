const std = @import("std");
const Heap = @import("fixed_size_heap.zig").FixedSizeHeap(ScheduledEvent, 32, scheduled_event_comp, true);

pub const ScheduledEvent = struct {
    context: *anyopaque,
    callback: *const fn (*anyopaque) void,
    scheduled_at_tick: u64 = undefined, // ignored when passing it into the .schedule method
};

fn scheduled_event_comp(a: ScheduledEvent, b: ScheduledEvent) std.math.Order {
    return std.math.order(a.scheduled_at_tick, b.scheduled_at_tick);
}

pub const Scheduler = struct {
    heap: Heap,
    ticks: u64,

    pub inline fn init() Scheduler {
        return Scheduler{
            .heap = Heap.init(),
            .ticks = 0,
        };
    }

    pub fn schedule(self: *Scheduler, event: ScheduledEvent, after_ticks: u64) void {
        var e = event;
        e.scheduled_at_tick = self.ticks + after_ticks;
        self.heap.push(e);
    }

    pub fn tick(self: *Scheduler) void {
        while (self.heap.peek()) |top| {
            if (top.scheduled_at_tick == self.ticks) {
                top.callback(top.context);
                _ = self.heap.pop();
            } else {
                break;
            }
        }

        // For simplicity, assume we will never overflow (around 500,000 years of continuous operation)
        self.ticks += 1;
    }
};

const LogContext = struct {
    log: *[4]u8,
    index: *usize,
    value: u8,
};

test "single event fires after correct number of ticks" {
    var scheduler = Scheduler.init();

    var fired = false;

    const event = ScheduledEvent{
        .context = &fired,
        .callback = struct {
            fn fire(ctx: *anyopaque) void {
                const flag: *bool = @alignCast(@ptrCast(ctx));
                flag.* = true;
            }
        }.fire,
    };

    scheduler.schedule(event, 3);

    while (scheduler.ticks < 3) {
        try std.testing.expect(!fired);
        scheduler.tick();
    }

    scheduler.tick();
    try std.testing.expect(fired);
}

fn record(ctx: *anyopaque) void {
    const log_ctx: *LogContext = @alignCast(@ptrCast(ctx));
    log_ctx.log[log_ctx.index.*] = log_ctx.value;
    log_ctx.index.* += 1;
}

test "multiple events fire in correct order" {
    var scheduler = Scheduler.init();

    var log: [4]u8 = undefined;
    var index: usize = 0;

    var ctx1 = LogContext{ .log = &log, .index = &index, .value = 1 };
    var ctx2 = LogContext{ .log = &log, .index = &index, .value = 2 };
    var ctx3 = LogContext{ .log = &log, .index = &index, .value = 3 };
    var ctx4 = LogContext{ .log = &log, .index = &index, .value = 4 };

    scheduler.schedule(ScheduledEvent{ .context = &ctx2, .callback = record }, 2);
    scheduler.schedule(ScheduledEvent{ .context = &ctx1, .callback = record }, 1);
    scheduler.schedule(ScheduledEvent{ .context = &ctx4, .callback = record }, 4);
    scheduler.schedule(ScheduledEvent{ .context = &ctx3, .callback = record }, 3);

    while (scheduler.ticks <= 4) {
        scheduler.tick();
    }

    try std.testing.expectEqualSlices(u8, &.{ 1, 2, 3, 4 }, &log);
}

fn increment(ctx: *anyopaque) void {
    const c: *u8 = @alignCast(@ptrCast(ctx));
    c.* += 1;
}

test "events scheduled for same tick all fire" {
    var scheduler = Scheduler.init();

    var count: u8 = 0;

    const event = ScheduledEvent{ .context = &count, .callback = increment };

    scheduler.schedule(event, 2);
    scheduler.schedule(event, 2);
    scheduler.schedule(event, 2);

    while (scheduler.ticks < 2) {
        try std.testing.expect(count == 0);
        scheduler.tick();
    }

    scheduler.tick();
    try std.testing.expectEqual(@as(u8, 3), count);
}

test "scheduler tick counter advances correctly" {
    var scheduler = Scheduler.init();

    try std.testing.expectEqual(@as(u64, 0), scheduler.ticks);

    scheduler.tick();
    try std.testing.expectEqual(@as(u64, 1), scheduler.ticks);

    scheduler.tick();
    try std.testing.expectEqual(@as(u64, 2), scheduler.ticks);
}
