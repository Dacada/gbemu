const std = @import("std");
const lib = @import("lib");
const Channel1 = lib.channel.Channel(1);
const Channel2 = lib.channel.Channel(2);

const cpu_freq_hz = 4_194_304;
const apu_freq_hz = cpu_freq_hz / 4;
const frame_seq_freq_hz = 512;
const apu_ticks_per_div = apu_freq_hz / frame_seq_freq_hz;

// target tone: ~440Hz
// f = 131072 / (2048 - N)
// N = 2048 - 131072 / f
// f = 440 -> N ~= 1750
const period: u11 = 1750;
const period_low = period & 0xFF;
const period_high = (period & 0x0700) >> 8;

fn wait_and_collect_until(channel: anytype, max_samples: usize) !std.ArrayList(f32) {
    var samples = std.ArrayList(f32).init(std.testing.allocator);
    var div_counter: usize = 0;

    while (samples.items.len <= max_samples) {
        const sample = channel.tick();
        try samples.append(sample);

        div_counter += 1;
        if (div_counter == apu_ticks_per_div) {
            channel.divtick();
            div_counter = 0;
        }
    }

    return samples;
}

test "test channel 1" {
    // freq sweep
    // pace = slowest, direction = increasing, step = large (small increments)
    const nr10_val = 0b0_111_0_111;

    // duty and length
    // duty = 50%, length = don't care (disabled)
    const nr11_val = 0b10_000000;

    // envelope:
    // disabled (but do not disable the dac!)
    const nr12_val = 0b1111_0_000;

    // period and length enable
    const nr13_val = period_low;
    const nr14_val = 0b1_0_000_000 | period_high;

    var channel = Channel1.init();

    try std.testing.expect(!channel.write(0, 0).any());
    try std.testing.expect(!channel.write(1, 0).any());
    try std.testing.expect(!channel.write(2, 0).any());
    try std.testing.expect(!channel.write(3, 0).any());
    try std.testing.expect(!channel.write(4, 0).any());

    try std.testing.expect(!channel.write(0, nr10_val).any());
    try std.testing.expect(!channel.write(1, nr11_val).any());
    try std.testing.expect(!channel.write(2, nr12_val).any());
    try std.testing.expect(!channel.write(3, nr13_val).any());
    try std.testing.expect(!channel.write(4, nr14_val).any());

    var samples = try wait_and_collect_until(&channel, 1_200_000);
    defer samples.deinit();

    const output_file = try std.fs.createFileAbsolute("/tmp/out_ch1.raw", .{});
    defer output_file.close();
    try output_file.writeAll(std.mem.sliceAsBytes(samples.items));

    const expected: []const f32 = @alignCast(std.mem.bytesAsSlice(f32, @embedFile("res/ch1.raw")));
    try std.testing.expectEqualSlices(f32, expected, samples.items);
}

test "test channel 2" {
    // duty and length
    // duty = 50%, length = 0 (64 steps)
    const nr21_val = 0b10_000000;

    // envelope:
    // initial volume = 0, direction = up, period = 1 step
    const nr22_val = 0b0000_1_001;

    // period and length enable
    const nr23_val = period_low;
    const nr24_val = 0b1_1_000_000 | period_high;

    var channel = Channel2.init();

    try std.testing.expect(!channel.write(1, 0).any());
    try std.testing.expect(!channel.write(2, 0).any());
    try std.testing.expect(!channel.write(3, 0).any());
    try std.testing.expect(!channel.write(4, 0).any());

    try std.testing.expect(!channel.write(1, nr21_val).any());
    try std.testing.expect(!channel.write(2, nr22_val).any());
    try std.testing.expect(!channel.write(3, nr23_val).any());
    try std.testing.expect(!channel.write(4, nr24_val).any());

    var samples = try wait_and_collect_until(&channel, 300_000);
    defer samples.deinit();

    const output_file = try std.fs.createFileAbsolute("/tmp/out_ch2.raw", .{});
    defer output_file.close();
    try output_file.writeAll(std.mem.sliceAsBytes(samples.items));

    const expected: []const f32 = @alignCast(std.mem.bytesAsSlice(f32, @embedFile("res/ch2.raw")));
    try std.testing.expectEqualSlices(f32, expected, samples.items);
}
