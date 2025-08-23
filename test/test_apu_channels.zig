const std = @import("std");
const lib = @import("lib");
const Channel2 = lib.channel.Channel2;

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

fn wait_and_collect_until(channel: *Channel2, max_samples: usize) !std.ArrayList(f32) {
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

test "test channel 2" {
    // duty and length
    // duty = 50%, length = 0 (64 steps)
    const nr21_val = (0b10 << 6) | 0;

    // envelope:
    // initial volume = 0, direction = up, period = 1 step
    const nr22_val = (0 << 4) | (1 << 3) | 0b001;

    // period and length enable
    const nr23_val = period_low;
    const nr24_val = (1 << 7) | (1 << 6) | period_high;

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
