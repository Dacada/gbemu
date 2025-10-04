const std = @import("std");
const lib = @import("lib");
const Channel1 = lib.channel.Channel(1);
const Channel2 = lib.channel.Channel(2);
const Channel3 = lib.channel.Channel(3);
const Channel4 = lib.channel.Channel(4);

const cpu_freq_hz = 4_194_304;
const apu_freq_hz = cpu_freq_hz / 2;
const frame_seq_freq_hz = 512;
const apu_ticks_per_div = apu_freq_hz / frame_seq_freq_hz;

// target tone: ~440Hz (for square channels)
// f = 131072 / (2048 - N)
// N = 2048 - 131072 / f
// f = 440 -> N ~= 1750
const period: u11 = 1750;
const period_low = period & 0xFF;
const period_high = (period & 0x0700) >> 8;

// target tone: ~440Hz (for wave channel)
// f = 65536 / (2048 - N)
// N = 2048 - 65536 / f
// f = 440 -> N ~= 1899
const period2: u11 = 1899;
const period2_low = period2 & 0xFF;
const period2_high = (period2 & 0x700) >> 8;

fn waitAndCollectUntil(channel: anytype, max_samples: usize) !std.ArrayList(f32) {
    var samples = try std.ArrayList(f32).initCapacity(std.testing.allocator, max_samples);
    var div_counter: usize = 0;

    while (samples.items.len <= max_samples) {
        const sample = channel.tick();
        try samples.append(std.testing.allocator, sample);

        div_counter += 1;
        if (div_counter == apu_ticks_per_div) {
            channel.divtick();
            div_counter = 0;
        }
    }

    return samples;
}

fn getExpectedArray(comptime name: []const u8) ![]const f32 {
    const bytes = @embedFile(name);
    const floats = try std.testing.allocator.alloc(f32, bytes.len / 4);
    @memcpy(@as([]u8, @ptrCast(floats)), bytes);
    return floats;
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

    const nsamples: usize = 600_000;

    var samples = try waitAndCollectUntil(&channel, nsamples);
    defer samples.deinit(std.testing.allocator);

    const output_file = try std.fs.createFileAbsolute("/tmp/out_ch2.raw", .{});
    defer output_file.close();
    try output_file.writeAll(std.mem.sliceAsBytes(samples.items));

    const expected: []const f32 = try getExpectedArray("res/ch2.raw");
    defer std.testing.allocator.free(expected);
    try std.testing.expectEqualSlices(f32, expected, samples.items);
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

    const nsamples: usize = 2_400_000;

    var samples = try waitAndCollectUntil(&channel, nsamples);
    defer samples.deinit(std.testing.allocator);

    const output_file = try std.fs.createFileAbsolute("/tmp/out_ch1.raw", .{});
    defer output_file.close();
    try output_file.writeAll(std.mem.sliceAsBytes(samples.items));

    const expected: []const f32 = try getExpectedArray("res/ch1.raw");
    defer std.testing.allocator.free(expected);
    try std.testing.expectEqualSlices(f32, expected, samples.items);
}

test "test channel 4" {
    // length = mid
    const nr41_val = 0b10_0000;

    // initial volume = 15, no envelope
    const nr42_val = 0b1111_0_000;

    // not very fast period, narrow width
    const nr43_val = 0b0011_1_001;

    // length enable
    const nr44_val = 0b1_1_000_000;

    var channel = Channel4.init();

    try std.testing.expect(!channel.write(1, 0).any());
    try std.testing.expect(!channel.write(2, 0).any());
    try std.testing.expect(!channel.write(3, 0).any());
    try std.testing.expect(!channel.write(4, 0).any());

    try std.testing.expect(!channel.write(1, nr41_val).any());
    try std.testing.expect(!channel.write(2, nr42_val).any());
    try std.testing.expect(!channel.write(3, nr43_val).any());
    try std.testing.expect(!channel.write(4, nr44_val).any());

    const nsamples = 800_000;

    var samples = try waitAndCollectUntil(&channel, nsamples);
    defer samples.deinit(std.testing.allocator);

    // change width to wide and retrigger
    const nr43_val_2 = nr43_val & 0b1111_0_111;
    try std.testing.expect(!channel.write(3, nr43_val_2).any());
    try std.testing.expect(!channel.write(4, nr44_val).any());

    var samples_2 = try waitAndCollectUntil(&channel, nsamples);
    defer samples_2.deinit(std.testing.allocator);

    const output_file = try std.fs.createFileAbsolute("/tmp/out_ch4.raw", .{});
    defer output_file.close();
    try output_file.writeAll(std.mem.sliceAsBytes(samples.items));
    try output_file.writeAll(std.mem.sliceAsBytes(samples_2.items));

    const expected: []const f32 = try getExpectedArray("res/ch4.raw");
    defer std.testing.allocator.free(expected);
    try std.testing.expectEqualSlices(f32, expected[0..(nsamples + 1)], samples.items);
    try std.testing.expectEqualSlices(f32, expected[(nsamples + 1)..], samples_2.items);
}

test "test channel 3" {
    // dac on
    const nr30_val = 0b1000_0000;

    // maximum length timer
    const nr31_val = 0;

    // 100% output level
    const nr32_val = 0b0_01_00000;

    // period
    const nr33_val = period2_low;

    // length enable, trigger
    const nr34_val = 0b1_1_000_000 | period2_high;

    var channel = Channel3.init();

    try std.testing.expect(!channel.write(0, 0).any());
    try std.testing.expect(!channel.write(1, 0).any());
    try std.testing.expect(!channel.write(2, 0).any());
    try std.testing.expect(!channel.write(3, 0).any());
    try std.testing.expect(!channel.write(4, 0).any());

    // Write a sawtooth cycle to the wave memory
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 0, 0xFF).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 1, 0xEE).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 2, 0xDD).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 3, 0xCC).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 4, 0xBB).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 5, 0xAA).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 6, 0x99).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 7, 0x88).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 8, 0x77).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 9, 0x66).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 10, 0x55).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 11, 0x44).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 12, 0x33).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 13, 0x22).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 14, 0x11).any());
    try std.testing.expect(!Channel3.wave_memory_interface.write(&channel, 15, 0x00).any());

    try std.testing.expect(!channel.write(0, nr30_val).any());
    try std.testing.expect(!channel.write(1, nr31_val).any());
    try std.testing.expect(!channel.write(2, nr32_val).any());
    try std.testing.expect(!channel.write(3, nr33_val).any());
    try std.testing.expect(!channel.write(4, nr34_val).any());

    const nsamples = 2_500_000;

    var samples = try waitAndCollectUntil(&channel, nsamples);
    defer samples.deinit(std.testing.allocator);

    const output_file = try std.fs.createFileAbsolute("/tmp/out_ch3.raw", .{});
    defer output_file.close();
    try output_file.writeAll(std.mem.sliceAsBytes(samples.items));

    const expected: []const f32 = try getExpectedArray("res/ch3.raw");
    defer std.testing.allocator.free(expected);
    try std.testing.expectEqualSlices(f32, expected, samples.items);
}
