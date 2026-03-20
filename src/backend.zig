const std = @import("std");

pub const NullAudioBackend = struct {
    pub const SamplingRate = 44_100.0;
    pub inline fn init() NullAudioBackend {
        return NullAudioBackend{};
    }
    pub fn submit(_: *NullAudioBackend, _: f32, _: f32) void {}
};

pub const WavAudioBackendError = error{TrackSizeOverflow};

pub const WavAudioBackend = struct {
    pub const SamplingRate = 44_100.0;
    const Sample = struct { left: i16, right: i16 };

    filename: []const u8,
    allocator: std.mem.Allocator,
    samples: std.ArrayList(Sample),

    pub inline fn init(filename: []const u8, allocator: std.mem.Allocator) !WavAudioBackend {
        return WavAudioBackend{
            .filename = filename,
            .allocator = allocator,
            .samples = try std.ArrayList(Sample).initCapacity(allocator, 1024),
        };
    }

    pub fn deinit(self: *WavAudioBackend) void {
        self.samples.deinit(self.allocator);
    }

    pub fn submit(self: *WavAudioBackend, left: f32, right: f32) void {
        self.samples.append(self.allocator, .{
            .left = @intFromFloat(left * std.math.maxInt(i16)),
            .right = @intFromFloat(right * std.math.maxInt(i16)),
        }) catch @panic("error during submit");
    }

    pub fn writeToDisk(self: *WavAudioBackend) !void {
        const sampling_rate: u32 = @intFromFloat(SamplingRate);
        const num_channels: u16 = 2;
        const bits_per_sample: u16 = 16;

        var content = try std.ArrayList(u8).initCapacity(self.allocator, self.samples.items.len * 4);
        defer content.deinit(self.allocator);

        var buff = [4]u8{ 0, 0, 0, 0 };

        try content.appendSlice(self.allocator, "RIFF");
        try content.appendSlice(self.allocator, &buff); // size placeholder
        try content.appendSlice(self.allocator, "WAVE");
        try content.appendSlice(self.allocator, "fmt ");

        // PCM format: chunk size 16, tag 1
        std.mem.writeInt(u32, &buff, 16, .little);
        try content.appendSlice(self.allocator, &buff);
        std.mem.writeInt(u16, buff[0..2], 1, .little);
        try content.appendSlice(self.allocator, buff[0..2]);

        // Number of chanels
        std.mem.writeInt(u16, buff[0..2], num_channels, .little);
        try content.appendSlice(self.allocator, buff[0..2]);

        // Sampling rate
        std.mem.writeInt(u32, &buff, sampling_rate, .little);
        try content.appendSlice(self.allocator, &buff);

        // Byte rate
        std.mem.writeInt(u32, &buff, (sampling_rate * bits_per_sample * num_channels) / 8, .little);
        try content.appendSlice(self.allocator, &buff);

        // Block align
        std.mem.writeInt(u16, buff[0..2], (bits_per_sample * num_channels) / 8, .little);
        try content.appendSlice(self.allocator, buff[0..2]);

        // Bits per sample
        std.mem.writeInt(u16, buff[0..2], bits_per_sample, .little);
        try content.appendSlice(self.allocator, buff[0..2]);

        try content.appendSlice(self.allocator, "data");

        // Number of bytes in data
        const bytes: u64 = (bits_per_sample * num_channels * @as(u64, @intCast(self.samples.items.len))) / 8;
        if (bytes > std.math.maxInt(u32)) {
            return WavAudioBackendError.TrackSizeOverflow;
        }
        std.mem.writeInt(u32, &buff, @truncate(bytes), .little);
        try content.appendSlice(self.allocator, &buff);

        for (self.samples.items) |sample| {
            std.mem.writeInt(i16, buff[0..2], sample.left, .little);
            std.mem.writeInt(i16, buff[2..4], sample.right, .little);
            try content.appendSlice(self.allocator, &buff);
        }

        const size: u32 = @intCast(content.items.len - 8);
        std.mem.writeInt(u32, content.items[4..8], size, .little);

        const file = try std.fs.createFileAbsolute(self.filename, .{});
        try file.writeAll(content.items);
        file.close();
    }
};
