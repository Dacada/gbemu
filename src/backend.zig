const std = @import("std");

pub const NullAudioBackend = struct {
    pub const SamplingRate = 44_100.0;
    pub inline fn init() NullAudioBackend {
        return NullAudioBackend{};
    }
    pub fn submit(_: *NullAudioBackend, _: f32, _: f32) void {}
};

pub const NullVideoBackend = struct {
    const Width = 160;
    const Height = 144;
    pub inline fn init() NullVideoBackend {
        return NullVideoBackend{};
    }
    pub fn submit(_: *NullVideoBackend, _: u2) void {}
};

pub const WavAudioBackendError = error{TrackSizeOverflow};

pub const WavAudioBackend = struct {
    pub const SamplingRate = 44_100.0;
    const Sample = struct { left: i16, right: i16 };

    allocator: ?std.mem.Allocator,
    samples: ?std.ArrayList(Sample),

    pub inline fn init() WavAudioBackend {
        return WavAudioBackend{
            .allocator = null,
            .samples = null,
        };
    }

    pub fn setup(self: *WavAudioBackend, allocator: std.mem.Allocator) !void {
        self.allocator = allocator;
        self.samples = try std.ArrayList(Sample).initCapacity(allocator, 1024);
    }

    pub fn deinit(self: *WavAudioBackend) void {
        if (self.samples) |*samples| {
            if (self.allocator) |allocator| {
                samples.deinit(allocator);
            }
        }
    }

    pub fn submit(self: *WavAudioBackend, left: f32, right: f32) void {
        if (self.allocator == null or self.samples == null) {
            @panic("call to submit before call to setup!");
        }

        // if (left > 1.0 or left < -1.0) {
        //     std.debug.print("left channel out of bounds: {d}\n", .{left});
        // }
        // if (right > 1.0 or right < -1.0) {
        //     std.debug.print("right channel out of bounds: {d}\n", .{right});
        // }
        const cleft = @max(@min(1.0, left), -1.0);
        const cright = @max(@min(1.0, right), -1.0);
        self.samples.?.append(self.allocator.?, .{
            .left = @intFromFloat(cleft * std.math.maxInt(i16)),
            .right = @intFromFloat(cright * std.math.maxInt(i16)),
        }) catch @panic("error during submit");
    }

    pub fn writeToDisk(self: *WavAudioBackend, io: std.Io, filename: []const u8) !void {
        if (self.allocator == null or self.samples == null) {
            @panic("call to writeToDisk before call to setup!");
        }

        const allocator = self.allocator.?;
        const samples = self.samples.?;

        const sampling_rate: u32 = @intFromFloat(SamplingRate);
        const num_channels: u16 = 2;
        const bits_per_sample: u16 = 16;

        var content = try std.ArrayList(u8).initCapacity(allocator, samples.items.len * 4);
        defer content.deinit(allocator);

        var buff = [4]u8{ 0, 0, 0, 0 };

        try content.appendSlice(allocator, "RIFF");
        try content.appendSlice(allocator, &buff); // size placeholder
        try content.appendSlice(allocator, "WAVE");
        try content.appendSlice(allocator, "fmt ");

        // PCM format: chunk size 16, tag 1
        std.mem.writeInt(u32, &buff, 16, .little);
        try content.appendSlice(allocator, &buff);
        std.mem.writeInt(u16, buff[0..2], 1, .little);
        try content.appendSlice(allocator, buff[0..2]);

        // Number of chanels
        std.mem.writeInt(u16, buff[0..2], num_channels, .little);
        try content.appendSlice(allocator, buff[0..2]);

        // Sampling rate
        std.mem.writeInt(u32, &buff, sampling_rate, .little);
        try content.appendSlice(allocator, &buff);

        // Byte rate
        std.mem.writeInt(u32, &buff, (sampling_rate * bits_per_sample * num_channels) / 8, .little);
        try content.appendSlice(allocator, &buff);

        // Block align
        std.mem.writeInt(u16, buff[0..2], (bits_per_sample * num_channels) / 8, .little);
        try content.appendSlice(allocator, buff[0..2]);

        // Bits per sample
        std.mem.writeInt(u16, buff[0..2], bits_per_sample, .little);
        try content.appendSlice(allocator, buff[0..2]);

        try content.appendSlice(allocator, "data");

        // Number of bytes in data
        const bytes: u64 = (bits_per_sample * num_channels * @as(u64, @intCast(samples.items.len))) / 8;
        if (bytes > std.math.maxInt(u32)) {
            return WavAudioBackendError.TrackSizeOverflow;
        }
        std.mem.writeInt(u32, &buff, @truncate(bytes), .little);
        try content.appendSlice(allocator, &buff);

        for (samples.items) |sample| {
            std.mem.writeInt(i16, buff[0..2], sample.left, .little);
            std.mem.writeInt(i16, buff[2..4], sample.right, .little);
            try content.appendSlice(allocator, &buff);
        }

        const size: u32 = @intCast(content.items.len - 8);
        std.mem.writeInt(u32, content.items[4..8], size, .little);

        const file = try std.Io.Dir.createFileAbsolute(io, filename, .{});
        try file.writePositionalAll(io, content.items, 0);
        file.close(io);
    }
};
