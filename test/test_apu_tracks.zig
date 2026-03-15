const std = @import("std");
const lib = @import("lib");
const tracker = lib.tracker;
const Backend = lib.backend.WavAudioBackend;
const Apu = lib.apu.Apu(Backend);

const cpu_freq_hz = 4_194_304;
const apu_freq_hz = cpu_freq_hz / 2;
const frame_seq_freq_hz = 512;
const apu_ticks_per_div = apu_freq_hz / frame_seq_freq_hz;

fn ticksPerSubdivision(bpm: usize, tpb: usize) usize {
    const bpm_f: f64 = @floatFromInt(bpm);
    const tpb_f: f64 = @floatFromInt(tpb);
    const apu_f: f64 = @floatFromInt(apu_freq_hz);

    // seconds_per_subdiv = 60 / (BPM * ticks_per_beat)
    // target_apu_ticks   = apu_freq_hz * seconds_per_subdiv
    const target = apu_f * 60.0 / (bpm_f * tpb_f);
    return @intFromFloat(@round(target));
}

fn actualBpmFromSubdivisionTicks(ticks_per_subdivision: usize, tpb: usize) f64 {
    const apu_f: f64 = @floatFromInt(apu_freq_hz);
    const tps_f: f64 = @floatFromInt(ticks_per_subdivision);
    const tpb_f: f64 = @floatFromInt(tpb);

    // seconds_per_beat = (ticks_per_subdiv * ticks_per_beat) / apu_freq_hz
    // BPM_actual       = 60 / seconds_per_beat
    return 60.0 * apu_f / (tps_f * tpb_f);
}

fn tickApu(apu: *Apu, ticks: usize, div_counter: *usize) void {
    for (0..ticks) |_| {
        apu.tick();
        div_counter.* += 1;
        if (div_counter.* == apu_ticks_per_div) {
            apu.divtick();
            div_counter.* = 0;
        }
    }
}

test "test_track_1" {
    const allocator = std.testing.allocator;

    var song = try tracker.Song.init(allocator, .{ .bpm = 110, .tpb = 4 });
    defer song.deinit(allocator);
    try song.addNotesCh2(allocator, 0, 0, .one_in_two, 0b1111, &.{
        .{ "A4", 1 },
        .{ "G#4", 1 },
        .{ "A#4", 1 },
        .{ "A4", 4 },
        .{ null, 1 },

        .{ "A4", 1 },
        .{ "G#4", 1 },
        .{ "A#4", 1 },
        .{ "F#4", 4 },
        .{ null, 1 },

        .{ "A4", 1 },
        .{ "G#4", 1 },
        .{ "A#4", 1 },
        .{ "A4", 1 },
        .{ "G#4", 1 },
        .{ "F#4", 1 },
        .{ "A4", 2 },

        .{ "B4", 1 },
        .{ null, 1 },
        .{ "B4", 1 },
        .{ null, 1 },
        .{ "B4", 4 },
    });

    const ticks_per_subdivision = ticksPerSubdivision(song.metadata.bpm, song.metadata.tpb);

    const actualBpm = actualBpmFromSubdivisionTicks(ticks_per_subdivision, song.metadata.tpb);
    std.debug.print("Target BPM: {d}\nActual BPM: {d}\n", .{ song.metadata.bpm, actualBpm });

    var backend = try Backend.init("/tmp/test_track_1.wav", allocator);
    defer backend.deinit();
    var apu = Apu.init(&backend);

    const events = try tracker.play(song, allocator);
    defer allocator.free(events);

    var div_counter: usize = 0;
    for (events) |event| {
        for (event) |write| {
            const val = apu.read(write.address);
            try std.testing.expect(!val[0].any());
            const flag = apu.write(write.address, write.apply(val[1]));
            try std.testing.expect(!flag.any());
            tickApu(&apu, ticks_per_subdivision, &div_counter);
        }
        allocator.free(event);
    }
    allocator.free(events);

    try backend.writeToDisk();
}
