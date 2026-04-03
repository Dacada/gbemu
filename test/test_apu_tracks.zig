const std = @import("std");
const lib = @import("lib");
const tracker = lib.tracker;

const Container = lib.dependency_container.Container(.{
    .audio_backend = .mock_wav,
    .debugger = .mock,
});
const Apu = Container.Apu;

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

fn expectIdenticalFiles(comptime expected: []const u8, actual: []const u8, allocator: std.mem.Allocator) !void {
    const expectedBytes = @embedFile(expected);

    const actualFile = try std.fs.openFileAbsolute(actual, .{});
    defer actualFile.close();
    const actualBytes = try actualFile.readToEndAlloc(allocator, expectedBytes.len);
    defer allocator.free(actualBytes);

    try std.testing.expectEqualSlices(u8, expectedBytes, actualBytes);
}

fn testTrack(allocator: std.mem.Allocator, song: tracker.Song, comptime name: []const u8) !void {
    const ticks_per_subdivision = ticksPerSubdivision(song.metadata.bpm, song.metadata.tpb);

    //const actualBpm = actualBpmFromSubdivisionTicks(ticks_per_subdivision, song.metadata.tpb);
    //std.debug.print("Target BPM: {d}\nActual BPM: {d}\n", .{ song.metadata.bpm, actualBpm });

    const tmp_file = std.fmt.comptimePrint("/tmp/{s}.wav", .{name});
    const res_file = std.fmt.comptimePrint("res/{s}.wav", .{name});

    var container = Container.init(.{
        .audio_wav_filename = tmp_file,
        .allocator = allocator,
    });

    var apu = try container.get_apu();
    var backend = try container.get_audio_backend();

    // container won't deinit stuff for us
    defer backend.deinit();

    const events = try tracker.play(song, allocator);
    defer allocator.free(events);

    // Start sound system
    try std.testing.expect(!apu.write(0x16, 0b1000_0000).any());

    var total_ticks: usize = 0;
    var div_counter: usize = 0;
    for (events) |event| {
        defer allocator.free(event);

        for (event) |write| {
            //std.debug.print("addr={x} set={b} clear={b} | time={d}\n", .{ write.address, write.set_mask, write.clear_mask, total_ticks });
            const read_flags, const val = apu.read(write.address);
            try std.testing.expect(!read_flags.any());
            const write_flags = apu.write(write.address, write.apply(val));
            try std.testing.expect(!write_flags.any());
        }

        tickApu(apu, ticks_per_subdivision, &div_counter);
        total_ticks += ticks_per_subdivision;
    }

    try backend.writeToDisk();

    try expectIdenticalFiles(res_file, tmp_file, allocator);
}

test "test_track_1" {
    const allocator = std.testing.allocator;

    const meta = tracker.SongMetadata{ .bpm = 110, .tpb = 4 };
    var song = try tracker.Song.init(allocator, meta);
    defer song.deinit(allocator);

    song
        .compose(allocator)
        .channel(null).enable().defaults()
        .channel(.ch1).disable()
        .channel(.ch3).disable()
        .channel(.ch4).disable()
        .channel(.ch2).enable().defaults()
        .phrase(&.{
            .{ "A4", meta.beat(1, 4) },
            .{ "G#4", meta.beat(1, 4) },
            .{ "A#4", meta.beat(1, 4) },
            .{ "A4", meta.beat(1, 1) },
            .{ null, meta.beat(1, 4) },
        })
        .phrase(&.{
            .{ "A4", meta.beat(1, 4) },
            .{ "G#4", meta.beat(1, 4) },
            .{ "A#4", meta.beat(1, 4) },
            .{ "F#4", meta.beat(1, 1) },
            .{ null, meta.beat(1, 4) },
        })
        .phrase(&.{
            .{ "A4", meta.beat(1, 4) },
            .{ "G#4", meta.beat(1, 4) },
            .{ "A#4", meta.beat(1, 4) },
            .{ "A4", meta.beat(1, 4) },
            .{ "G#4", meta.beat(1, 4) },
            .{ "F#4", meta.beat(1, 4) },
            .{ "A4", meta.beat(1, 2) },
        })
        .phrase(&.{
            .{ "B4", meta.beat(1, 4) },
            .{ null, meta.beat(1, 4) },
            .{ "B4", meta.beat(1, 4) },
            .{ null, meta.beat(1, 4) },
            .{ "B4", meta.beat(1, 1) },
        })
        .rest(1) // need this to get the last note to sound out completely.
        .finish();

    try testTrack(allocator, song, "track1");
}

test "happy birthday" {
    const allocator = std.testing.allocator;

    // By ChatGPT, from my DSL
    const meta = tracker.SongMetadata{ .bpm = 32, .tpb = 32 };
    var song = try tracker.Song.init(allocator, meta);
    defer song.deinit(allocator);

    song
        .compose(allocator)
        .channel(null).enable().defaults()
        .channel(.ch1).disable()
        .channel(.ch3).disable()
        .channel(.ch4).disable()
        .channel(.ch2).enable().defaults()
        .phrase(&.{
            // Happy birthday to you
            .{ "G4", meta.beat(11, 32) }, .{ null, meta.beat(1, 32) },
            .{ "G4", meta.beat(3, 32) },  .{ null, meta.beat(1, 32) },
            .{ "A4", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "G4", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "C5", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "B4", meta.beat(15, 32) }, .{ null, meta.beat(1, 32) },

            // Happy birthday to you
            .{ "G4", meta.beat(11, 32) }, .{ null, meta.beat(1, 32) },
            .{ "G4", meta.beat(3, 32) },  .{ null, meta.beat(1, 32) },
            .{ "A4", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "G4", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "D5", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "C5", meta.beat(15, 32) }, .{ null, meta.beat(1, 32) },

            // Happy birthday dear ___
            .{ "G4", meta.beat(11, 32) }, .{ null, meta.beat(1, 32) },
            .{ "G4", meta.beat(3, 32) },  .{ null, meta.beat(1, 32) },
            .{ "G5", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "E5", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "C5", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "B4", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "A4", meta.beat(15, 32) }, .{ null, meta.beat(1, 32) },

            // Happy birthday to you
            .{ "F5", meta.beat(11, 32) }, .{ null, meta.beat(1, 32) },
            .{ "F5", meta.beat(3, 32) },  .{ null, meta.beat(1, 32) },
            .{ "E5", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "C5", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "D5", meta.beat(7, 32) },  .{ null, meta.beat(1, 32) },
            .{ "C5", meta.beat(31, 32) },
        })
        .rest(1)
        .finish();

    try testTrack(allocator, song, "birthday");
}

test "showcase" {
    const allocator = std.testing.allocator;

    const meta = tracker.SongMetadata{ .bpm = 120, .tpb = 64 };
    var song = try tracker.Song.init(allocator, meta);
    defer song.deinit(allocator);

    // smooth sine-like
    try song.waveforms.append(allocator, .{
        .id = 0,
        .waveform = .{ 0x0, 0x2, 0x4, 0x6, 0x8, 0xA, 0xC, 0xE, 0xF, 0xE, 0xC, 0xA, 0x8, 0x6, 0x4, 0x2, 0x0, 0xE, 0xC, 0xA, 0x8, 0x6, 0x4, 0x2, 0x0, 0x2, 0x4, 0x6, 0x8, 0xA, 0xC, 0xE },
    });

    // sharp square variant
    try song.waveforms.append(allocator, .{
        .id = 1,
        .waveform = .{ 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 },
    });

    // sawtooth ramp
    try song.waveforms.append(allocator, .{
        .id = 2,
        .waveform = .{ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF, 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF },
    });

    // triangle wave
    try song.waveforms.append(allocator, .{
        .id = 3,
        .waveform = .{ 0x0, 0x2, 0x4, 0x6, 0x8, 0xA, 0xC, 0xE, 0xF, 0xE, 0xC, 0xA, 0x8, 0x6, 0x4, 0x2, 0x0, 0x2, 0x4, 0x6, 0x8, 0xA, 0xC, 0xE, 0xF, 0xE, 0xC, 0xA, 0x8, 0x6, 0x4, 0x2 },
    });

    // formant-ish vocal tone
    try song.waveforms.append(allocator, .{
        .id = 4,
        .waveform = .{ 0x0, 0x3, 0x7, 0xB, 0xF, 0xD, 0xA, 0x6, 0x3, 0x1, 0x2, 0x5, 0x9, 0xD, 0xF, 0xC, 0x8, 0x4, 0x2, 0x3, 0x6, 0xA, 0xE, 0xF, 0xC, 0x9, 0x5, 0x2, 0x1, 0x3, 0x7, 0xB },
    });

    // metallic / bell-like
    try song.waveforms.append(allocator, .{
        .id = 5,
        .waveform = .{ 0xF, 0x1, 0xE, 0x2, 0xD, 0x3, 0xC, 0x4, 0xB, 0x5, 0xA, 0x6, 0x9, 0x7, 0x8, 0x8, 0x7, 0x9, 0x6, 0xA, 0x5, 0xB, 0x4, 0xC, 0x3, 0xD, 0x2, 0xE, 0x1, 0xF, 0x0, 0xF },
    });

    // noisy digital texture
    try song.waveforms.append(allocator, .{
        .id = 6,
        .waveform = .{ 0xF, 0x0, 0xA, 0x3, 0xC, 0x1, 0x8, 0x5, 0x2, 0xE, 0x4, 0xB, 0x6, 0x9, 0xD, 0x7, 0x1, 0xC, 0x3, 0xF, 0x5, 0x0, 0xA, 0x8, 0xE, 0x2, 0x9, 0x4, 0xB, 0x6, 0xD, 0x7 },
    });

    // pulse sweep (fake PWM)
    try song.waveforms.append(allocator, .{
        .id = 7,
        .waveform = .{ 0xF, 0xF, 0xF, 0xF, 0x0, 0x0, 0x0, 0x0, 0xF, 0xF, 0xF, 0x0, 0x0, 0x0, 0x0, 0x0, 0xF, 0xF, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xF, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 },
    });

    var c = song.compose(allocator)
        .channel(null).enable().defaults()
        .channel(.ch1).enable().defaults().advance(1)
        .channel(.ch2).enable().defaults().advance(1)
        .channel(.ch3).enable().defaults().advance(1)
        .channel(.ch4).enable().defaults().advance(1);

    c = c.channel(.ch1);
    var ear: usize = 1;
    for (1..8) |_| {
        inline for (.{ 6, 5, 4 }) |oct| {
            inline for (.{ "A", "D", "F" }) |n| {
                if (ear == 1) {
                    c = c.pan(true, true);
                    ear = 2;
                } else if (ear == 2) {
                    c = c.pan(true, false);
                    ear = 3;
                } else if (ear == 3) {
                    c = c.pan(true, true);
                    ear = 4;
                } else if (ear == 4) {
                    c = c.pan(false, true);
                    ear = 1;
                }

                const note = std.fmt.comptimePrint("{s}{d}", .{ n, oct });

                c = c.volume(5)
                    .pitch(note)
                    .periodSweep(.{
                        .pace = 2,
                        .direction = .increase,
                        .step = 7,
                    })
                    .advance(meta.beat(1, 4))
                    .periodSweep(.{
                        .pace = 2,
                        .direction = .decrease,
                        .step = 7,
                    })
                    .advance(meta.beat(1, 4))
                    .periodSweep(null)
                    .volume(0)
                    .advance(meta.beat(1, 8));
            }
        }
    }

    c = c.channel(.ch2);
    inline for (.{ .one_in_eight, .one_in_four, .one_in_two, .three_in_four }) |duty| {
        inline for (.{ "C4", "D4", "E4", "F4", "G4" }) |note| {
            c = c
                .duty(duty)
                .pitch(note)

                // attack
                .volume(13)
                .envelope(.{
                    .dir = .increase,
                    .pace = 7,
                })
                .advance(meta.beat(1, 2))

                // sustain
                .envelope(null)
                .advance(meta.beat(1, 1))

                // decay
                .envelope(.{
                    .dir = .decrease,
                    .pace = 1,
                })
                .advance(meta.beat(1, 2));
        }
    }

    c = c
        .channel(.ch3)
        .dac(false)
        .outputLevel(.full)
        .advance(meta.beat(2, 1));
    inline for (0..8) |i| {
        c = c
            .waveform(i)
            .advance(1);
        inline for (.{ "C5", "C4", "G5", "G4" }) |pitch| {
            c = c
                .dac(true)
                .pitch(pitch)
                .advance(meta.beat(9, 8));
        }
        c = c
            .dac(false)
            .advance(meta.beat(1, 4));
    }

    c = c.channel(.ch4);
    inline for (.{ .wide, .narrow }) |width| {
        inline for (0..std.math.maxInt(u3)) |div| {
            inline for (0..std.math.maxInt(u4)) |shift| {
                c = c
                    .volume(10)
                    .lfsr(div, shift, width)
                    .advance(meta.beat(11, 64));
            }
        }
        if (width == .wide) {
            c = c
                .volume(0)
                .advance(meta.beat(1, 1));
        }
    }
    c = c
        .volume(0);

    try testTrack(allocator, song, "showcase");
}
