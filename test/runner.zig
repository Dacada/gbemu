const std = @import("std");
const builtin = @import("builtin");

// Ignore output sent through the logging system, except for the testutil scope, which we will display as captured output
const logCaptureContext = struct {
    var allocator: ?std.mem.Allocator = null;
    var captured: ?std.ArrayList([]const u8) = null;
};
fn reportingLogFn(comptime _: std.log.Level, comptime scope: @TypeOf(.enum_literal), comptime format: []const u8, args: anytype) void {
    if (scope != .testutil) {
        return;
    }

    if (logCaptureContext.allocator == null or logCaptureContext.captured == null) {
        return;
    }

    const buffer = std.fmt.allocPrint(logCaptureContext.allocator.?, format, args) catch {
        return;
    };

    logCaptureContext.captured.?.append(buffer) catch {
        return;
    };
}
pub const std_options = .{
    .logFn = reportingLogFn,
};

// Capture stderr/stdout output
var current_stderr: std.posix.fd_t = std.posix.STDERR_FILENO;
var current_stdout: std.posix.fd_t = std.posix.STDOUT_FILENO;
pub const os = struct {
    pub const io = struct {
        pub fn getStdErrHandle() std.posix.fd_t {
            return current_stderr;
        }
        pub fn getStdOutHandle() std.posix.fd_t {
            return current_stdout;
        }
    };
};

fn extractCapturedOutput(fd: std.posix.fd_t, allocator: std.mem.Allocator) !?[]const u8 {
    try std.posix.lseek_END(fd, 0);
    const offset = try std.posix.lseek_CUR_get(fd);

    if (offset <= 0) {
        return null;
    }

    const buff = try allocator.alloc(u8, offset);

    // for simplicity, assume the read will always be complete
    try std.posix.lseek_SET(fd, 0);
    _ = try std.posix.read(fd, buff);
    try std.posix.ftruncate(fd, 0);

    return buff;
}

fn makeColorOutputFunctions(comptime color: std.io.tty.Color) type {
    return struct {
        fn beginColor(tty: std.io.tty.Config, writer: anytype) !void {
            try tty.setColor(writer, color);
        }

        fn writeColor(tty: std.io.tty.Config, writer: anytype, bytes: []const u8) !void {
            try tty.setColor(writer, color);
            try writer.writeAll(bytes);
            try tty.setColor(writer, .reset);
        }
    };
}

fn endColor(tty: std.io.tty.Config, writer: anytype) !void {
    try tty.setColor(writer, .reset);
}

const goodOutput = makeColorOutputFunctions(std.io.tty.Color.green);
const errorOutput = makeColorOutputFunctions(std.io.tty.Color.red);
const importantOutput = makeColorOutputFunctions(std.io.tty.Color.blue);

fn displayCapturedOutputLine(tty: std.io.tty.Config, writer: anytype, name: []const u8, content: []const u8) !void {
    try writer.writeAll("\n    ↪ ");
    try importantOutput.writeColor(tty, writer, name);
    try writer.writeAll(": ");
    try writer.writeAll(std.mem.trim(u8, content, "\n\r \t"));
}

const RunnerErrorEntry = struct {
    name: []const u8,
    err: []const u8,
};

const RunnerTiming = struct {
    name: []const u8,
    time: i64,
};

const RunnerCapturedOutput = struct {
    content: []const u8,
    displayed: bool,
};

const RunnerLogCapturedOutput = struct {
    lines: []const []const u8,
    displayed: bool,
};

pub fn main() !void {
    if (builtin.test_functions.len == 0) {
        return;
    }

    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    const ttyConfig = std.io.tty.detectConfig(stdout);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const fake_fds = .{
        .stdout = try std.posix.memfd_create("fake_stdout", 0),
        .stderr = try std.posix.memfd_create("fake_stderr", 0),
    };

    var outputs = .{
        .stdout = std.StringHashMap(RunnerCapturedOutput).init(allocator),
        .stderr = std.StringHashMap(RunnerCapturedOutput).init(allocator),
    };

    logCaptureContext.allocator = allocator;
    logCaptureContext.captured = std.ArrayList([]const u8).init(allocator);
    var logOutputs = std.StringHashMap(RunnerLogCapturedOutput).init(allocator);

    var failures = std.ArrayList(RunnerErrorEntry).init(allocator);

    var slowest = RunnerTiming{
        .name = "null",
        .time = 0,
    };

    try writer.writeAll("=== TESTING SESSION BEGINS ===\n");

    var total_tests: usize = 0;
    var total_failures: usize = 0;
    for (builtin.test_functions) |t| {
        std.testing.allocator_instance = .{};

        total_tests += 1;
        current_stdout = fake_fds.stdout;
        current_stderr = fake_fds.stderr;
        const start = std.time.milliTimestamp();
        const result = t.func();
        const elapsed = std.time.milliTimestamp() - start;
        current_stderr = std.posix.STDERR_FILENO;
        current_stdout = std.posix.STDOUT_FILENO;

        inline for (@typeInfo(@TypeOf(outputs)).Struct.fields) |field| {
            const fd = @field(fake_fds, field.name);
            const buffNullable = try extractCapturedOutput(fd, allocator);
            if (buffNullable) |buff| {
                try @field(outputs, field.name).put(t.name, .{
                    .content = buff,
                    .displayed = false,
                });
            }
        }

        if (logCaptureContext.captured.?.items.len > 0) {
            try logOutputs.put(t.name, .{
                .lines = try logCaptureContext.captured.?.toOwnedSlice(),
                .displayed = false,
            });
            logCaptureContext.captured.?.clearRetainingCapacity();
        }

        if (slowest.time < elapsed) {
            slowest.name = t.name;
            slowest.time = elapsed;
        }

        var errorsHappened = false;

        if (std.testing.allocator_instance.deinit() == .leak) {
            errorsHappened = true;
            total_failures += 1;
            try failures.append(RunnerErrorEntry{
                .name = t.name,
                .err = "MemoryLeak",
            });
        }

        result catch |err| {
            errorsHappened = true;
            total_failures += 1;
            try failures.append(RunnerErrorEntry{
                .name = t.name,
                .err = try std.fmt.allocPrint(allocator, "{}", .{err}),
            });
        };

        if (errorsHappened) {
            try errorOutput.writeColor(ttyConfig, writer, "X");
        } else {
            try writer.writeByte('.');
        }
    }
    try writer.writeByte('\n');

    if (failures.items.len > 0) {
        try errorOutput.writeColor(ttyConfig, writer, "\nFAILED TESTS");

        for (failures.items) |failed_test| {
            try writer.writeAll("\n  → ");
            try importantOutput.writeColor(ttyConfig, writer, failed_test.name);
            try writer.writeAll(": ");
            try errorOutput.writeColor(ttyConfig, writer, failed_test.err);

            inline for (@typeInfo(@TypeOf(outputs)).Struct.fields) |field| {
                const outputNullable = @field(outputs, field.name).getEntry(failed_test.name);
                if (outputNullable) |output| {
                    try displayCapturedOutputLine(ttyConfig, writer, field.name, output.value_ptr.content);
                    output.value_ptr.displayed = true;
                }
            }
            const outputNullable = logOutputs.getEntry(failed_test.name);
            if (outputNullable) |output| {
                for (output.value_ptr.lines) |line| {
                    try displayCapturedOutputLine(ttyConfig, writer, "testutil", line);
                }
                output.value_ptr.displayed = true;
            }
        }
        try writer.writeByte('\n');
    }

    var additional_output = false;
    inline for (@typeInfo(@TypeOf(outputs)).Struct.fields) |field| {
        var output_iter = @field(outputs, field.name).iterator();
        while (output_iter.next()) |output| {
            if (!output.value_ptr.displayed) {
                additional_output = true;
                break;
            }
        }
        if (additional_output) {
            break;
        }
    }
    var logOutputs_iter = logOutputs.iterator();
    while (logOutputs_iter.next()) |output| {
        if (!output.value_ptr.displayed) {
            additional_output = true;
            break;
        }
    }

    if (additional_output) {
        try errorOutput.writeColor(ttyConfig, writer, "\nCAPTURED OUTPUT");
        inline for (@typeInfo(@TypeOf(outputs)).Struct.fields) |field| {
            var output_iter = @field(outputs, field.name).iterator();
            while (output_iter.next()) |output| {
                if (output.value_ptr.displayed) {
                    continue;
                }
                try writer.writeAll("\n  → ");
                try importantOutput.writeColor(ttyConfig, writer, output.key_ptr.*);
                try displayCapturedOutputLine(ttyConfig, writer, field.name, output.value_ptr.content);
            }
        }
        var logOutput_iter = logOutputs.iterator();
        while (logOutput_iter.next()) |output| {
            if (output.value_ptr.displayed) {
                continue;
            }
            try writer.writeAll("\n  → ");
            try importantOutput.writeColor(ttyConfig, writer, output.key_ptr.*);
            for (output.value_ptr.lines) |line| {
                try displayCapturedOutputLine(ttyConfig, writer, "testutil", line);
            }
        }
        try writer.writeByte('\n');
    }

    try importantOutput.writeColor(ttyConfig, writer, "\nSUMMARY");

    try writer.writeAll("\n  → ");
    try importantOutput.beginColor(ttyConfig, writer);
    try std.fmt.format(writer, "{d}", .{total_tests});
    try endColor(ttyConfig, writer);
    try writer.writeAll(" tests, ");
    if (total_failures == 0) {
        try goodOutput.beginColor(ttyConfig, writer);
    } else {
        try errorOutput.beginColor(ttyConfig, writer);
    }
    try std.fmt.format(writer, "{d}", .{total_failures});
    try endColor(ttyConfig, writer);
    try writer.writeAll(" failures");

    try writer.writeAll("\n  → Slowest test: ");
    try importantOutput.writeColor(ttyConfig, writer, slowest.name);
    try std.fmt.format(writer, " ({d}ms)", .{slowest.time});
    try writer.writeAll("\n=== TESTING SESSION ENDS ===\n");
}
