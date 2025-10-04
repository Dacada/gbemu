const std = @import("std");
const builtin = @import("builtin");

// Ignore output sent through the logging system, except for the testutil scope, which we will display as captured output
const log_capture_context = struct {
    var allocator: ?std.mem.Allocator = null;
    var captured: ?std.ArrayList([]const u8) = null;
};
fn reportingLogFn(comptime _: std.log.Level, comptime scope: @TypeOf(.enum_literal), comptime format: []const u8, args: anytype) void {
    if (scope != .testutil) {
        return;
    }

    if (log_capture_context.allocator == null or log_capture_context.captured == null) {
        return;
    }

    const buffer = std.fmt.allocPrint(log_capture_context.allocator.?, format, args) catch {
        return;
    };

    log_capture_context.captured.?.append(log_capture_context.allocator.?, buffer) catch {
        return;
    };
}
pub const std_options: std.Options = .{
    .logFn = reportingLogFn,
};

// Capture stderr/stdout output
var fake_stderr: ?std.posix.fd_t = null;
var fake_stdout: ?std.posix.fd_t = null;
var real_stderr: ?std.posix.fd_t = null;
var real_stdout: ?std.posix.fd_t = null;
fn initStdStreamCapture() !void {
    fake_stdout = try std.posix.memfd_create("fake_stdout", 0);
    fake_stderr = try std.posix.memfd_create("fake_stderr", 0);
    real_stdout = try std.posix.dup(std.posix.STDOUT_FILENO);
    real_stderr = try std.posix.dup(std.posix.STDERR_FILENO);
}
fn maskStd() !void {
    if (fake_stderr) |fd| {
        try std.posix.dup2(fd, std.posix.STDERR_FILENO);
    }
    if (fake_stdout) |fd| {
        try std.posix.dup2(fd, std.posix.STDOUT_FILENO);
    }
}
fn restoreStd() !void {
    if (real_stderr) |fd| {
        try std.posix.dup2(fd, std.posix.STDERR_FILENO);
    }
    if (real_stdout) |fd| {
        try std.posix.dup2(fd, std.posix.STDOUT_FILENO);
    }
}

// Panic handling
var panicking = false;
fn testRunnerPanic(msg: []const u8, first_trace_addr: ?usize) noreturn {
    if (panicking) {
        std.posix.abort();
    }
    panicking = true;

    restoreStd() catch {};

    var stdout_buffer: [1024]u8 = undefined;
    const stdout_fileno = std.fs.File.stdout();
    var stdout_file_writer = stdout_fileno.writer(&stdout_buffer);
    const tty_config = std.Io.tty.detectConfig(stdout_fileno);
    const writer = &stdout_file_writer.interface;

    ErrorOutput.writeColor(tty_config, writer, "PANIC!\n") catch {};

    if (current_test) |t| {
        writer.writeAll("  → in: ") catch {};
        ImportantOutput.beginColor(tty_config, writer) catch {};
        writer.print("{s}\n", .{t.name}) catch {};
        endColor(tty_config, writer) catch {};
        writer.writeAll("    ↪ msg: ") catch {};
    } else {
        writer.writeAll("  → msg: ") catch {};
    }

    ImportantOutput.beginColor(tty_config, writer) catch {};
    writer.print("{s}\n", .{msg}) catch {};
    endColor(tty_config, writer) catch {};

    writer.writeAll("\n  → captured output: ") catch {};
    if (fake_stdout) |fd| {
        const captured_stdout = extractCapturedOutput(fd, log_capture_context.allocator.?) catch null;
        if (captured_stdout) |cap| {
            displayCapturedOutputLine(tty_config, writer, "stdout", cap) catch {};
        }
    }
    if (fake_stderr) |fd| {
        const captured_stderr = extractCapturedOutput(fd, log_capture_context.allocator.?) catch null;
        if (captured_stderr) |cap| {
            displayCapturedOutputLine(tty_config, writer, "stderr", cap) catch {};
        }
    }
    for (log_capture_context.captured.?.items) |line| {
        displayCapturedOutputLine(tty_config, writer, "log call", line) catch {};
    }

    writer.writeAll("\nStack trace (stderr):\n") catch {};
    // if (@errorReturnTrace()) |t| {
    //     std.debug.dumpStackTrace(t.*);
    // }
    std.debug.dumpCurrentStackTrace(first_trace_addr orelse @returnAddress());

    writer.flush() catch {};
    std.posix.abort();
}
pub const panic = std.debug.FullPanic(testRunnerPanic);

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

fn MakeColorOutputFunctions(comptime color: std.io.tty.Color) type {
    return struct {
        fn beginColor(tty: std.io.tty.Config, writer: *std.Io.Writer) !void {
            try tty.setColor(writer, color);
        }

        fn writeColor(tty: std.io.tty.Config, writer: *std.Io.Writer, bytes: []const u8) !void {
            try tty.setColor(writer, color);
            try writer.writeAll(bytes);
            try tty.setColor(writer, .reset);
        }
    };
}

fn endColor(tty: std.io.tty.Config, writer: *std.Io.Writer) !void {
    try tty.setColor(writer, .reset);
}

const GoodOutput = MakeColorOutputFunctions(std.io.tty.Color.green);
const ErrorOutput = MakeColorOutputFunctions(std.io.tty.Color.red);
const ImportantOutput = MakeColorOutputFunctions(std.io.tty.Color.blue);

fn displayCapturedOutputLine(tty: std.io.tty.Config, writer: *std.Io.Writer, name: []const u8, content: []const u8) !void {
    try writer.writeAll("\n    ↪ ");
    try ImportantOutput.writeColor(tty, writer, name);
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

pub fn main() void {
    innerMain() catch @panic("test runner error");
}

var current_test: ?*const std.builtin.TestFn = null;

pub fn innerMain() !void {
    if (builtin.test_functions.len == 0) {
        return;
    }

    try initStdStreamCapture();

    var stdout_buffer: [1024]u8 = undefined;
    const stdout_fileno = std.fs.File.stdout();
    var stdout_file_writer = stdout_fileno.writer(&stdout_buffer);
    const tty_config = std.Io.tty.detectConfig(stdout_fileno);
    const writer = &stdout_file_writer.interface;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const fake_fds = .{
        .stdout = fake_stdout,
        .stderr = fake_stderr,
    };

    var outputs = .{
        .stdout = std.StringHashMap(RunnerCapturedOutput).init(allocator),
        .stderr = std.StringHashMap(RunnerCapturedOutput).init(allocator),
    };

    log_capture_context.allocator = allocator;
    log_capture_context.captured = try std.ArrayList([]const u8).initCapacity(allocator, 64);
    var log_outputs = std.StringHashMap(RunnerLogCapturedOutput).init(allocator);

    var failures = try std.ArrayList(RunnerErrorEntry).initCapacity(allocator, 8);

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
        try writer.flush();
        try maskStd();
        const start = std.time.milliTimestamp();
        current_test = &t;
        const result = t.func();
        current_test = null;
        const elapsed = std.time.milliTimestamp() - start;
        try restoreStd();

        inline for (@typeInfo(@TypeOf(outputs)).@"struct".fields) |field| {
            const fd = @field(fake_fds, field.name);
            const buff_nullable = if (fd) |f| try extractCapturedOutput(f, allocator) else null;
            if (buff_nullable) |buff| {
                try @field(outputs, field.name).put(t.name, .{
                    .content = buff,
                    .displayed = false,
                });
            }
        }

        if (log_capture_context.captured.?.items.len > 0) {
            try log_outputs.put(t.name, .{
                .lines = try log_capture_context.captured.?.toOwnedSlice(allocator),
                .displayed = false,
            });
            log_capture_context.captured.?.clearRetainingCapacity();
        }

        if (slowest.time < elapsed) {
            slowest.name = t.name;
            slowest.time = elapsed;
        }

        var errors_happened = false;

        if (std.testing.allocator_instance.deinit() == .leak) {
            errors_happened = true;
            total_failures += 1;
            try failures.append(
                allocator,
                RunnerErrorEntry{
                    .name = t.name,
                    .err = "MemoryLeak",
                },
            );
        }

        result catch |err| {
            errors_happened = true;
            total_failures += 1;
            try failures.append(
                allocator,
                RunnerErrorEntry{
                    .name = t.name,
                    .err = try std.fmt.allocPrint(allocator, "{}", .{err}),
                },
            );
        };

        if (errors_happened) {
            try ErrorOutput.writeColor(tty_config, writer, "X");
        } else {
            try writer.writeByte('.');
        }
    }
    try writer.writeByte('\n');

    if (failures.items.len > 0) {
        try ErrorOutput.writeColor(tty_config, writer, "\nFAILED TESTS");

        for (failures.items) |failed_test| {
            try writer.writeAll("\n  → ");
            try ImportantOutput.writeColor(tty_config, writer, failed_test.name);
            try writer.writeAll(": ");
            try ErrorOutput.writeColor(tty_config, writer, failed_test.err);

            inline for (@typeInfo(@TypeOf(outputs)).@"struct".fields) |field| {
                const output_nullable = @field(outputs, field.name).getEntry(failed_test.name);
                if (output_nullable) |output| {
                    try displayCapturedOutputLine(tty_config, writer, field.name, output.value_ptr.content);
                    output.value_ptr.displayed = true;
                }
            }
            const output_nullable = log_outputs.getEntry(failed_test.name);
            if (output_nullable) |output| {
                for (output.value_ptr.lines) |line| {
                    try displayCapturedOutputLine(tty_config, writer, "testutil", line);
                }
                output.value_ptr.displayed = true;
            }
        }
        try writer.writeByte('\n');
    }

    var additional_output = false;
    inline for (@typeInfo(@TypeOf(outputs)).@"struct".fields) |field| {
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
    var log_outputs_iter = log_outputs.iterator();
    while (log_outputs_iter.next()) |output| {
        if (!output.value_ptr.displayed) {
            additional_output = true;
            break;
        }
    }

    if (additional_output) {
        try ErrorOutput.writeColor(tty_config, writer, "\nCAPTURED OUTPUT");
        inline for (@typeInfo(@TypeOf(outputs)).@"struct".fields) |field| {
            var output_iter = @field(outputs, field.name).iterator();
            while (output_iter.next()) |output| {
                if (output.value_ptr.displayed) {
                    continue;
                }
                try writer.writeAll("\n  → ");
                try ImportantOutput.writeColor(tty_config, writer, output.key_ptr.*);
                try displayCapturedOutputLine(tty_config, writer, field.name, output.value_ptr.content);
            }
        }
        var log_output_iter = log_outputs.iterator();
        while (log_output_iter.next()) |output| {
            if (output.value_ptr.displayed) {
                continue;
            }
            try writer.writeAll("\n  → ");
            try ImportantOutput.writeColor(tty_config, writer, output.key_ptr.*);
            for (output.value_ptr.lines) |line| {
                try displayCapturedOutputLine(tty_config, writer, "testutil", line);
            }
        }
        try writer.writeByte('\n');
    }

    try ImportantOutput.writeColor(tty_config, writer, "\nSUMMARY");

    try writer.writeAll("\n  → ");
    try ImportantOutput.beginColor(tty_config, writer);
    try writer.print("{d}", .{total_tests});
    try endColor(tty_config, writer);
    try writer.writeAll(" tests, ");
    if (total_failures == 0) {
        try GoodOutput.beginColor(tty_config, writer);
    } else {
        try ErrorOutput.beginColor(tty_config, writer);
    }
    try writer.print("{d}", .{total_failures});
    try endColor(tty_config, writer);
    try writer.writeAll(" failures");

    try writer.writeAll("\n  → Slowest test: ");
    try ImportantOutput.writeColor(tty_config, writer, slowest.name);
    try writer.print(" ({d}ms)", .{slowest.time});
    try writer.writeAll("\n=== TESTING SESSION ENDS ===\n");
    try writer.flush();
}
