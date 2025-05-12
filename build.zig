const std = @import("std");

pub fn build(b: *std.Build) void {
    // Source paths
    const exe_source = "src/main.zig";
    const lib_source = "src/lib.zig";
    const test_source = "test/test.zig";

    // Compiler options passed in
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Lib contains the emulator functionality itself
    const lib = b.addStaticLibrary(.{
        .name = "gbemu",
        .root_source_file = b.path(lib_source),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);
    const lib_module = b.addModule("lib", .{
        .root_source_file = b.path(lib_source),
    });

    // Exe is the emulator executable
    const exe = b.addExecutable(.{
        .name = "gbemu",
        .root_source_file = b.path(exe_source),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("lib", lib_module); // emulator functionality
    b.installArtifact(exe);

    // We add a run artifact so that the emulator can be run
    const run_exe = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_exe.addArgs(args);
    }

    // Unit tests for lib are included here
    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path(lib_source),
        .target = target,
        .optimize = optimize,
    });
    lib_unit_tests.root_module.addImport("lib", lib_module); // emulator functionality

    // We add a run artifact so that the unit tests can be run
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // Extra stuff to test outside the functional code (lib)
    const extended_unit_tests = b.addTest(.{
        .root_source_file = b.path(test_source),
        .target = target,
        .optimize = optimize,
    });
    extended_unit_tests.root_module.addImport("lib", lib_module); // emulator functionality

    // We add a run artifact so that the unit tests can be run
    const run_extended_unit_tests = b.addRunArtifact(extended_unit_tests);

    // Create a run step and make it run the emulator
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_exe.step);

    // Create a test step and make it run the unit tests
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_extended_unit_tests.step);
}
