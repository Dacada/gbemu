const std = @import("std");

pub fn build(b: *std.Build) void {
    // Source paths
    const exe_source = b.path("src/main.zig");
    const lib_source = b.path("src/lib.zig");
    const test_source = b.path("test/test.zig");
    const rom_tester_source = b.path("test/rom_tester.zig");
    const test_runner_source = b.path("test/runner.zig");

    // Compiler options passed in
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Lib contains the emulator functionality itself
    const lib = b.addStaticLibrary(.{
        .name = "gbemu",
        .root_source_file = lib_source,
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);
    const lib_module = b.addModule("lib", .{
        .root_source_file = lib_source,
    });

    // Exe is the emulator executable
    const exe = b.addExecutable(.{
        .name = "gbemu",
        .root_source_file = exe_source,
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibC();
    exe.linkSystemLibrary("readline");
    exe.root_module.addImport("lib", lib_module); // emulator functionality
    b.installArtifact(exe);

    // We add a run artifact so that the emulator can be run
    const run_exe = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_exe.addArgs(args);
    }

    // Rom tester is an executable that orchestrates testing various ROMs
    const rom_tester = b.addExecutable(.{
        .name = "rom-tester",
        .root_source_file = rom_tester_source,
        .target = target,
        .optimize = optimize,
    });
    rom_tester.linkLibC();
    rom_tester.linkSystemLibrary("readline");
    rom_tester.root_module.addImport("lib", lib_module);
    b.installArtifact(rom_tester);

    // A run artifact to run it as part of testing
    const run_rom_tester = b.addRunArtifact(rom_tester);

    // Unit tests for lib are included here
    const lib_unit_tests = b.addTest(.{
        .root_source_file = lib_source,
        .test_runner = .{
            .mode = .simple,
            .path = test_runner_source,
        },
        .target = target,
        .optimize = optimize,
    });
    lib_unit_tests.root_module.addImport("lib", lib_module); // emulator functionality

    // We add a run artifact so that lib unit tests can be run
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // Extra stuff to test outside the functional code (lib)
    const extended_unit_tests = b.addTest(.{
        .root_source_file = test_source,
        .test_runner = .{
            .mode = .simple,
            .path = test_runner_source,
        },
        .target = target,
        .optimize = optimize,
    });
    extended_unit_tests.root_module.addImport("lib", lib_module); // emulator functionality

    // We add a run artifact so that the extended tests can be run
    const run_extended_unit_tests = b.addRunArtifact(extended_unit_tests);

    // Unit tests for exe are included here
    const exe_unit_tests = b.addTest(.{
        .root_source_file = exe_source,
        .test_runner = .{
            .mode = .simple,
            .path = test_runner_source,
        },
        .target = target,
        .optimize = optimize,
    });
    exe_unit_tests.root_module.addImport("lib", lib_module); // emulator functionality

    // We add a run artifact so that exe unit tests can be run
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Create a run step and make it run the emulator
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_exe.step);

    // Create test steps and make them run unit tests
    const test_all_step = b.step("test-all", "Run all unit tests");
    test_all_step.dependOn(&run_lib_unit_tests.step);
    test_all_step.dependOn(&run_extended_unit_tests.step);
    test_all_step.dependOn(&run_exe_unit_tests.step);
    test_all_step.dependOn(&run_rom_tester.step);

    const test_lib_step = b.step("test-lib", "Run library unit tests");
    test_lib_step.dependOn(&run_lib_unit_tests.step);

    const test_exe_step = b.step("test-exe", "Run executable unit tests");
    test_exe_step.dependOn(&run_exe_unit_tests.step);

    const test_integ_step = b.step("test-integ", "Run integration tests");
    test_integ_step.dependOn(&run_extended_unit_tests.step);

    const test_roms_step = b.step("test-roms", "Run tests for roms");
    test_roms_step.dependOn(&run_rom_tester.step);
}
