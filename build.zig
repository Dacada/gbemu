const std = @import("std");

pub fn build(b: *std.Build) void {
    const lib_source = "src/lib.zig";
    const test_source = "test/test.zig";

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

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

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path(lib_source),
        .target = target,
        .optimize = optimize,
    });
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const extended_unit_tests = b.addTest(.{
        .root_source_file = b.path(test_source),
        .target = target,
        .optimize = optimize,
    });
    extended_unit_tests.root_module.addImport("lib", lib_module);
    const run_extended_unit_tests = b.addRunArtifact(extended_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_extended_unit_tests.step);
}
