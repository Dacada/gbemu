const std = @import("std");

const Options = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,

    fn init(b: *std.Build) Options {
        return .{
            .target = b.standardTargetOptions(.{}),
            .optimize = b.standardOptimizeOption(.{}),
        };
    }
};

const Sources = struct {
    exe: std.Build.LazyPath,
    lib: std.Build.LazyPath,
    integ_tests: std.Build.LazyPath,
    rom_tester: std.Build.LazyPath,
    test_runner: std.Build.LazyPath,

    fn init(b: *std.Build) Sources {
        return .{
            .exe = b.path("src/main.zig"),
            .lib = b.path("src/lib.zig"),
            .integ_tests = b.path("test/test.zig"),
            .rom_tester = b.path("test/rom_tester.zig"),
            .test_runner = b.path("test/runner.zig"),
        };
    }
};

const Modules = struct {
    // Lib contains the emulator functionality itself
    lib: *std.Build.Module,

    // Exe is the emulator itself
    exe: *std.Build.Module,

    // Rom tester is an executable that orchestrates testing various ROMs
    rom_tester: *std.Build.Module,

    // Test runner, used for running all unit tests
    test_runner: std.Build.Step.Compile.TestRunner,

    // Integ tests for lib
    integ_tests: *std.Build.Module,

    fn init(b: *std.Build, src: Sources, opt: Options) Modules {
        var res = Modules{
            .lib = b.addModule("lib", .{
                .root_source_file = src.lib,
                .target = opt.target,
                .optimize = opt.optimize,
            }),
            .exe = b.addModule("exe", .{
                .root_source_file = src.exe,
                .target = opt.target,
                .optimize = opt.optimize,
            }),
            .rom_tester = b.addModule("rom-tester", .{
                .root_source_file = src.rom_tester,
                .target = opt.target,
                .optimize = opt.optimize,
            }),
            .test_runner = .{
                .mode = .simple,
                .path = src.test_runner,
            },
            .integ_tests = b.addModule("integ", .{
                .root_source_file = src.integ_tests,
                .target = opt.target,
                .optimize = opt.optimize,
            }),
        };

        // add the emulator functionality to the other modules
        res.exe.addImport("lib", res.lib);
        res.rom_tester.addImport("lib", res.lib);
        res.integ_tests.addImport("lib", res.lib);

        return res;
    }
};

const CompileSteps = struct {
    lib: *std.Build.Step.Compile,
    lib_tests: *std.Build.Step.Compile,
    integ_tests: *std.Build.Step.Compile,

    exe: *std.Build.Step.Compile,
    exe_tests: *std.Build.Step.Compile,

    rom_tester: *std.Build.Step.Compile,

    fn init(b: *std.Build, mods: Modules) CompileSteps {
        var res = CompileSteps{
            .lib = b.addLibrary(.{
                .name = "gbemu",
                .root_module = mods.lib,
            }),
            .lib_tests = b.addTest(.{
                .root_module = mods.lib,
                .test_runner = mods.test_runner,
            }),
            .integ_tests = b.addTest(.{
                .root_module = mods.integ_tests,
                .test_runner = mods.test_runner,
            }),

            .exe = b.addExecutable(.{
                .name = "gbemu",
                .root_module = mods.exe,
            }),
            .exe_tests = b.addTest(.{
                .root_module = mods.exe,
                .test_runner = mods.test_runner,
            }),

            .rom_tester = b.addExecutable(.{
                .name = "rom-tester",
                .root_module = mods.rom_tester,
            }),
        };

        // Need this for the debugger cli
        res.exe.linkLibC();
        res.rom_tester.linkLibC();
        res.exe.linkSystemLibrary("readline");
        res.rom_tester.linkSystemLibrary("readline");

        b.installArtifact(res.lib);
        b.installArtifact(res.exe);
        b.installArtifact(res.rom_tester);

        return res;
    }
};

const Executable = struct {
    exe: *std.Build.Step.Run,
    lib_tests: *std.Build.Step.Run,
    exe_tests: *std.Build.Step.Run,
    integ_tests: *std.Build.Step.Run,
    rom_tester: *std.Build.Step.Run,

    fn init(b: *std.Build, comp: CompileSteps) Executable {
        return .{
            .exe = b.addRunArtifact(comp.exe),
            .lib_tests = b.addRunArtifact(comp.lib_tests),
            .exe_tests = b.addRunArtifact(comp.exe_tests),
            .integ_tests = b.addRunArtifact(comp.integ_tests),
            .rom_tester = b.addRunArtifact(comp.rom_tester),
        };
    }
};

fn createCmdArgs(b: *std.Build, executable: Executable) void {
    // Allow to pass parameters to the emulator executable
    if (b.args) |args| {
        executable.exe.addArgs(args);
    }

    // Create a bunch of cmd args
    const run_step = b.step("run", "Run the emulator");
    run_step.dependOn(&executable.exe.step);

    var test_lib_step = b.step("test-lib", "Run library unit tests");
    test_lib_step.dependOn(&executable.lib_tests.step);

    var test_exe_step = b.step("test-exe", "Run executable unit tests");
    test_exe_step.dependOn(&executable.exe_tests.step);

    var test_integ_step = b.step("test-integ", "Run integration tests");
    test_integ_step.dependOn(&executable.integ_tests.step);

    var test_roms_step = b.step("test-roms", "Run test roms");
    test_roms_step.dependOn(&executable.rom_tester.step);

    const test_all_step = b.step("test-all", "Run all tests except for roms");
    test_all_step.dependOn(test_lib_step);
    test_all_step.dependOn(test_exe_step);
    test_all_step.dependOn(test_integ_step);
}

pub fn build(b: *std.Build) void {
    const options = Options.init(b);
    const sources = Sources.init(b);
    const modules = Modules.init(b, sources, options);
    const compile = CompileSteps.init(b, modules);

    const executable = Executable.init(b, compile);

    createCmdArgs(b, executable);
}
