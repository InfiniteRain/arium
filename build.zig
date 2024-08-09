const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // SHARED LIBRARY

    const shared_lib = b.addStaticLibrary(.{
        .name = "shared",
        .root_source_file = b.path("shared/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(shared_lib);

    // LIBRARY

    const lib = b.addStaticLibrary(.{
        .name = "arium",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    // EXECUTABLE

    const exe = b.addExecutable(.{
        .name = "arium",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    // RUN COMMAND

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // LIBRARY UNIT TESTS

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // EXECUTABLE UNIT TESTS

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // TEST STEP

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);

    // LSP CHECK

    const lsp_check = b.addExecutable(.{
        .name = "arium",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lsp_check_step = b.step("check", "Check if Arium compiles");
    lsp_check_step.dependOn(&lsp_check.step);

    // LANGUAGE TESTS

    const lang_tests = b.addExecutable(.{
        .name = "arium-lang-tests",
        .root_source_file = b.path("test/lang_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lang_tests_cmd = b.addRunArtifact(lang_tests);
    const run_lang_tests_step = b.step("lang-test", "Run language tests");
    run_lang_tests_step.dependOn(&lang_tests_cmd.step);

    // FIRST PARTY DEPENDENCIES

    lang_tests.root_module.addImport("arium", &lib.root_module);

    lib.root_module.addImport("shared", &shared_lib.root_module);
    exe.root_module.addImport("shared", &shared_lib.root_module);
    lib_unit_tests.root_module.addImport("shared", &shared_lib.root_module);
    exe_unit_tests.root_module.addImport("shared", &shared_lib.root_module);
    lsp_check.root_module.addImport("shared", &shared_lib.root_module);
    lang_tests.root_module.addImport("shared", &shared_lib.root_module);

    // THIRD PARTY DEPENDENCIES

    const dep_strings = [_][]const u8{"clap"};

    for (dep_strings) |dep_string| {
        const dep = b.dependency(dep_string, .{});

        shared_lib.root_module.addImport(dep_string, dep.module(dep_string));
        exe.root_module.addImport(dep_string, dep.module(dep_string));
        lsp_check.root_module.addImport(dep_string, dep.module(dep_string));
        lib.root_module.addImport(dep_string, dep.module(dep_string));
        lib_unit_tests.root_module.addImport(dep_string, dep.module(dep_string));
        exe_unit_tests.root_module.addImport(dep_string, dep.module(dep_string));
        lang_tests.root_module.addImport(dep_string, dep.module(dep_string));
    }
}
