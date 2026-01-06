const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ARIUM

    const arium_mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const arium_lib = b.addLibrary(.{
        .linkage = .static,
        .name = "arium",
        .root_module = arium_mod,
    });

    b.installArtifact(arium_lib);

    // EXE

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("cli/cli.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_mod.addImport("arium", arium_mod);

    const clap_dep = b.dependency("clap", .{
        .target = target,
        .optimize = optimize,
    });

    exe_mod.addImport("clap", clap_dep.module("clap"));

    const exe = b.addExecutable(.{
        .name = "arium",
        .root_module = exe_mod,
    });

    b.installArtifact(exe);

    // RUN CMD

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the CLI");
    run_step.dependOn(&run_cmd.step);

    // LANG TEST CMD

    const lang_tests = b.addExecutable(.{
        .name = "lang-test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/lang_test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const lang_tests_cmd = b.addRunArtifact(lang_tests);
    const lang_tests_step = b.step("lang_test", "Run language tests");

    lang_tests_step.dependOn(&lang_tests_cmd.step);

    lang_tests.root_module.addImport("arium", arium_mod);

    // TEST CMD

    const lib_unit_tests = b.addTest(.{
        .root_module = arium_mod,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const lang_tests_unit_tests = b.addTest(.{
        .root_module = lang_tests.root_module,
    });

    const run_lang_tests_unit_tests = b.addRunArtifact(lang_tests_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_lang_tests_unit_tests.step);

    // CHECK CMD

    const check_step = b.step("check", "Check build");

    // CHECK CLI

    const check_cli_mod = b.addExecutable(.{
        .name = "check_cli",
        .root_module = b.createModule(.{
            .root_source_file = b.path("cli/cli.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    check_step.dependOn(&check_cli_mod.step);

    check_cli_mod.root_module.addImport("arium", arium_mod);
    check_cli_mod.root_module.addImport("clap", clap_dep.module("clap"));

    // CHECK LANG TEST

    const check_lang_test_mod = b.addExecutable(.{
        .name = "check_lang_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/lang_test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    check_step.dependOn(&check_lang_test_mod.step);

    check_lang_test_mod.root_module.addImport("arium", arium_mod);
}
