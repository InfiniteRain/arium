const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // SHARED

    const shared_mod = b.createModule(.{
        .root_source_file = b.path("shared/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const shared_lib = b.addLibrary(.{
        .linkage = .static,
        .name = "shared",
        .root_module = shared_mod,
    });

    b.installArtifact(shared_lib);

    // ARIUM

    const arium_mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    arium_mod.addImport("shared", shared_mod);

    const arium_lib = b.addLibrary(.{
        .linkage = .static,
        .name = "arium",
        .root_module = arium_mod,
    });

    b.installArtifact(arium_lib);

    // EXE

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_mod.addImport("arium", arium_mod);
    exe_mod.addImport("shared", shared_mod);

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

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // LANG TEST CMD

    const lang_tests = b.addExecutable(.{
        .name = "lang-test",
        .root_source_file = b.path("test/lang_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lang_tests_cmd = b.addRunArtifact(lang_tests);
    const lang_tests_step = b.step("lang_test", "Run language tests");

    lang_tests_step.dependOn(&lang_tests_cmd.step);

    lang_tests.root_module.addImport("arium", arium_mod);
    lang_tests.root_module.addImport("shared", shared_mod);

    // TEST CMD

    const lib_unit_tests = b.addTest(.{
        .root_module = arium_mod,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);

    // CHECK CMD

    const check = b.addExecutable(.{
        .name = "check",
        // Check will try and build lang tests, as that
        // module depends on both arium and shared, and therefore
        // will build those too (and check them).
        .root_source_file = b.path("test/lang_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const check_step = b.step("check", "Check build");
    check_step.dependOn(&check.step);

    check.root_module.addImport("shared", shared_mod);
    check.root_module.addImport("arium", arium_mod);

    // THIRD PARTY DEPENDENCIES

    const dep_names = [_][]const u8{"clap"};
    const mods = [_]*std.Build.Module{
        arium_mod,
        exe_mod,
        shared_mod,
        lang_tests.root_module,
        check.root_module,
    };

    for (dep_names) |dep_name| {
        const dep = b.dependency(dep_name, .{
            .target = target,
            .optimize = optimize,
        });

        for (mods) |mod| {
            mod.addImport(dep_name, dep.module(dep_name));
        }
    }
}
