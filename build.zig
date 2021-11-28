const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("main", "main.zig");

    const exe_opt = b.addOptions();
    exe.addOptions("build_options", exe_opt);

    // To be updated - options will not work as expected
    exe_opt.addOption(bool, "debugCmd", false);
    exe_opt.addOption(bool, "debugDisp", false);
    exe_opt.addOption(bool, "debugLoop", false);
    exe_opt.addOption(bool, "debugStart", false);
    exe_opt.addOption(bool, "debugStages", false);

   const pipes_pkg: std.build.Pkg = .{
        .name = "pipes",
        .path = .{ .path = "./pipes.zig" },
        .dependencies = &.{exe_opt.getPackage("build_options")},
    };

    const filters_pkg: std.build.Pkg = .{
        .name = "filters",
        .path = .{ .path = "./filters.zig" },
        .dependencies = &.{exe_opt.getPackage("build_options")},
        //.dependencies = &.{exe_opt.getPackage("build_options"), pipes_pkg},
    };
    
    exe.addPackage(pipes_pkg);
    exe.addPackage(filters_pkg);

    exe.setTarget(target); // best for this cpu
    //exe.setTarget(.{                                  // generic x86_64 - about 8% slower on my box
    //    .cpu_arch = .x86_64,
    //    .os_tag = .linux,
    //    .abi = .gnu,
    //    .cpu_model = .baseline,                       // .baseline encompasses more old cpus
    //}); 
    exe.setBuildMode(mode);
    //exe.pie = true;
    //exe.setBuildMode(std.builtin.Mode.ReleaseFast);     // to hard code ReleaseFast/ReleaseSafe etc
    exe.setOutputDir(".");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
