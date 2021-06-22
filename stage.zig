const std = @import("std");

const debugDisp = false;
const debugLoop = false;
const debugStages = false;
const debugCmd = false;
const debugStart = false;

// will later exploit usingnamespace to allow users to add stages to library of stages

const State = packed enum(u4) {
    peekto,
    readto,
    output,
    start,
    run,
    done,
    anyinput,
};

const stageError = error{
    endOfStream,
    noInStream,
    noOutStream,
    finished,
    active,
};

const Message = packed enum(u2) {
    ok,
    data,
    sever,
};

pub fn ConnType(comptime S: type, comptime D: type) type {
    return struct {
        pub const Conn = @This();

        src: *S = undefined, // used by output
        from: usize = 0,
        sout: usize = 0,
        dst: *S = undefined, // used by input
        to: usize = 0,
        sin: usize = 0,
        in: Message = undefined,
        data: D = undefined,

        fn set(self: *Conn, p: []S, f: usize, o: usize, t: usize, s: usize) void {
            self.from = f;
            self.src = &p[f]; // stage output
            self.sout = o;
            self.to = t;
            self.dst = &p[t]; // stage input
            self.sin = s;
            //self.in=.ok;
            //if (o==0) self.src.outC = self;
            //if (s==0) self.dst.inC = self;
        }
    };
}

pub fn PipeType(comptime T: type) type {
    return struct {
        pub const Stage = @This();
        pub const Data = T;
        pub const Conn = ConnType(Stage, Data);

        outC: ?*Conn = null,
        inC: ?*Conn = null,
        state: State = undefined,
        commit: isize = -90909090,
        frame: anyframe = undefined,
        rc: anyerror!void = undefined,
        n: []Conn = undefined,
        name: ?[]const u8 = null,
        label: ?[]const u8 = null,
        end: bool = false,
        i: usize = undefined,

        pub fn peekto(self: *Stage) !Data {
            if (debugCmd) std.log.info("peekto {*} inC {*}\n", .{ self, self.inC });
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekto {*} in {} {}\n", .{ c, c.in, c.data });
                while (c.in == .ok) {
                    //std.log.info("while suspended",.{});
                    suspend {
                        self.state = .peekto;
                        self.frame = @frame();
                    }
                }
                //std.log.info("while exited",.{});
            }
            self.state = .done;
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekto {}_{s} {} in {} {}\n", .{ self.i, self.name, c.sout, c.in, c.data });
                if (c.in == .data) {
                    return c.data;
                } else {
                    return error.endOfStream;
                }
            }
            return error.noInStream;
        }

        pub fn readto(self: *Stage) !Data {
            if (self.inC) |c| {
                while (c.in == .ok) {
                    suspend {
                        self.state = .readto;
                        self.frame = @frame();
                    }
                }
            }
            self.state = .done;
            if (self.inC) |c| {
                if (c.in == .data) {
                    c.in = .ok;
                    return c.data;
                } else {
                    return error.endOfStream;
                }
            }
            return error.noInStream;
        }

        pub fn output(self: *Stage, v: Data) !void {
            if (self.outC) |c| {
                while (c.in == .data) {
                    suspend {
                        self.state = .output;
                        self.frame = @frame();
                        if (debugCmd) std.log.info("output {}_{s} {} in {} {}\n", .{ self.i, self.name, c.sout, c.in, v });
                    }
                }
            }
            self.state = .done;
            if (self.outC) |c| {
                if (debugCmd) std.log.info("output {*} {} in {} {}\n", .{ c, c.sout, c.in, v });
                if (c.in == .ok) {
                    c.in = .data;
                    c.data = v;
                    return;
                } else {
                    return error.endOfStream;
                }
            }
            return error.noOutStream;
        }

        pub fn selectOutput(self: *Stage, o: usize) !void {
            return self.setoutC(o);
        }

        pub fn severOutput(self: *Stage) !void {
            if (self.outC) |c| {
                self.outC = null;
            } else return error.noOutStream;
        }

        pub fn severInput(self: *Stage) !void {
            if (self.inC) |c| {
                self.inC = null;
            } else return error.noOutStream;
        }

        pub fn selectInput(self: *Stage, i: usize) !void {
            return self.setinC(i);
        }

        pub fn selectAnyInput(self: *Stage) !usize {
            // std.log.info("anyin {*} inC {*}\n",.{self, self.inC});
            if (countInstreams(self) > 0) {
                // std.log.info("anyin {}", .{countInstreams(self)});
                if (self.inC) |c| {
                    if (debugCmd) std.log.info("anyin pre {}_{s} in {} {}\n", .{ self.i, self.name, c.in, c.data });
                    if (c.in == .ok) {
                        suspend {
                            self.state = .anyinput;
                            self.frame = @frame();
                        }
                    }
                }
                self.state = .done;
                if (self.inC) |c| {
                    if (debugCmd) std.log.info("anyin post {}_{s} {} in {} {}\n", .{ self.i, self.name, c.sin, c.in, c.data });
                    if (c.in == .data) {
                        return c.sin;
                    } else {
                        return error.endOfStream;
                    }
                }
            }
            return error.noInStream;
        }

        pub fn endStage(self: *Stage) void {
            for (self.n) |*c| {
                if (c.dst == self) {
                    self.inC = null;
                }
                if (c.src == self) {
                    self.outC = null;
                }
            }
            return;
        }

        fn setinC(self: *Stage, i: usize) !void {
            for (self.n) |*c| {
                if (c.dst == self and c.sin == i) {
                    self.inC = c;
                    return;
                }
            }
            self.inC = null;
            return error.noInStream;
        }

        pub fn countInstreams(self: *Stage) usize {
            var count: usize = 0;
            for (self.n) |c| {
                if (c.dst == self and (c.in == .data or c.src.outC != null))
                    count += 1;
            }
            return count;
        }

        fn setoutC(self: *Stage, o: usize) !void {
            for (self.n) |*c| {
                if (c.src == self and c.sout == o) {
                    self.outC = c;
                    return;
                }
            }
            self.outC = null;
            return error.noOutStream;
        }

        pub fn exactdiv(s: *Stage, d: Data) callconv(.Async) !void {
            defer { s.endStage(); }
            return myStages.exactdiv(Stage, Data, s, d);
        }
        pub fn console(s: *Stage) callconv(.Async) !void {
            defer { s.endStage(); }
            return myStages.console(Stage, s);
        }
        pub fn fanin(s: *Stage) callconv(.Async) !void {
            defer { s.endStage(); }
            return myStages.fanin(Stage, s);
        }
        pub fn gen(s: *Stage, d: Data) callconv(.Async) !void {
            defer { s.endStage(); }
            return myStages.gen(Stage, Data, s, d);
        }
        pub fn slice(s: *Stage, d:*[]Data) callconv(.Async) !void {
            defer { s.endStage(); }
            return myStages.slice(Stage, Data, s, d);
        }
    };
}

const myStages = struct {
    fn exactdiv(comptime S:type, comptime D:type, self: *S, d: D) !void {
        if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
        var i: @TypeOf(self.*).Data = undefined;
        while (true) {
            if (debugStages) std.log.info("div pre peek {}_{s} {*}", .{ self.i, self.name, self.outC });
            i = try self.peekto();
            //if (debugStages) std.log.info("div post peek {}_{s} {}",.{self.i, self.name, i});
            if (i % d == 0)
                try self.selectOutput(0)
            else
                self.selectOutput(1) catch |err| {
                    if (err != error.noOutStream) return err;
                };
            if (debugStages) std.log.info("div out {}_{s} {*}", .{ self.i, self.name, self.outC });
            _ = self.output(i) catch |err| {
                if (err != error.noOutStream) return err;
            };
            _ = try self.readto();
        }
    }
    
    fn slice(comptime S:type, comptime D:type, self: *S, slc:*[]D) !void {
        var input:bool = false;
        _ = self.selectInput(0) catch |err| { if (err == error.noInStream) input = true; };
        if (input) {
            for (slc.*) |d| {
                _ = try self.output(d);
            }
        } else {
            var i:u32 = 0;
            slc.len = i;
            while (true) : (i += 1) {
                const d = self.peekto() catch { break; };
                slc.len = i+1;
                slc.*[i] = d;
                _ = try self.readto();
            }
            return;
        }
    }

    fn console(comptime S:type, self: *S) !void {
        const stdout = std.io.getStdOut().writer();
        if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
        var i: @TypeOf(self.*).Data = undefined;
        while (true) {
            if (debugStages) std.log.info("con in {}_{s} {*}", .{ self.i, self.name, self.inC });
            i = try self.peekto();
            try stdout.print("{} ", .{i});
            _ = self.output(i) catch {};
            _ = try self.readto();
        }
    }

    fn fanin(comptime S:type, self: *S) !void {
        if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
        while (true) {
            _ = self.selectAnyInput() catch |err| {
                if (err == error.endOfStream) continue else return err;
            };
            if (debugStages) std.log.info("fan {}_{s} {*} {*}", .{ self.i, self.name, self.inC, self.outC });
            try self.output(try self.peekto());
            _ = try self.readto();
        }
    }

    fn gen(comptime S:type, comptime D:type, self: *S, limit: D) !void {
        if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
        var i: @TypeOf(self.*).Data = 0;
        while (i < limit) : (i += 1) {
            if (debugStages) std.log.info("gen out {}_{s} {*}", .{ self.i, self.name, self.outC });
            try self.output(i);
        }
    }
};

pub fn PipeInstance(comptime T: type, pp: anytype) type {

    // build tuple of types for stage Fn and Args
    comptime var arg_set: []const type = &[_]type{};

    inline for (pp) |stg, i| {
        comptime var flag: bool = true;
        inline for (stg) |elem| {
            switch (@typeInfo(@TypeOf(elem))) {
                .Fn, .BoundFn => {
                    if (debugStart) std.debug.print("fn {} {}\n", .{ i, elem });
                    arg_set = arg_set ++ &[_]type{std.meta.Tuple(&[_]type{ @TypeOf(elem), std.meta.ArgsTuple(@TypeOf(elem)) })};
                    flag = false;
                },
                else => {},
            }
        }
        if (flag)
            arg_set = arg_set ++ &[_]type{std.meta.Tuple(&[_]type{void})};
    }

    return struct {
        const Stage = T;
        const Conn = Stage.Conn;
        const thePipe = @This();
        const pipe = pp;

        // stages/filter of the pipe
        p: [pipe.len]Stage = [_]Stage{undefined} ** pipe.len,

        // connection nodes
        nodes: [pipe.len]Conn = [_]Stage.Conn{undefined} ** pipe.len,

        pub fn args(self: *thePipe, context: anytype) std.meta.Tuple(arg_set) {
            var tuple: std.meta.Tuple(arg_set) = undefined;

            // fill in the tuple
            inline for (pipe) |stg, i| {
                comptime var flag: bool = true;
                inline for (stg) |elem, j| {
                    switch (@typeInfo(@TypeOf(elem))) {
                        .Fn, .BoundFn => { // fn....
                            tuple[i][0] = elem;
                            tuple[i][1][0] = &self.p[i];
                            flag = false;
                        },
                        .Struct => { // struct....
                            inline for (elem) |arg, k| {
                                switch (@typeInfo(@TypeOf(arg))) {
                                    .Int, .ComptimeInt => {
                                        if (debugStart) std.debug.print("Int {} {}\n", .{ j, arg });
                                        tuple[i][1][k + 1] = arg;
                                    },
                                    .Pointer => {
                                        if (debugStart) std.debug.print("Ptr {} {s}\n", .{ j, arg });
                                            if (arg[0] == '&') {
                                                comptime {
                                                    tuple[i][1][k + 1] = &@field(context, arg[1..]);
                                                }
                                            } else {
                                                comptime {
                                                    tuple[i][1][k + 1] = @field(context, arg);
                                                }
                                        }
                                    },
                                    // more types will be needed for depending on additional stages
                                    else => {
                                        if (debugStart) std.debug.print("else {} {}\n", .{ j, @typeInfo(@TypeOf(arg)) });
                                        unreachable;
                                    },
                                }
                            }
                        },
                        else => {},
                    }
                }
                if (flag)
                    tuple[i][0] = .{};
            }

            if (debugStart) {
                comptime var i = 0;
                inline while (i < tuple.len) : (i += 1) {
                    std.debug.print("{} {s}\n", .{ i, tuple[i][0] });
                    if (@TypeOf(tuple[i][0]) != void) {
                        std.debug.print("   {*}\n", .{tuple[i][1][0]});
                    }
                }
            }

            return tuple;
        }

        // setup the pipe structs
        pub fn setup(allocator: *std.mem.Allocator) *thePipe {
            var self: *thePipe = allocator.create(thePipe) catch 
                unreachable;

            var p = self.p[0..]; // simipify life using slices
            var nodes = self.nodes[0..];

            inline for (pipe) |stg, i| {
                p[i] = Stage{ .i = i }; // ensure default values are set

                inline for (stg) |elem, j| {
                    switch (@typeInfo(@TypeOf(elem))) {
                        .Fn, .BoundFn => { // fn....
                            var name = @typeName(@TypeOf(elem));
                            const one = std.mem.indexOfPos(u8, name, 0, @typeName(Stage)).?;
                            const start = std.mem.indexOfPos(u8, name, one + @typeName(Stage).len + 1, @typeName(Stage)).?;
                            const end = std.mem.indexOfPos(u8, name, start + @typeName(Stage).len + 1, ")").?;
                            p[i].name = name[start + @typeName(Stage).len + 1 .. end];
                            //if (debugStart) std.debug.print("stg {} {s}\n", .{i, cp[i].name});
                        },
                        .Pointer => { // *const u8...
                            if (debugStart) std.debug.print("label {} {s}\n", .{ i, elem });
                            p[i].label = elem;
                        },
                        .Bool => { // bool...
                            if (debugStart) std.debug.print("end {}\n", .{i});
                            p[i].end = true;
                        },
                        else => {},
                    }
                }
            }

            // storage for hashmap used for node creation
            var buffer: [@sizeOf(@TypeOf(nodes)) + 1000]u8 = undefined;
            const buffalloc = &std.heap.FixedBufferAllocator.init(&buffer).allocator;
            var map = std.hash_map.StringHashMap(Conn).init(buffalloc);
            defer map.deinit();

            // create the pipe's nodes - all fields are initialized by .set or in .run so no Conn{} is needed
            var jj: u32 = 0;
            for (p) |item, ii| {
                var stg = false;
                if (p[ii].name) |_|
                    stg = true;

                if (stg and !p[ii].end) {
                    nodes[jj].set(p, ii, 0, ii + 1, 0);
                    jj += 1;
                }

                if (p[ii].label) |lbl| {
                    if (map.get(lbl)) |*k| {
                        if (!stg) {
                            if (p[ii].end) {
                                nodes[jj - 1].set(p, ii - 1, 0, k.to, k.sin);
                                k.set(p, k.from, k.sout, k.to, k.sin + 1);
                            } else {
                                nodes[jj].set(p, k.from, k.sout, ii + 1, 0);
                                jj += 1;
                                k.set(p, k.from, k.sout + 1, k.to, k.sin);
                            }
                        } else {
                            if (!p[ii].end) {
                                nodes[jj].set(p, k.from, k.sout, ii, k.sin);
                                jj += 1;
                                k.set(p, k.from, k.sout + 1, k.to, k.sin + 1);
                            }
                        }
                        map.put(lbl, k.*) catch 
                            unreachable;
                    } else if (stg) {
                        var k = Conn{};
                        k.set(p, ii, 1, ii, 1);
                        map.put(lbl, k) catch 
                            unreachable;
                    }
                }

                if (debugStart) std.debug.print("{} {}\n", .{ ii, jj });
            }

            // save the node slice in the stages;
            for (p) |item, ii| {
                p[ii].n = self.nodes[0..jj];
            }

            // debug
            if (debugStart) {
                for (p) |s| {
                    if (s.name) |n|
                        std.debug.print("{} {s} {s} {} {*} {*} {*}\n", .{ s.i, s.label, s.name, s.end, s.n, s.inC, s.outC })
                    else
                        std.debug.print("{} {s} {s} {} \n", .{ s.i, s.label, s.name, s.end });
                }

                var kk: u32 = 0;
                while (kk < jj) : (kk += 1) {
                    const c = nodes[kk];
                    std.debug.print("{} {s}({}),{} -> {s}({}),{}\n", .{ kk, p[c.from].name, c.from, c.sout, p[c.to].name, c.to, c.sin });
                }
            }

            return self;
        }

        // run the pipe
        pub fn run(self: *thePipe, tuple: std.meta.Tuple(arg_set)) !void {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            const allocator = &arena.allocator;

            var p = self.p[0..]; // use slices
            var nodes = self.p[0].n;

            // set starting input/output streams to 0
            for (nodes) |*n| {
                if (n.sout == 0) n.src.outC = n;
                if (n.sin == 0) n.dst.inC = n;
                n.in = .ok;
            }

            // set stages into starting State
            for (p) |*s, i| {
                if (s.name) |_| {
                    s.commit = -@intCast(isize, (i + 1));
                    s.state = .start;
                    s.rc = error.active;
                }
            }

            // main dispatch loop

            var commit: isize = -9999;
            var state: u32 = 9;
            var loop: u32 = 0;
            var severed: u32 = 0;

            running: while (severed < nodes.len) {
                var temp: isize = 9999;

                if (loop < 10) for (nodes) |*c, i| {
                    if (c == c.dst.inC and (c.dst.state == .peekto or c.dst.state == .readto) and c.in != .ok and c.dst.commit == commit) {
                        if (debugLoop) {
                            if (state == 2)
                                loop += 1
                            else {
                                state = 2;
                                loop = 0;
                            }
                        }
                        if (debugDisp) std.log.info("pre p dst.state {} {}_{s} sin {} in {} {}\n\n", .{ c.dst.state, c.dst.i, c.dst.name, c.sin, c.in, c.data });
                        resume c.dst.frame;
                        continue :running;
                    }

                    if (c.dst.state == .anyinput and c.in != .ok and c.in != .sever and c.dst.commit == commit) {
                        if (debugDisp) {
                            if (state == 3)
                                loop += 1
                            else {
                                state = 3;
                                loop = 0;
                            }
                        }
                        c.dst.inC = c;
                        if (debugLoop) std.log.info("pre a dst.state {} {}_{s} sin {} \n\n", .{ c.dst.state, c.dst.i, c.dst.name, c.sin });
                        resume c.dst.frame;
                        continue :running;
                    }

                    if (c == c.src.outC and c.src.state == .output and c.in != .data and c.src.commit == commit) {
                        if (debugLoop) {
                            if (state == 1)
                                loop += 1
                            else {
                                state = 1;
                                loop = 0;
                            }
                        }
                        if (debugDisp) std.log.info("pre o src.state {} {}_{s} sout {} in {} {}\n\n", .{ c.src.state, c.src.i, c.src.name, c.sout, c.in, c.data });
                        resume c.src.frame;
                        continue :running;
                    }

                    if ((c.src.outC == null and c.in == .ok) or (c.dst.inC == null and c.in == .ok)) {
                        c.in = .sever;
                        severed += 1;
                        continue :running;
                    }

                    if (c.dst.state == .start and c.dst.commit == commit) {
                        c.dst.state = .run;
                        c.dst.commit = 0;
                        comptime var j = 0;
                        inline while (j < p.len) : (j += 1) {
                            if (@TypeOf(tuple[j][0]) != void) { // prevent inline from generating void calls
                                if (c.dst.i == j)
                                    _ = @asyncCall(allocator.alignedAlloc(u8, 16, @frameSize(tuple[j][0])) catch 
                                        unreachable
                                    , &self.p[j].rc, tuple[j][0], tuple[j][1]);
                            }
                        } 
                        continue :running;
                    }

                    if (c.src.state == .start and c.src.commit == commit) {
                        c.src.state = .run;
                        c.src.commit = 0;
                        comptime var j = 0;
                        inline while (j < p.len) : (j += 1) {
                            if (@TypeOf(tuple[j][0]) != void) { // prevent inline from generating void calls
                                if (c.src.i == j)
                                    _ = @asyncCall(allocator.alignedAlloc(u8, 16, @frameSize(tuple[j][0])) catch 
                                        unreachable
                                    , &self.p[j].rc, tuple[j][0], tuple[j][1]);
                            }
                        }
                        continue :running;
                    }

                    if (c.dst.commit < temp) temp = c.dst.commit;
                    if (c.src.commit < temp) temp = c.src.commit;
                };

                // is it time to commit to a higher level?
                if (debugDisp) std.debug.print("commit {} {}\n", .{ commit, temp });
                if (temp > commit) {
                    commit = temp;
                    continue :running;
                }

                // when we use os threads in stages we will need to wait for them here.

                if (debugLoop) if (loop >= 10) {
                    std.debug.print("\nlooped {}\n", .{state});
                    for (nodes) |*c| {
                        std.debug.print("start {*} src {}_{s} {} {*} {} {} dst {}_{s} {} {*} {} {} in {} {}\n", .{ c, c.src.i, c.src.name, c.sout, c.src.outC, c.src.state, c.src.commit, c.dst.i, c.dst.name, c.sin, c.dst.inC, c.dst.state, c.dst.commit, c.in, c.data });
                    }
                    break :running;
                };

                for (p) |*s| {
                    if (debugDisp) std.log.info("{s} {}", .{ s.name, s.rc });
                    if (s.rc) |_| {
                        continue;
                    } else |err| {
                        if (err == error.endOfStream or s.commit == -90909090) {
                            continue;
                        } else if (severed < nodes.len) {
                            std.debug.print("\nStalled! {s} rc {}\n", .{ s.name, err });
                            for (nodes) |*c| {
                                std.debug.print("start {*} src {}_{s} {} {*} {} {} dst {}_{s} {} {*} {} {} in {} {}\n", .{ c, c.src.i, c.src.name, c.sout, c.src.outC, c.src.state, c.src.commit, c.dst.i, c.dst.name, c.sin, c.dst.inC, c.dst.state, c.dst.commit, c.in, c.data });
                            }
                            return err;
                        }
                    }
                }
                break :running;
            }

            if (debugStart) {
                std.debug.print("\n{}\n", .{Stage});
                for (nodes) |*c| {
                    std.debug.print("start {*} src {}_{s} {} {*} {} {} dst {}_{s} {} {*} {} {} in {} {}\n", .{ c, c.src.i, c.src.name, c.sout, c.src.outC, c.src.state, c.src.commit, c.dst.i, c.dst.name, c.sin, c.dst.inC, c.dst.state, c.dst.commit, c.in, c.data });
                }
            }

            return;
        }
    };
}

pub const x = struct {
    var xxx: u16 = 5;
    var aaa: u16 = 100;
    var ar = [_]u64{ 11, 22, 33, 44 };
    var sss: []u64 = ar[0..];
};

pub const y = struct {
    var xxx: u16 = 17;
    var aaa: u16 = 200;
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    x.aaa += 3;

    // create pipe commands and stages for type u128
    const uP = PipeType(u64);

    // a sample pipe using uP stages
    const pipe = .{
        .{ uP.gen, .{"aaa"} },
        .{ "a", uP.exactdiv, .{"xxx"} },
        .{ "b", uP.fanin },
        .{ uP.console, true },
        .{"a"},
        .{ "c", uP.exactdiv, .{7} },
        .{ "b", true },
        .{"c"},
        .{ uP.exactdiv, .{11} },
        .{ "b", true },
    };

    // play with a var
    x.aaa += 7;

    // create a container for this pipe with type uP and setup the pipe's structure
    var myPipe = PipeInstance(uP, pipe).setup(allocator);

    // create and fill in an args tuple for this pipe base on using context x.
    const myPipeArgs = myPipe.args(x);

    // run the pipe with the above args
    try myPipe.run(myPipeArgs);

    std.debug.print("\n", .{});

    x.aaa = 50;
    x.xxx = 13;

    // run again with updated args
    try myPipe.run(myPipe.args(x));
    
    std.debug.print("\n", .{});
    
    // and using a different context structure
    try myPipe.run(myPipe.args(y));
    
    const pSlicer = .{
        .{ uP.slice, .{"&sss"} },
        .{ uP.console, true },
    };
    
    std.debug.print("\n", .{});
    
    var sPiper = PipeInstance(uP, pSlicer).setup(allocator);
    try sPiper.run(sPiper.args(x));
    
    const pSlicew = .{
        .{ uP.gen, .{3} },
        .{ uP.slice, .{"&sss"}, true },
    };
    
    std.debug.print("\n", .{});
    
    var sPipew = PipeInstance(uP, pSlicew).setup(allocator);
    try sPipew.run(sPipew.args(x));
    
    try sPiper.run(sPiper.args(x));
    
}    
