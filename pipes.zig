const std = @import("std");

const build_options = @import("build_options");
const debugDisp = build_options.debugDisp;
const debugLoop = build_options.debugLoop;
const debugStageTypes = build_options.debugStageTypes;
const debugCmd = build_options.debugCmd;
const debugStart = build_options.debugStart;

// will later exploit usingnamespace to allow users to add stages to library of stages

const State = enum(u4) {
    peekTo,
    readTo,
    output,
    start,
    run,
    done,
    anyinput,
    any,
};

const stageError = error{
    ok,
    endOfStream,
    noInStream,
    noOutStream,
    outOfBounds,
    finished,
    active,
};

const Message = enum(u2) {
    ok,
    data,
    sever,
};

// pub fn ReturnOf(comptime func: anytype) type {
//     return switch (@typeInfo(@TypeOf(func))) {
//         .Fn, .BoundFn => |fn_info| fn_info.return_type.?,
//         else => unreachable,
//     };
// }

// we use TypeUnion to store the values passed between stages.  We define the Stage by passing a tuple of types with
// the types valid to use in the pipe's TypeUnion(s).  We use Filters to create non generic versions of the filter functions
// with specific types, validating that the types are valid for the Stage.

pub fn TypeUnion(comptime list: anytype) type { // list is atuple of the types in this typeUnion

    const info = @typeInfo(@TypeOf(list)); // validate list is a tuple
    if (info != .Struct)
        @compileError("Expected struct type");
    if (!info.Struct.is_tuple)
        @compileError("Struct type must be a tuple type");

    // define the typedUnion's enum (ESet) based on std.meta.FieldEnum
    comptime var s = 0;
    comptime var a = 0;
    comptime var enumFields: [list.len]std.builtin.TypeInfo.EnumField = undefined;
    comptime var decls = [_]std.builtin.TypeInfo.Declaration{};
    inline for (list) |T, i| {
        std.debug.assert(@TypeOf(T) == type); // validate list entry is a type
        enumFields[i].name = @typeName(T);
        enumFields[i].value = i;
        if (@sizeOf(T) > s) s = @sizeOf(T); // track size and alignment needed to store the value
        if (@alignOf(T) > a) a = @alignOf(T);
    }

    const TSet = @Type(.{
        .Enum = .{ // create the enum type
            .layout = .Auto,
            .tag_type = std.math.IntFittingRange(0, list.len - 1),
            .fields = &enumFields,
            .decls = &decls,
            .is_exhaustive = true,
        },
    });

    return struct {
        pub const TU = @This();

        // create buffer for the value with correct alignment and size for the included types
        value: [s]u8 align(a) = [_]u8{undefined} ** s,
        type: TSet = undefined,

        // convert an ESet literal to the corresponding type (if t is runtime we get an error)
        pub fn TypeOf(comptime e: TSet) type {
            return list[@enumToInt(e)];
        }

        pub fn getType(self: *TU) @TypeOf(._) {
            return self.type;
        }

        pub fn put(self: *TU, v: anytype) void {
            if (@TypeOf(v) == TU) {
                std.mem.copy(u8, &self.value, &v.value);
                self.type = v.type;
                return;
            }
            inline for (list) |T, i| {
                if (T == @TypeOf(v)) {
                    @ptrCast(*T, &self.value).* = v;
                    self.type = @intToEnum(TSet, i);
                    return;
                }
            }
            std.debug.print("put: type {} not in TypeUnion {}\n", .{ @TypeOf(v), TU });
            unreachable;
        }

        // return the value of typeUnion - validate that stored and requested types match.
        pub fn get(self: *TU, comptime T: type) T {
            if (T == TU)
                return self.*;
            inline for (list) |U, i| {
                if (i == @enumToInt(self.type)) {
                    if (T == U)
                        return @ptrCast(*T, &self.value).*;
                    std.debug.print("get: Union {} expected type {} found {}\n", .{ list, U, T });
                    unreachable;
                }
            }
            std.debug.print("get: Union {} instance not initialized\n", .{list});
            unreachable;
        }

        // test if type is in typeUnion
        pub fn inUnion(comptime T: type) bool {
            inline for (list) |U| {
                if (T == U)
                    return true;
            }
            return false;
        }

        // check if the typeUnion contains a value of type
        pub fn typeIs(self: TU, comptime T: type) bool {
            inline for (list) |U, i| {
                if (i == @enumToInt(self.type)) {
                    if (T == U)
                        return true;
                }
            }
            return false;
        }
        
        // create a typeUnion via allocator and set its value
        //fn init(allocator: *std.mem.Allocator) TU {
        //    var self: *TU = allocator.create(TU) catch unreachable;
        //    return self.*;
        //}
        
        pub fn list(alloc: *std.mem.Allocator, T: anytype, v: anytype) []TU {
        
            const inf = @typeInfo(@TypeOf(v)); // validate v is a tuple
            if (inf != .Struct)
                @compileError("Expected struct type");
            if (!inf.Struct.is_tuple)
                @compileError("Struct type must be a tuple type");
                
            const tuArray = [v.len]TU;
            var self: *tuArray = alloc.create(tuArray) catch unreachable;
    
            for (self) | *tu, i | {
                comptime var j = 0;
                inline while (j<v.len) : (j+=1) {
                    if (i==j)
                        tu.put(@as(T[j],v[j]));
                }
            }
            return self;
        }
    };
}

// This is used to define the connections between stages (and pipes)

pub fn ConnType(comptime S: type, comptime TU: type) type {
    return struct {
        pub const Conn = @This();

        next: ?*Conn = null,
        src: *S = undefined, // used by output
        from: usize = 0,
        sout: usize = 0,
        dst: *S = undefined, // used by input
        to: usize = 0,
        sin: usize = 0,
        in: Message = undefined,
        data: TU = undefined,

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

// Used to create a type for stages that can be used with any of the types in the list, which needs to be a tuple of types.
// A stage is defined as a filter, its args and connections

pub fn Stage(list: anytype) type {
    return struct {
        pub const StageType = @This();
        pub const TU = TypeUnion(list);
        pub const Conn = ConnType(StageType, TU);
        
        pub var pipeAddr:usize = 0;

        outC: ?*Conn = null,
        inC: ?*Conn = null,
        state: State = undefined,
        commit: isize = -90909090,
        frame: anyframe = undefined,
        rc: anyerror!void = undefined,
        err: stageError = error.ok,
        n: []Conn = undefined,
        name: ?[]const u8 = null,
        end: bool = false,
        i: usize = undefined,
        allocator: *std.mem.Allocator = undefined,

        // these are the commands that can be used inside filters.  Filters defines what a pipe stage does.

        pub fn typeIs(self: *StageType, comptime T: type) !bool {
            if (debugCmd) std.log.info("peekTo {*} inC {*}\n", .{ self, self.inC });
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekTo {*} in {} {}\n", .{ c, c.in, c.data });
                while (c.in == .ok) {
                    suspend {
                        self.state = .peekTo;
                        self.frame = @frame();
                    }
                }
            }
            self.state = .done;
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekTo {}_{s} {} in {} {}\n", .{ self.i, self.name, c.sout, c.in, c.data });
                if (c.in == .data) {
                    return c.data.typeIs(T);
                } else {
                    self.err = error.endOfStream;
                    return self.err;
                }
            }
            self.err = error.noInStream;
            return self.err;
        }

        pub fn peekTo(self: *StageType, comptime T: type) !T {
            if (debugCmd) std.log.info("peekTo {*} inC {*}\n", .{ self, self.inC });
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekTo {*} in {} {}\n", .{ c, c.in, c.data });
                while (c.in == .ok) {
                    suspend {
                        self.state = .peekTo;
                        self.frame = @frame();
                    }
                }
            }
            self.state = .done;
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekTo {}_{s} {} in {} {}\n", .{ self.i, self.name, c.sout, c.in, c.data });
                if (c.in == .data) {
                    return c.data.get(T);
                } else {
                    self.err = error.endOfStream;
                    return self.err;
                }
            }
            self.err = error.noInStream;
            return self.err;
        }

        pub fn readTo(self: *StageType, comptime T: type) !T {
            if (self.inC) |c| {
                while (c.in == .ok) {
                    suspend {
                        self.state = .readTo;
                        self.frame = @frame();
                    }
                }
            }
            self.state = .done;
            if (self.inC) |c| {
                if (c.in == .data) {
                    c.in = .ok;
                    return c.data.get(T);
                } else {
                    self.err =  error.endOfStream;
                    return self.err;
                }
            }
            self.err = error.noInStream;
            return self.err;
        }

        pub fn output(self: *StageType, v: anytype) !void {
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
                    c.data.put(v);
                    return;
                } else {
                    self.err =  error.endOfStream;
                    return self.err;
                }
            }
            self.err = error.noOutStream;
            return self.err;
        }

        pub fn selectOutput(self: *StageType, o: usize) !void {
            return self.setoutC(o);
        }

        pub fn severOutput(self: *StageType) !void {
            if (self.outC) |_| {
                self.outC = null;
            } else {
                self.err = error.noOutStream;
                return self.err;
            }
        }

        pub fn severInput(self: *StageType) !void {
            if (self.inC) |_| {
                self.inC = null;
            } else {
                self.err = error.noInStream;
                return self.err;
            }
        }

        pub fn selectInput(self: *StageType, i: usize) !void {
            return self.setinC(i);
        }

        pub fn selectAnyInput(self: *StageType) !usize {
            // std.log.info("anyin {*} inC {*}\n",.{self, self.inC});
            if (countInstreams(self) > 0) {
                // std.log.info("anyin {*}", .{self.inC});
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
                        self.err =  error.endOfStream;
                        return self.err;
                    }
                }
            }
            self.err = error.noInStream;
            return self.err;
        }
          
        pub fn endStage(self: *StageType) void {
            if (debugCmd) std.debug.print("end: {s}\n",.{self.name});
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
        
        pub fn ok(self: *StageType) !void {
            return if (self.err == error.ok or self.err == error.endOfStream) .{} else self.err;
        }

        fn setinC(self: *StageType, i: usize) !void {
            for (self.n) |*c| {
                if (c.dst == self and c.sin == i) {
                    self.inC = c;
                    return;
                }
            }
            self.inC = null;
            self.err = error.noInStream;
            return self.err;
        }

        pub fn countInstreams(self: *StageType) usize {
            var count: usize = 0;
            for (self.n) |c| {
                if (c.dst == self and (c.in == .data or c.src.outC != null))
                    count += 1;
            }
            return count;
        }

        fn setoutC(self: *StageType, o: usize) !void {
            for (self.n) |*c| {
                if (c.src == self and c.sout == o) {
                    self.outC = c;
                    return;
                }
            }
            self.outC = null;
            self.err = error.noOutStream;
            return self.err;
        }
        
    };
}

pub fn run(comptime context:type, pp:anytype) !void {

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    
    const thisPipe = Mint(pp).init(allocator);
    return thisPipe.run(context);
    
}

// Once we have defined the Stage & Filters types, we need to use a pipe tuple and prepare to run it using a
// PipeInstance.  The PipeInstances is used to set the args for a pipe using struct(s) as context blocks.  Any required
// args are set, the run funcion should be called to execute the pipe.

pub fn Mint(pp: anytype) type {

    comptime var lmap = [_]?u8{null} ** pp.len; // mapping for stg to label
    comptime var nodesLen = pp.len; // number of nodes for the pipe
    
    comptime var labels: std.meta.Tuple( // list of enum literals type for labels
        list: {
        comptime var l: []const type = &[_]type{};
        inline for (pp) |stg| {
            if (@typeInfo(@TypeOf(stg[0])) == .EnumLiteral and stg.len > 1 and @typeInfo(@TypeOf(stg[1])) != .EnumLiteral) {
                l = l ++ &[_]type{@TypeOf(stg[0])};
            }
        }
        break :list l; // return tuple of enum literal types, to create labels tuple
    }) = undefined;
    
    { // context block - we want to use k later in runtime
        comptime var k = 0; // save enum literals in the labels tuple
        inline for (pp) |stg| {
            if (@typeInfo(@TypeOf(stg[0])) == .EnumLiteral and stg.len > 1 and @typeInfo(@TypeOf(stg[1])) != .EnumLiteral) {
                labels[k] = stg[0];
                k += 1;
            } // nodesLen may need adjustment with addpipe/callpipe
            inline for (stg) |elem, j| {
                if (@typeInfo(@TypeOf(elem)) == .EnumLiteral and elem == ._) {
                    if (j > 0)
                        nodesLen -= 1
                    else
                        unreachable; // illegal ._ at start of stage Tuple
                }
            }
        } // map stage to label
        inline for (pp) |stg, i| {
            if (@typeInfo(@TypeOf(stg[0])) == .EnumLiteral and stg[0] != ._) {
                for (labels) |lbl, j| {
                    if (stg[0] == lbl)
                        lmap[i] = j; // save the mapping
                }
            }
        }
    }
    
    //inline for (pp) |_, i| {
    //    if (lmap[i]) |l| {
    //        @compileLog(i);
    //        @compileLog(labels[l]);
    //    }
    //}

    comptime var ST:?type = null; // get the StageType from one of the Filter function calls
    comptime var arg_set: []const type = &[_]type{}; // build tuple of types for stage Fn and Args
    inline for (pp) |stg, i| { // fill in the args types
        comptime var flag: bool = true;
        inline for (stg) |elem| {
            const E = @typeInfo(@TypeOf(elem));
            switch (E) {
                .Fn, .BoundFn => {
                    if (debugStart) std.debug.print("fn {} {}\n", .{ i, elem });
                    arg_set = arg_set ++ &[_]type{std.meta.Tuple(&[_]type{ @TypeOf(elem), std.meta.ArgsTuple(@TypeOf(elem)) })};
                    flag = false;
                    
                    if (ST == null) { // get StageType from a Filter fn's first arg type which must be *StageType
                        std.debug.assert(!E.Fn.is_generic);
                        ST = @typeInfo(E.Fn.args[0].arg_type.?).Pointer.child;
                    }
                },
                else => {},
            }
        }
        if (flag)
            arg_set = arg_set ++ &[_]type{std.meta.Tuple(&[_]type{void})};
    }
    
    std.debug.assert(ST != null);
    
    return struct { // return an instance of ThisPipe
        const StageType = ST.?; // The StageType as extracted from a Filter function
        const Conn = StageType.Conn; // list of connections
        const ThisPipe = @This(); // this pipe's type
        const pipe = pp; // the pipe source tuple

        // stages/filter of the pipe
        p: [pipe.len]StageType = [_]StageType{undefined} ** pipe.len,

        // connection nodes
        nodes: [nodesLen]Conn = [_]StageType.Conn{undefined} ** nodesLen,
        
        // the pipes last error (or void)
        rc: anyerror!void = undefined,

        // associate the args with the stage's filters and a context struct.  Returns an arg_tuple

        fn args(self: *ThisPipe, context: anytype) std.meta.Tuple(arg_set) {

            //tuple for calling fn(s) allong with the arguement tuples reguired
            var tuple: std.meta.Tuple(arg_set) = undefined;

            // fill in the args tuple adding the fn(s) and argument values, using the passed contect structure block
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
                                //@compileLog(i);
                                //@compileLog(@typeInfo(@TypeOf(tuple[i][1][k+1])));
                                //@compileLog(arg);
                                switch (@typeInfo(@TypeOf(arg))) {
                                    .Int, .Float, .ComptimeInt, .ComptimeFloat, .EnumLiteral => { // constants
                                        if (debugStart) std.debug.print("Int {} {}\n", .{ j, arg });
                                        tuple[i][1][k + 1] = arg;
                                    },
                                    .Null => {
                                        tuple[i][1][k + 1] = null;
                                    },
                                    .Pointer => { // string with a var, var.field or (use a slice, not an array)
                                        if (debugStart) std.debug.print("Ptr {} {s}\n", .{ j, arg });

                                        // this would be much simpiler if runtime vars worked in nested tuples...

                                        if (@TypeOf(tuple[i][1][k + 1]) == []const u8 and arg[0] == "\"") { // expect a string
                                            tuple[i][1][k + 1] = arg[1..];
                                        } else if (context != void) comptime {
                                            // use the type of the args tuple to decide what we are pointing too
                                            switch (@typeInfo(@TypeOf(tuple[i][1][k + 1]))) {
                                                .Int, .Float => {
                                                    if (std.mem.indexOfPos(u8, arg, 0, ".")) |dot| // single level struct.field
                                                        tuple[i][1][k + 1] = @field(@field(context, arg[0..dot]), arg[dot + 1 ..])
                                                    else
                                                        tuple[i][1][k + 1] = @field(context, arg);
                                                },
                                                .Pointer => |p| {
                                                    switch (@typeInfo(p.child)) {
                                                        .Int, .Float, .Pointer, .Struct => {
                                                            if (std.mem.indexOfPos(u8, arg, 0, ".")) |dot| // single level
                                                                tuple[i][1][k + 1] = &@field(@field(context, arg[0..dot]), arg[dot + 1 ..])
                                                            else
                                                                tuple[i][1][k + 1] = &@field(context, arg);
                                                        },
                                                        else => {
                                                            @compileLog(p.child);
                                                            @compileLog(@typeInfo(p.child));
                                                        },
                                                    }
                                                },
                                                .Optional => |p| {
                                                    switch (@typeInfo(p.child)) {
                                                        .Int, .Float, .Pointer, .Struct => {
                                                            if (std.mem.indexOfPos(u8, arg, 0, ".")) |dot| // single level
                                                                tuple[i][1][k + 1] = &@field(@field(context, arg[0..dot]), arg[dot + 1 ..])
                                                            else
                                                                tuple[i][1][k + 1] = &@field(context, arg);
                                                        },
                                                        else => {
                                                            @compileLog(p.child);
                                                            @compileLog(@typeInfo(p.child));
                                                        },
                                                    }
                                                },
                                                else => {
                                                    @compileLog(@TypeOf(tuple[i][1][k + 1])); // unsupported arg type
                                                },
                                            }
                                        };
                                    },
                                    // more types will be needed, depending on additional stages
                                    else => {
                                        @compileLog(@TypeOf(arg)); // unsupported arg type
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

        // setup the pipe structs.  Only call once per PipeInstances.  An allocator is requied for the needed storage.

        pub fn init(allocator: *std.mem.Allocator) *ThisPipe {
            var self: *ThisPipe = allocator.create(ThisPipe) catch unreachable;
            
            StageType.pipeAddr =  @ptrToInt(self);
            
            var p = self.p[0..]; // simipify life using slices
            var nodes = self.nodes[0..];

            inline for (pipe) |stg, i| {
                p[i] = StageType{ .i = i, .allocator = allocator }; // ensure default values are set
               
                inline for (stg) |elem, j| { // parse the pipe
                    switch (@typeInfo(@TypeOf(elem))) {
                        .Fn, .BoundFn => { // fn....
                            var name = @typeName(@TypeOf(elem));
                            //std.debug.print("{s}\n",.{name});
                            const end = std.mem.indexOfPos(u8, name, 0, ")).Fn.return_type").?;
                            const start = std.mem.lastIndexOf(u8,name[0..end],".").? + 1;
                            p[i].name = name[start..end];
                            if (debugStart) std.debug.print("stg {} {s}\n", .{ i, p[i].name });
                        },
                        //.Pointer => { // *const u8...
                        //    if (debugStart) std.debug.print("label {} {s}\n", .{ i, elem });
                        //    p[i].label = elem;
                        //},
                        .EnumLiteral => { // label (.any) or end (._)
                            if (j > 0 and elem == ._) { // j == 0 case caught by constructor
                                p[i].end = true;
                                if (debugStart) std.debug.print("end {}\n", .{i});
                            }
                        },
                        else => {},
                    }
                }
            }

            // var buffer: [@sizeOf(@TypeOf(nodes)) + 1000]u8 = undefined;
            // const buffalloc = &std.heap.FixedBufferAllocator.init(&buffer).allocator;
            // var map = std.hash_map.StringHashMap(Conn).init(buffalloc);
            // defer map.deinit();

            var map: [nodesLen]?Conn = .{null} ** nodesLen;

            // create the pipe's nodes - all fields are initialized by .set or in .run so no Conn{} is needed
            var j: u32 = 0;
            for (p) |item, i| {
                const stg = if (item.name) |_| true else false;

                if (stg and !item.end ) { // add nodes for each stage that does not have an end of stream
                    nodes[j].set(p, i, 0, i + 1, 0);
                    j += 1;
                }

                if (lmap[i]) |lbl| {  // get the label index of this pp step
                    if (map[lbl]) |*k| {
                        if (!stg) {
                            if (item.end) {
                                if (i == 0 or !p[i-1].end) { // if previous item did not have an end
                                    nodes[j - 1].set(p, i - 1, 0, k.to, k.sin); 
                                    k.set(p, k.from, k.sout, k.to, k.sin + 1);
                                } else { // this is a null output stream
                                    k.set(p, k.from, k.sout+1, k.to, k.sin);
                                }
                            } else {
                                nodes[j].set(p, k.from, k.sout, i + 1, 0);
                                j += 1;
                                k.set(p, k.from, k.sout + 1, k.to, k.sin);
                            }
                        } else {
                            if (item.end) {
                                nodes[j].set(p, k.from, k.sout, i, k.sin);
                                j += 1;
                                k.set(p, k.from, k.sout + 1, k.to, k.sin + 1);
                            }
                        }
                    } else if (stg) {
                        map[lbl] = Conn{};
                        map[lbl].?.set(p, i, 1, i, 1);
                    }
                 }
                if (debugStart) std.debug.print("{} {}\n", .{ i, j });
            }
                                                                    
            // save the node slice in the stages;
            for (p) |_, i| {
                p[i].n = self.nodes[0..j];
            }

            // debug
            if (debugStart) {
                for (self.nodes[0..j]) |c, k| {
                    std.debug.print("{} {s}({}),{} -> {s}({}),{}\n", .{ k, p[c.from].name, c.from, c.sout, p[c.to].name, c.to, c.sin });
                }
            }

            return self;
        }

        // run the pipe, you can repeat runs with different arg_tuple(s) without redoing setup.

        pub fn run(self: *ThisPipe, context: anytype) !void {
            const tuple = self.args(context);
            //var arena3 = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            //defer arena3.deinit();
            
            var p = self.p[0..]; // use slices
            var nodes = self.p[0].n;
            self.rc = .{};
            
            const allocator = p[0].allocator;
            
            // set starting input/output streams to lowest connect streams (usually 0 - but NOT always)
            for (nodes) |*n| {
            
                if (n.src.outC) |c| {
                    if (n.sout < c.sout) n.src.outC = n;
                } else
                    n.src.outC = n;
                    
                if (n.dst.inC) |c| {
                    if (n.sin < c.sin) n.dst.inC = n;
                } else
                    n.dst.inC = n;
                
                n.in = .ok;
            }

            // set stages to starting State
            for (p) |*s, i| {
                if (s.name) |_| {
                    s.commit = -@intCast(isize, (i + 1));
                    s.state = .start;
                    s.rc = error.active;
                    s.err = error.ok;
                }
            }

            // main dispatch loop

            var commit: isize = -9999;
            var state: u32 = 9;
            var loop: u32 = 0;
            var severed: u32 = 0;

            running: while (severed < nodes.len) {
                var temp: isize = 9999;

                if (loop < 10) for (nodes) |*c| {

                    // dispatch a pending output
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

                    // dispatch a pending peekTo or readTo
                    if (c == c.dst.inC and (c.dst.state == .peekTo or c.dst.state == .readTo) and c.in != .ok and c.dst.commit == commit) {
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

                    // dispatch a pending anyinput
                    if (c.dst.state == .anyinput and c.in != .ok and c.in != .sever and c.dst.commit == commit) {
                        if (debugLoop) {
                            if (state == 3)
                                loop += 1
                            else {
                                state = 3;
                                loop = 0;
                            }
                        }
                        c.dst.inC = c;
                        if (debugDisp) std.log.info("pre a dst.state {} {}_{s} sin {} \n\n", .{ c.dst.state, c.dst.i, c.dst.name, c.sin });
                        resume c.dst.frame;
                        continue :running;
                    }

                    // compete a sever after any data in the node is consumed
                    if ((c.src.outC == null and c.in == .ok) or (c.dst.inC == null and c.in == .ok)) {
                        if (debugDisp) std.log.info("pre sever {} {}_{s} sin {} \n\n", .{ c.dst.state, c.dst.i, c.dst.name, c.sin });
                        c.in = .sever;
                        severed += 1;
                        continue :running;
                    }

                    // start a stage fliter from connection destination
                    if (c.dst.state == .start and c.dst.commit == commit) {
                        c.dst.state = .run;
                        c.dst.commit = 0;
                        comptime var j = 0;
                        inline while (j < p.len) : (j += 1) {
                            if (@TypeOf(tuple[j][0]) != void) { // prevent inline from generating void calls
                                if (c.dst.i == j)
                                    _ = @asyncCall(allocator.alignedAlloc(u8, 16, @frameSize(tuple[j][0])) catch
                                        unreachable, &self.p[j].rc, tuple[j][0], tuple[j][1]);
                            }
                        }
                        continue :running;
                    }

                    // start a stage filter from connection source
                    if (c.src.state == .start and c.src.commit == commit) {
                        c.src.state = .run;
                        c.src.commit = 0;
                        comptime var j = 0;
                        inline while (j < p.len) : (j += 1) {
                            if (@TypeOf(tuple[j][0]) != void) { // prevent inline from generating void calls
                                if (c.src.i == j)
                                    _ = @asyncCall(allocator.alignedAlloc(u8, 16, @frameSize(tuple[j][0])) catch
                                        unreachable, &self.p[j].rc, tuple[j][0], tuple[j][1]);
                            }
                        }
                        continue :running;
                    }

                    // track commit levels
                    if (c.dst.commit < temp) temp = c.dst.commit;
                    if (c.src.commit < temp) temp = c.src.commit;
                };

                // is it time to commit to a higher level?
                if (debugDisp) std.debug.print("commit {} {}\n", .{ commit, temp });
                if (temp > commit) {
                    commit = temp;
                    continue :running;
                }

                // when/if we use os threads in stages we will need to wait for them here.

                if (debugLoop) if (loop >= 10) {
                    std.debug.print("\nlooped {}\n", .{state});
                    for (nodes) |*c| {
                        std.debug.print("start {*} src {}_{s} {} {*} {} {} dst {}_{s} {} {*} {} {} in {} {}\n", .{ c, c.src.i, c.src.name, c.sout, c.src.outC, c.src.state, c.src.commit, c.dst.i, c.dst.name, c.sin, c.dst.inC, c.dst.state, c.dst.commit, c.in, c.data });
                    }
                    break :running;
                };

                // check completion and detect stalls
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
                                std.debug.print("src {}_{s} {} {} {} dst {}_{s} {} {} {} in {} {}\n", .{ c.src.i, c.src.name, c.sout, c.src.state, c.src.commit, c.dst.i, c.dst.name, c.sin, c.dst.state, c.dst.commit, c.in, c.data });
                            }
                            self.rc = err;
                            return err;
                        }
                    }
                }
                break :running;
            }

            if (debugStart) {
                std.debug.print("\n{}\n", .{StageType});
                for (nodes) |*c| {
                    std.debug.print("src {}_{s} {} {} {} dst {}_{s} {} {} {} in {} {}\n", .{ c.src.i, c.src.name, c.sout, c.src.state, c.src.commit, c.dst.i, c.dst.name, c.sin, c.dst.state, c.dst.commit, c.in, c.data });
                }
            }

            return;
        }
    };
}
