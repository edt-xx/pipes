const std = @import("std");

const debugDisp = false;
const debugLoop = false;
const debugStages = false;
const debugCmd = false;
const debugStart = false;

// will later exploit usingnamespace to allow users to add stages to library of stages

const State = enum(u4) {
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
    
// we use TypeUnion to store the values passed between stages.  We define the StageType by passing a tuple of types with
// the types valid to use in the pipe's TypeUnion(s).  We use Filters to create non generic versions of the filter functions 
// with specific types, validating that the types are valid for the StageType.
    
pub fn TypeUnion(comptime list: anytype) type {     // list is atuple of the types in this typeUnion

    const info = @typeInfo(@TypeOf(list));          // validate list is a tuple
    if (info != .Struct)
            @compileError("Expected struct type");
    if (!info.Struct.is_tuple)
            @compileError("Struct type must be a tuple type");

    // define the typedUnion's enum (ESet)
    comptime var s=0;
    comptime var a=0;
    comptime var enumFields: [list.len]std.builtin.TypeInfo.EnumField = undefined;
    comptime var decls = [_]std.builtin.TypeInfo.Declaration{};
    inline for (list) |T, i| {
        std.debug.assert(@TypeOf(T) == type);       // validate list entry is a type
        enumFields[i].name = @typeName(T);
        enumFields[i].value = i;
        if (@sizeOf(T) > s) s = @sizeOf(T);         // track size and alignment needed to store the value
        if (@alignOf(T) > a) a = @alignOf(T);
    }
    
    const TSet = @Type( .{ .Enum = .{               // create the enum type 
            .layout = .Auto,
            .tag_type = std.math.IntFittingRange(0, list.len-1),
            .fields = &enumFields,
            .decls = &decls,
            .is_exhaustive = true,
            }
        });
    
    return struct {
        const TU = @This();
        
        // create buffer for the value with correct alignment and size for the included types
        value: [s]u8 align(a) = [_]u8{undefined} ** s, 
        type: TSet = undefined,      

        // convert an ESet literal to the corresponding type (if t is runtime we get an error)
        pub fn TypeOf(comptime e: TSet) type {
            return list[@enumToInt(e)];
        }
        
        pub fn getType(self:*TU) @TypeOf(._) {
            return self.type;
        }

        pub fn put(self:*TU, v:anytype) void {
            if (@TypeOf(v) == TU) {
                std.mem.copy(u8,&self.value,&v.value);
                self.type = v.type;
                return;
            } 
            inline for (list) |T, i| {
                if (T==@TypeOf(v)) {
                    @ptrCast(*T,&self.value).* = v; 
                    self.type =  @intToEnum(TSet,i);
                    return;
                }
            }
            std.debug.print("put: type {} not in TypeUnion {}\n",.{@TypeOf(v),TU});
            unreachable;
        }
        
        // return the value of typeUnion - validate that stored and requested types match.
        pub fn get(self:*TU, comptime T:type) T {
            if (T == TU)
                return self.*;
            inline for (list) |U, i| {
                if (i == @enumToInt(self.type)) {
                    if (T==U) 
                        return @ptrCast(*T,&self.value).*;                    
                    std.debug.print("get: Union {} expected type {} found {}\n",.{list, U, T});
                    unreachable;                 
                }
            } 
            unreachable;    // not initialized
        }

        // test if type is in typeUnion
        pub fn inUnion(comptime T:type) bool { 
            inline for (list) |U| {
                if (T == U)
                    return true;
            }
            return false;
        }

        // check if the typeUnion contains a value of type
        pub fn typeIs(self:TU, comptime T:type) bool {
            inline for (list) |U, i| {
                if (i == @enumToInt(self.type)) {
                    if (T==U)
                        return true;
                }
            }
            return false;
        }

    };
}    

// This is used to define the connections between stages (and pipes)

pub fn ConnType(comptime S: type, comptime TU: type) type {

    return struct {
    
        pub const Conn = @This();

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

pub fn StageType(list: anytype) type {

    return struct {
    
        pub const Stage = @This();
        pub const TU = TypeUnion(list);
        pub const Conn = ConnType(Stage, TU);

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
        allocator: *std.mem.Allocator = undefined,
        
// these are the commands that can be used inside filters.  Filters defines what a pipe stage does.        
        
        pub fn peekto(self: *Stage, comptime T:type) !T {
            if (debugCmd) std.log.info("peekto {*} inC {*}\n", .{ self, self.inC });
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekto {*} in {} {}\n", .{ c, c.in, c.data });
                while (c.in == .ok) {
                    suspend {
                        self.state = .peekto;
                        self.frame = @frame();
                    }
                }
            }
            self.state = .done;
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekto {}_{s} {} in {} {}\n", .{ self.i, self.name, c.sout, c.in, c.data });
                if (c.in == .data) {
                    return c.data.get(T);
                } else {
                    return error.endOfStream;
                }
            }
            return error.noInStream;
        }

        pub fn readto(self: *Stage, comptime T: type) !T {
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
                    return c.data.get(T);
                } else {
                    return error.endOfStream;
                }
            }
            return error.noInStream;
        }

        pub fn output(self: *Stage, v: anytype) !void {
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
                    return error.endOfStream;
                }
            }
            return error.noOutStream;
        }

        pub fn selectOutput(self: *Stage, o: usize) !void {
            return self.setoutC(o);
        }

        pub fn severOutput(self: *Stage) !void {
            if (self.outC) |_| {
                self.outC = null;
            } else return error.noOutStream;
        }

        pub fn severInput(self: *Stage) !void {
            if (self.inC) |_| {
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
    
    };
}

// using std.meta.args.Tuple implies that no filter can use generic functions.  We use Filter to generate filter functions
// that are not generic.  We do not have the functions in StageType since we generate the args for the functions via
// PipeInstance.args and can set the first arg of the non generic filter function to the StageType used by the pipe.  

pub fn Filters(comptime Stage:type, comptime T:type) type {

    std.debug.assert(Stage.TU.inUnion(T)); 
    
    return struct {
    
        const S = Stage; 
        
        fn exactdiv(self:*S, d: T) callconv(.Async) !void {
            defer { self.endStage(); }
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            while (true) {
                if (debugStages) std.log.info("div pre peek {}_{s} {*}", .{ self.i, self.name, self.outC });
                var i = try self.peekto(u64);
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
                _ = try self.readto(u64);
            }
        }
        
        fn slice(self:*S, slc:*[]T) callconv(.Async) !void {
            defer { self.endStage(); }
            var input:bool = false;
            _ = self.selectInput(0) catch |err| { if (err == error.noInStream) input = true; };
            if (input) {
                for (slc.*) |d| {
                    _ = try self.output(d);
                }
            } else { 
                loop: {                     // probably should use an arraylist here and slice the results
                    var i:u32 = 0;
                    const max = slc.len;    // when updating, do not exceed the initial length of the slice
                    slc.len = i;
                    while (i<max) : (i += 1) {
                        const d = self.peekto(T) catch { break :loop; };
                        slc.len = i+1;
                        slc.*[i] = d;
                        _ = try self.readto(T);
                    }                
                    return error.outOfBounds;
                }
                //need to make a copy if we output it
                //if (S.TU.inUnion(*[]T))
                //    _ = self.output(slc) catch {};
            }
            return;
        }

        fn console(self:*S) callconv(.Async) !void {
            defer { self.endStage(); }
            const stdout = std.io.getStdOut().writer();
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            while (true) {
                if (debugStages) std.log.info("con in {}_{s} {*}", .{ self.i, self.name, self.inC });
                var e = try self.peekto(T);
                switch (@typeInfo(T)) { 
                    .Int, .Enum, .Union => 
                        try stdout.print(" {}", .{e}),
                    .Pointer => 
                        try stdout.print(" {any}", .{e}),
                    else =>
                        try stdout.print(" {}", .{e}),
                }
                _ = self.output(e) catch {};
                _ = try self.readto(T);
            }
        }

        fn fanin(self:*S) callconv(.Async) !void {
            defer { self.endStage(); }
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            while (true) {
                _ = self.selectAnyInput() catch |err| {
                    if (err == error.endOfStream) continue else return err;
                };
                if (debugStages) std.log.info("fan {}_{s} {*} {*}", .{ self.i, self.name, self.inC, self.outC });
                const tmp = try self.peekto(S.TU);
                try self.output(tmp);
                _ = try self.readto(S.TU);
            }
        }

        fn gen(self:*S, limit: T) callconv(.Async) !void {
            defer { self.endStage(); }
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            var i:u64 = 0;
            while (i < limit) : (i += 1) {
                if (debugStages) std.log.info("gen out {}_{s} {*}", .{ self.i, self.name, self.outC });
                try self.output(i);
            }
        }
    
    };
}
            
// Once we have defined the StageType & Filters types, we need to use a pipe tuple and prepare to run it using a 
// PipeInstance.  The PipeInstances is used to set the args for a pipe using struct(s) as context blocks.  Any required 
// args are set, the run funcion should be called to execute the pipe.

pub fn PipeInstance(comptime T: type, pp: anytype) type {

    // build tuple of types for stage Fn and Args
    comptime var arg_set: []const type = &[_]type{};
    
    comptime var labels:std.meta.Tuple(                     // list of enum literals type for labels
        list: {
            comptime var l: []const type = &[_]type {};
            inline for (pp) |stg| {
                if (@typeInfo(@TypeOf(stg[0])) == .EnumLiteral and stg.len>1 and @typeInfo(@TypeOf(stg[1])) != .EnumLiteral)
                    l = l ++ &[_]type{@TypeOf(stg[0])};
            }
            break :list l;                                  // return tuple of enum literal types, to create labels tuple
        } ) = undefined;
    comptime var defn:[labels.len]u8 = undefined;           // stage the label is defined in
    comptime var lmap = [_]bool{false} ** pp.len;           // stage starts with a label
    {   
        comptime var j = 0;                                 // save enum literals in the labels tuple
        inline for (pp) |stg, i| {
            if (@typeInfo(@TypeOf(stg[0])) == .EnumLiteral and stg[0] != ._) {
                lmap[i] = true;
                if (stg.len>1 and @typeInfo(@TypeOf(stg[1])) != .EnumLiteral) {
                    labels[j] = stg[0];
                    defn[j] = i;
                    j += 1;
                }
            }
        }                                                   // *** should compute the number of nodes here too
    }                                                       // *** node array cannot be comptime (connections are dynamic)
   
    inline for (pp) |stg, i| {                              // create the args_set tuple for args function
        comptime var flag: bool = true;
        inline for (stg) |elem| {
            const E = @typeInfo(@TypeOf(elem));           
            switch (E) {
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
        const Stage = T;            // the StageType
        const Conn = Stage.Conn;    // list of connections
        const thePipe = @This();    // the pipeInstance
        const pipe = pp;            // the pipe source tuple

        // stages/filter of the pipe
        p: [pipe.len]Stage = [_]Stage{undefined} ** pipe.len,

        // connection nodes
        nodes: [pipe.len]Conn = [_]Stage.Conn{undefined} ** pipe.len,
        
// associate the args with the stage's filters and a context struct.  Returns an arg_tuple

        pub fn args(self: *thePipe, context: anytype) std.meta.Tuple(arg_set) {
        
            //tuple for calling fn(s) allong with the arguement tuples reguired
            var tuple: std.meta.Tuple(arg_set) = undefined;

            // fill in the tuple adding the fn(s) and argument values, using the passed contect structure block
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
                                    .Int, .ComptimeInt => { // constants
                                        if (debugStart) std.debug.print("Int {} {}\n", .{ j, arg });
                                        tuple[i][1][k + 1] = arg;
                                    },
                                    .Pointer => { // string with a var, &var, var.field or &var.field (use a slice, not an array)
                                        if (debugStart) std.debug.print("Ptr {} {s}\n", .{ j, arg });
                                        comptime {
                                            if (std.mem.indexOfPos(u8, arg, 0, ".")) |dot| { // handle single level struct.field
                                                if (arg[0] == '&') {
                                                        tuple[i][1][k+1] = &@field(@field(context, arg[1..dot]), arg[dot+1..]);
                                                } else {
                                                        tuple[i][1][k+1] = @field(@field(context, arg[0..dot]), arg[dot+1..]); 
                                                }
                                            } else if (arg[0] == '&') { 
                                                tuple[i][1][k+1] = &@field(context, arg[1..]);
                                            } else {
                                                tuple[i][1][k+1] = @field(context, arg);
                                            }
                                        }
                                    },
                                    // more types will be needed, depending on additional stages
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

// setup the pipe structs.  Only call once per PipeInstances.  An allocator is requied for the needed storage.

        pub fn setup(allocator: *std.mem.Allocator) *thePipe {
        
            var self: *thePipe = allocator.create(thePipe) catch unreachable;

            var p = self.p[0..]; // simipify life using slices
            var nodes = self.nodes[0..];

            inline for (pipe) |stg, i| {
                p[i] = Stage{ .i = i, .allocator = allocator };     // ensure default values are set

                inline for (stg) |elem, j | {                       // parse the pipe
                    switch (@typeInfo(@TypeOf(elem))) {
                        .Fn, .BoundFn => { // fn....
                            var name = @typeName(@TypeOf(elem));
                            const one = std.mem.indexOfPos(u8, name, 0, @typeName(Stage)).?;
                            const two = std.mem.indexOfPos(u8, name, one+@typeName(Stage).len+1, @typeName(Stage)).?;
                            const start = std.mem.indexOfPos(u8, name, two+@typeName(Stage).len+1, ")").?;
                            const end = std.mem.indexOfPos(u8, name, start+1, ")).").?;
                            p[i].name = name[start+2 .. end];
                            if (debugStart) std.debug.print("stg {} {s}\n", .{i, p[i].name});
                        },
                        //.Pointer => { // *const u8...
                        //    if (debugStart) std.debug.print("label {} {s}\n", .{ i, elem });
                        //    p[i].label = elem;
                        //},
                        .EnumLiteral => { // label (.any) or end (._)
                            if (j == 0) {
                                if (elem == ._) {
                                    std.debug.print("illegal ._ found: {}\n",.{stg});
                                    unreachable;
                                }
                                p[i].label = @tagName(elem);
                                if (debugStart) std.debug.print("label {} {s}\n", .{ i, elem });
                            } else {
                                if (elem == ._ ) {
                                    p[i].end = true;
                                    if (debugStart) std.debug.print("end {}\n", .{i});
                                } else {
                                    
                                    unreachable;
                                }
                            }
                            
                        },
                        else => {},
                    }
                }
            }

            // *** to be optimized not to need an allocator or a hashmap (setup done) ***
            
            // storage for hashmap used for node creation
            var buffer: [@sizeOf(@TypeOf(nodes)) + 1000]u8 = undefined;
            const buffalloc = &std.heap.FixedBufferAllocator.init(&buffer).allocator;
            var map = std.hash_map.StringHashMap(Conn).init(buffalloc);
            defer map.deinit();

            // create the pipe's nodes - all fields are initialized by .set or in .run so no Conn{} is needed
            var jj: u32 = 0;
            for (p) |item, ii| {

                const stg = if (item.name) |_| true else false;

                if (stg and !item.end) {
                    nodes[jj].set(p, ii, 0, ii + 1, 0);
                    jj += 1;
                }

                if (item.label) |lbl| {
                    if (map.get(lbl)) |*k| {
                        if (!stg) {
                            if (item.end) {
                                nodes[jj - 1].set(p, ii - 1, 0, k.to, k.sin);
                                k.set(p, k.from, k.sout, k.to, k.sin + 1);
                            } else {
                                nodes[jj].set(p, k.from, k.sout, ii + 1, 0);
                                jj += 1;
                                k.set(p, k.from, k.sout + 1, k.to, k.sin);
                            }
                        } else {
                            if (!item.end) {
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
            for (p) | _, ii| {
                p[ii].n = self.nodes[0..jj];
            }

            // debug
            if (debugStart) {
                for (p) |s| {
                    if (s.name) |_|
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

// run the pipe, you can repeat runs with different arg_tuple(s) without redoing setup.

        pub fn run(self: *thePipe, tuple: std.meta.Tuple(arg_set)) !void {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            const allocator = &arena.allocator;

            var p = self.p[0..];        // use slices
            var nodes = self.p[0].n;

            // set starting input/output streams to 0 & connection status to .ok
            for (nodes) |*n| {
                if (n.sout == 0) n.src.outC = n;
                if (n.sin == 0) n.dst.inC = n;
                n.in = .ok;
            }

            // set stages to starting State
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

                if (loop < 10) for (nodes) |*c| {
                
                    // dispatch a pending peekto or readto
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

                    // dispatch a pending anyinput
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

                    // compete a sever after any data in the node is consumed
                    if ((c.src.outC == null and c.in == .ok) or (c.dst.inC == null and c.in == .ok)) {
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
                                        unreachable
                                    , &self.p[j].rc, tuple[j][0], tuple[j][1]);
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
                                        unreachable
                                    , &self.p[j].rc, tuple[j][0], tuple[j][1]);
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

// two context structs
pub const x = struct {
    var xxx: u16 = 5;
    var aaa: u16 = 100;
    var ar = [_]u64{ 11, 22, 33, 44 };
    var sss: []u64 = ar[0..];
};

pub const y = struct {
    var xxx: u16 = 17;
    var aaa: u16 = 80;
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    x.aaa += 3;

    // create pipe commands and stages for type u128
    const uP = StageType(.{u64, *[]u64});
    const f = Filters(uP,u64);

    // a sample pipe using uP stages, note that ._ indicates stage or connector's output is not connected
    const pipe = .{
        .{ f.gen, .{"aaa"} },
        .{ .a, f.exactdiv, .{"xxx"}},
        .{ .b, f.fanin },
        .{ f.console, ._ },
        .{ .a },
        .{ .c, f.exactdiv, .{7}},
        .{ .b, ._ },
        .{ .c },
        .{ f.exactdiv, .{11}},
        .{ .b, ._ },
    };
    
    // play with a vars
    x.xxx += 1;
    x.aaa += 7;

    // create a container for this pipe with type uP and setup the pipe's structure
    var myPipe = PipeInstance(f.S, pipe).setup(allocator);

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
        .{ f.slice, .{"&sss"} },
        .{ f.console, ._ },
    };
    
    std.debug.print("\n", .{});
    
    var sPiper = PipeInstance(uP, pSlicer).setup(allocator);
    try sPiper.run(sPiper.args(x));
    
    const g = Filters(uP, *[]u64);
    const pSlicew = .{
        .{ f.gen, .{"ar.len"} },
        .{ f.slice, .{"&sss"} },
        .{ g.console, ._ },
    };
    
    std.debug.print("\n", .{});
    
    var sPipew = PipeInstance(uP, pSlicew).setup(allocator);
    
    // update the slice
    try sPipew.run(sPipew.args(x));
    
    // output the updated slice
    try sPiper.run(sPiper.args(x));
    
    std.debug.print("\n", .{});
    
}    
