const std = @import("std");

//const filters = @import("filters");

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
    call,
    start,
    run,
    done,
    sever,
    anyinput,
    commit,
    dummy,
};

const stageError = error{
    ok,
    finished,
    active,
    endOfStream,
    noInStream,
    noOutStream,
    outOfBounds,
    pipeStall,
};

const Message = enum(u2) {
    ok,
    data,
    sever,
};

pub fn ReturnOf(comptime func: anytype) type {
     return switch (@typeInfo(@TypeOf(func))) {
         .Fn, .BoundFn => |fn_info| fn_info.return_type.?,
         else => unreachable,
    };
}

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
        
        data: TU = undefined,
        in: Message = undefined,
        src: *S = undefined, // used by output
        from: usize = 0,
        sout: usize = 0,
        dst: *S = undefined, // used by input
        to: usize = 0,
        sin: usize = 0,
        //next: ?*Conn = null,    // unused but removing it causes stalls (WHY?)

        fn set(self: *Conn, p: []S, f: usize, o: usize, t: usize, s: usize) void {
            self.from = f;      // only used when building connectons (to be removed)
            self.src = &p[f];   // stage output
            self.sout = o;      // stream number
            self.to = t;        // only used when building connectons (to be removed)
            self.dst = &p[t];   // stage input
            self.sin = s;       // stream number
        }
    };
}

// Used to create a type for stages that can be used with any of the types in the list, which needs to be a tuple of types.
// A stage is defined as a filter, its args and connections
    
pub fn _Stage(comptime Make:*Z, list: anytype) type {

    return struct {
    
        pub const StageType = @This();
        pub const TU = TypeUnion(list);
        pub const Conn = ConnType(StageType, TU);
        
        outC: ?*Conn = null,            // output conn
        inC: ?*Conn = null,             // input conn 
        fwdC: ?*Conn = null,
        bwdC: ?*Conn = null,
        state: State = undefined,
        commit: isize = -90909090,
        frame: anyframe = undefined,
        err: stageError = error.ok,
        n: []Conn = undefined,          // connections used by this stage (might be removable)
        name: ?[]const u8 = null,       // name of the stage, may dissapear as we get it via questionable hack        
        end: bool = false,
        i: usize = undefined,
        allocator: *std.mem.Allocator = undefined,  // allocator in use for this pipe      
        pipeAddr:usize = 0,                         // Use Make.getPT() and PTIdx to get a pointer to the parent pipe
        PTIdx:usize = undefined,
        
        pub fn call(self: *StageType, ctx:anytype, tup:anytype) !void {
            
            const PT = Make.getPT();
            inline for (PT) | T, i | {
                if (i == self.PTIdx) {
                    const parent = @intToPtr(*T,self.pipeAddr); // parent pipe so we can adjust nodes
                    _ = parent;
                    const CallPipe = Make.Mint(tup);
                    //std.debug.print("call {any}\n",.{CallPipe.pipe});
                    var callpipe = CallPipe.init(self.allocator); // create callpipe instance
                    //var save = Conn{};
                    
                    if (self.outC == null and callpipe.outIdx < 255)
                        return error.noOutStream;
                        
                    if (self.inC == null and callpipe.inIdx < 255)
                        return error.noInStream;
                    
                    //if (callpipe.outIdx < 255) {
                    //    save.src = self.outC.?.src;
                    //    save.from = self.outC.?.from;
                    //    callpipe.p[callpipe.outIdx].outC = self.outC;
                    //    self.outC.?.src = &callpipe.p[callpipe.outIdx];
                    //    self.outC.?.from = callpipe.outIdx;
                    //}
                    //if (callpipe.inIdx < 255) {
                    //    save.dst = self.inC.?.dst;
                    //    save.to = self.inC.?.to;
                    //    callpipe.p[callpipe.inIdx].inC = self.inC;
                    //    self.inC.?.dst = &callpipe.p[callpipe.inIdx];
                    //    self.inC.?.to = callpipe.inIdx;
                    //}
                    try callpipe.run(ctx);
                    
                    //if (callpipe.outIdx < 255) { // restore output
                    //    self.outC.?.src = save.src;
                    //    self.outC.?.from = save.from;                    
                    // }
                    //if (callpipe.inIdx < 255) { //restore input
                    //    self.inC.?.dst = save.dst;
                    //    self.inC.?.to = save.to;
                    //}
                    _ = parent;
                }        
            }

            //suspend {
            //    self.state = .call;             // tell dispatcher to run the pipe
            //    self.frame = @frame();
            //}
            self.state = .done;
        }
        
        pub fn inStream(self: *StageType) !usize {
            if (self.inC) |c|
                return c.sin;
            return error.noInStream;
        }
        
        pub fn outStream(self: *StageType) !usize {
            if (self.outC) |c|
                return c.sout;
            return error.noOutStream;
        }
        
        pub fn typeIs(self: *StageType, comptime T: type) !bool {
            if (debugCmd) std.log.info("peekTo {*} inC {*}\n", .{ self, self.inC });
            if (self.inC) |c| {
                if (debugCmd) std.log.info("peekTo {*} in {} {}\n", .{ c, c.in, c.data });
                while (c.in == .ok) {
                    suspend {
                        //self.fwdC = c;
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
                    self.bwdC = c;
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
                    self.fwdC = c;
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
                self.state = .sever;
            } else {
                self.err = error.noOutStream;
                return self.err;
            }
        }

        pub fn severInput(self: *StageType) !void {
            if (self.inC) |_| {
                self.inC = null;
                self.state = .sever;
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
                    if (c.in == .data) {
                        return c.sin;
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
                    if (self.inC == c)
                        c.dst.bwdC = c;
                    self.inC = null;
                }
                if (c.src == self) {
                    if (self.outC == c)
                        c.src.fwdC = c;
                    self.outC = null;
                }
                self.state = .sever;
            }
            const PT = Make.getPT();
            inline for (PT) | T, i | {
                if (i == self.PTIdx) {
                    const parent = @intToPtr(*T,self.pipeAddr);             // parent pipe so we can track pipe.rc
                    if (@errorToInt(self.err) > @errorToInt(parent.rc))
                        parent.rc = self.err;
                }        
            }
            self.commit = 90909090;
            return;
        }
        
        pub fn ok(self: *StageType) !void {
            return  if (self.err == error.ok or self.err == error.endOfStream or self.err == error.noInStream) 
                        .{}
                    else 
                        self.err;
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

pub fn _run(comptime Make: *Z, comptime context:type, pp:anytype) !void {

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    
    const thisPipe = Make.Mint(pp).init(allocator);
    return thisPipe.run(context);
    
}

// In filters and the Stage struct we sometimes need to find the PipeType and instance that created the stage.
// Since PipeTypes and instances are created after Stages/Filters Type we have a small problem.  This comptime struct
// front ends PipeType creations so we get extract them in Stages.  When a Stage is created we use @prtToInto to save
// the instance pointer so we later can use getPT and an inline for to find the PipeType and @intToPtr to get the 
// pipe's instance.

pub const Z = struct {

    PT: []const type = &[_]type{},

    pub fn Mint(comptime self:*Z, pp:anytype) type {
        // @compileLog("recursive");
        const Pipe = _Mint(self,pp,self.PT.len);
        self.PT = &[_]type{Pipe} ++ self.PT;
        return Pipe;
    }

    pub fn Stage(comptime self:*Z, list:anytype) type {
        return _Stage(self, list);
    }
    
    pub fn run(comptime self:*Z, comptime context:type, pp:anytype) !void {
        return try _run(self, context, pp);
    }
    
    pub fn getPT(comptime self:*Z) []const type {
        return self.PT;
    }
    
};

// Once we have defined the Stage & Filters types, we need to create a pipeType using Mint.  The PipeType
// is used to set the args for a pipe using struct(s) as context blocks.  Any required args are set, 
// the run funcion should be called to execute the pipe.

// Mint creates a tuple, arg_set, of stage arguement types, sets up some label mapping, and returns a PipeType.
// init creates a PipeType using an allocator, then connects the stages using connectors and the label mapping. 
// run uses arg_set to create a tuple of values for the pipe stages arguements, fills in the values, and runs the pipe

fn _Mint(comptime Make:*Z, pp:anytype, pN:usize) type {

    _ = Make;

    comptime var lmap = [_]?u8{null} ** pp.len; // mapping for stg to label
    comptime var nodesLen = pp.len; // number of nodes for the pipe
    
    comptime var labels: std.meta.Tuple( // list of enum literals type for labels
        list: {
        comptime var l: []const type = &[_]type{};
        inline for (pp) |stg| {
            if (@typeInfo(@TypeOf(stg[0])) == .EnumLiteral and @tagName(stg[0])[0] != '_' and stg.len > 1 and  @typeInfo(@TypeOf(stg[1])) != .EnumLiteral) {
                l = l ++ &[_]type{@TypeOf(stg[0])};
            }
        }
        break :list l; // return tuple of enum literal types, to create labels tuple
    }) = undefined;
    
    { // context block - we want to use k later in runtime
        comptime var k = 0; // save enum literals in the labels tuple
        inline for (pp) |stg, i| {
            if (@typeInfo(@TypeOf(stg[0])) == .EnumLiteral and @tagName(stg[0])[0] != '_' and stg.len > 1 and  @typeInfo(@TypeOf(stg[1])) != .EnumLiteral) {
                labels[k] = stg[0];
                k += 1;
            } // nodesLen may need adjustment with addpipe/callpipe
            inline for (stg) |elem, j| {
                if (@typeInfo(@TypeOf(elem)) == .EnumLiteral) {
                    switch  (elem) {                     
                            ._  => { if (j > 0) nodesLen -= 1 else unreachable; },   // end must follow a label or filter 
                            ._i => { if (j == 0 and i < pp.len-1) nodesLen -= 1 else unreachable; },  // bad in connector
                            ._o => { if (j == 0 and i > 0) nodesLen -= 1 else unreachable; },  // bad out connector
                        else => {}, // extend when .add method added
                    }
                }
            }
        } // map stage to label
        inline for (pp) |stg, i| {
            if (@typeInfo(@TypeOf(stg[0])) == .EnumLiteral and @tagName(stg[0])[0] != '_' ) { // ignore ._{x}
                for (labels) |lbl, j| {
                    if (stg[0] == lbl)
                        lmap[i] = j; // save the mapping
                }
            }
        }
    }
        
    // inline for (pp) |_, i| {
    //    if (lmap[i]) |l| {
    //         @compileLog(i);
    //         @compileLog(labels[l]);
    //     }
    // }
    
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
        const pNum = pN;
        
        // stages/filter of the pipe
        p: [pipe.len]StageType = [_]StageType{undefined} ** pipe.len,

        // connection nodes
        nodes: [nodesLen]Conn = [_]StageType.Conn{undefined} ** nodesLen,
        
        // callpipe input and output connector numbers (255 when no connection)
        inIdx:u8 = 255,
        outIdx:u8 = 255,
        
        // commit level of pipe
        commit: isize = -90909090,
        severed: u32 = 0,
        
        // the pipes last error (or void)
        rc: anyerror = error.ok,
        
        // the pipes args for this run (see fn args)
        theArgs: std.meta.Tuple(arg_set) = undefined,
         
        pub fn init(allocator: *std.mem.Allocator) *ThisPipe {
        
            var self: *ThisPipe = allocator.create(ThisPipe) catch unreachable;
            
            //self.parent = Parent;
            
            var p = self.p[0..]; // simipify life using slices
            var nodes = self.nodes[0..];
            self.outIdx = 255;
            self.inIdx = 255;
                        
            //@compileLog(p.len);
            inline for (pipe) |stg, i| {
                p[i] = StageType{ .i=i, .allocator=allocator, .PTIdx = pNum, .pipeAddr = @ptrToInt(self) };
                                
                inline for (stg) |elem| { // parse the pipe
                    switch (@typeInfo(@TypeOf(elem))) {
                        .Fn, .BoundFn => { // fn....
                            var name = @typeName(@TypeOf(elem));
                            //std.debug.print("{s}\n",.{name});
                            const end = std.mem.indexOfPos(u8, name, 0, ")).Fn.return_type").?;
                            const start = std.mem.lastIndexOf(u8,name[0..end],".").? + 1;
                            p[i].name = name[start..end];
                            if (debugStart) std.debug.print("stg {} {s}\n", .{ i, p[i].name });
                        },
                        .EnumLiteral => { // label (.any) or end (._)
                            switch (elem) { // position of EnumLiteral in stage tuple already verified in Mint comptime
                                ._   => { p[i].end = true; },
                                ._i  => { if (i==0 or (i>0 and p[i-1].end)) 
                                              self.inIdx = if (self.inIdx==255) i+1 else unreachable // dup input connnector
                                          else
                                              unreachable; // illegal input connector
                                        },
                                ._o  => { if (!p[i-1].end) // i>0 known from Mint comptime
                                              self.outIdx = if (self.outIdx==255) i-1 else unreachable // dup output connnector
                                          else
                                              unreachable; // illegal output connector
                                        },
                                else => {}, // not interested in labels here
                            }
                            //std.debug.print("Idx {} {} {}\n",.{self.outIdx,self.inIdx, nodesLen});
                        },
                        else => {},
                    }
                }
            }

            // var buffer: [@sizeOf(@TypeOf(nodes)) + 1000]u8 = undefined;
            // const buffalloc = &std.heap.FixedBufferAllocator.init(&buffer).allocator;
            // var map = std.hash_map.StringHashMap(Conn).init(buffalloc);
            // defer map.deinit();
            //@compileLog(nodes.len);
            var map: [nodesLen]?Conn = .{null} ** nodesLen;

            // create the pipe's nodes - all Conn{} fields are initialized by .set or in .run so
            var j: u32 = 0;
            for (p) |item, i| {
                const stg = if (item.name) |_| true else false;

                if (stg and !item.end and i != self.outIdx) { // add nodes when not end of stream and no out connection
                    nodes[j].set(p, i, 0, i + 1, 0);
                    j += 1;
                }

                if (lmap[i]) |lbl| {  // get the label index of this pp step
                    if (map[lbl]) |*k| {
                        if (!stg) {
                            if (item.end) {
                                if (i == 0 or !p[i-1].end) { // if previous item did not have an end of stream
                                    if (p[i-1].name == null) {
                                        var n = map[lmap[i-1].?].?;
                                        nodes[j - 1].set(p, n.from, n.sout-1, k.to, k.sin);
                                        k.set(p, k.from, k.sout, k.to, k.sin + 1);
                                        n.set(p, n.from, n.sout + 1, n.to, n.sin);
                                    } else {
                                        nodes[j - 1].set(p, i - 1, 0, k.to, k.sin); 
                                        k.set(p, k.from, k.sout, k.to, k.sin + 1);
                                    }
                                } else { // this is a null output stream
                                    k.set(p, k.from, k.sout+1, k.to, k.sin);
                                }
                            } else {
                                nodes[j].set(p, k.from, k.sout, i + 1, 0);
                                j += 1;
                                //if (i > 0 and p[i-1].name != null) {
                                    k.set(p, k.from, k.sout + 1, k.to, k.sin);
                                //}
                            }
                        } else {
                            if (item.end) {
                                nodes[j].set(p, k.from, k.sout, i, k.sin);
                                j += 1;
                                k.set(p, k.from, k.sout + 1, k.to, k.sin + 1);
                            }
                        }
                    } else  {
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
                    std.debug.print("{} {}_{s} {} -> {}_{s} {}\n", .{ k, c.from, p[c.from].name, c.sout, c.to, p[c.to].name, c.sin });
                }
            }
            //@compileLog("done");
            //std.debug.print("{*}\n{*}\n",.{p,nodes});
            return self;
        }
 
 
        // associate the args with the stage's filters and a context struct.  Returns an arg_tuple
        
        fn args(self: *ThisPipe, context: anytype) std.meta.Tuple(arg_set) {

            //tuple for calling fn(s) allong with the arguement tuples reguired
            var tuple = self.theArgs;

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
                        .EnumLiteral => { // label
                            continue;
                        },
                        .Struct => { // struct we have found the tuple with the args...
                            inline for (elem) |arg, k| {
                                //@compileLog(i);
                                //@compileLog(@typeInfo(@TypeOf(tuple[i][1][k+1])));
                                //@compileLog(arg);
                                switch (@typeInfo(@TypeOf(arg))) {
                                    .Int, .Float, .ComptimeInt, .ComptimeFloat, .EnumLiteral => { // constants
                                        if (debugStart) std.debug.print("Int {} {}\n", .{ j, arg });
                                        tuple[i][1][k+1] = arg;
                                    },
                                    .Null => {
                                        tuple[i][1][k+1] = null;
                                    },
                                    .Pointer => { // string with a var, var.field or (use a slice, not an array)
                                        if (debugStart) std.debug.print("Ptr {} {s}\n", .{ j, arg });

                                        // this would be much simpiler if runtime vars worked in nested tuples...

                                        if (@TypeOf(tuple[i][1][k+1]) == []const u8 and arg[0] == '\'') { // expect a string
                                            tuple[i][1][k+1] = arg[1..];
                                            continue;
                                        }
                                        if (context == void) 
                                            continue;
                                            
                                        comptime {
                                            var t = @typeInfo(@TypeOf(tuple[i][1][k+1])); // base type from args tuple
                                            if (t == .Optional) { 
                                                t = @typeInfo(t.Optional.child); // type of the optional
                                            }
                                            switch (t) { // decide what to do with the arg using type in args tuple
                                                .Int, .Float => {
                                                    tuple[i][1][k+1] = if (std.mem.indexOfPos(u8,arg,0,".")) |dot|
                                                        @field(@field(context, arg[0..dot]), arg[dot+1..])
                                                    else
                                                        @field(context, arg);
                                                },
                                                .Pointer => |p| { 
                                                    if (p.size == .Slice) { // slices are implied pointers
                                                        tuple[i][1][k+1] = if (std.mem.indexOfPos(u8,arg,0,".")) |dot| 
                                                            @field(@field(context, arg[0..dot]), arg[dot+1..])
                                                        else
                                                            @field(context, arg);
                                                    } else { // pointer to something else (non slice)
                                                        switch (@typeInfo(p.child)) {
                                                            .Int, .Float, .Struct, .Pointer => {
                                                                tuple[i][1][k+1] = if (std.mem.indexOfPos(u8,arg,0,".")) |dot| 
                                                                    &@field(@field(context, arg[0..dot]), arg[dot+1..])
                                                                else
                                                                    &@field(context, arg);
                                                            },
                                                            else => {
                                                                @compileLog(p.child);
                                                                @compileLog(@typeInfo(p.child));
                                                            },
                                                        }
                                                    }
                                                },
                                                else => {
                                                    @compileLog(@TypeOf(tuple[i][1][k+1])); // unsupported arg type
                                                },
                                            }
                                        }
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
        

        // run the pipe, you can repeat runs with different arg_tuple(s) without redoing setup.

        pub fn run(self: *ThisPipe, context: anytype) !void {
                    
            const what:enum{prep,call,all} = .all;
                    
            var p = self.p[0..];        // use slices for easier to read code
            var nodes = self.p[0].n;
            const allocator = p[0].allocator; // use the same allocator used to allocate ThisPipe

            if (what == .prep or what == .all) {
            
                self.theArgs = self.args(context);
                //var arena3 = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                //defer arena3.deinit();
                            
                self.severed = 0;
                self.rc = error.ok;
                
                
                
                // set starting input/output streams to lowest connected streams (usually 0 - but NOT always)
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
                for (p) |*s| {
                    if (s.name) |_| {
                        //s.commit = -@intCast(isize, (i + 100));
                        s.commit = -1;
                        s.state = .start;
                        //s.rc = error.active;
                        s.err = error.ok;
                    }
                }

                // main dispatch loop

                self.commit = -1;
                self.severed = 0;
            }
            
            var state: u32 = 9;
            var loop: u32 = 0;       
            
            const cList = std.ArrayList(*Conn);

            var fwdList = cList.init(allocator);
            defer fwdList.deinit();
            var bwdList = cList.init(allocator);
            defer bwdList.deinit();
            
            var list:*cList = &fwdList;
            var runState:usize = 0;
            
            running: while (self.severed < nodes.len) {
                var temp: isize = 90909090;    
                
                runState = 0;
                
                if (fwdList.items.len > 0) {
                    list = &fwdList;
                    //std.debug.print(">",.{});
                } else {
                    runState = 1;
                    if (bwdList.items.len > 0) {
                        list = &bwdList;
                        //std.debug.print("<",.{});
                    } else
                        runState = 2;
                }

                //std.debug.print("({})",.{fwdList.items.len});
                if (runState<2) while (list.items.len>0) {
                    
                    const c = list.swapRemove(list.items.len-1);                    
                
                    // dispatch a pending peekTo or readTo
                    if (c == c.dst.inC and (c.dst.state == .peekTo or c.dst.state == .readTo) and c.in != .ok and c.dst.commit == self.commit) {
                        // std.debug.print("p",.{});
                        if (debugLoop) {
                            if (state == 2)
                                loop += 1
                            else {
                                state = 2;
                                loop = 0;
                            }
                        }
                        if (debugDisp) std.log.info("pre p dst.state {} {}_{s} sin {} in {} {}\n\n", .{ c.dst.state, c.dst.i, c.dst.name, c.sin, c.in, c.data });
                        //_ = list.swapRemove(i);
                        resume c.dst.frame;
                        if (c.dst.fwdC) |n| {
                            try fwdList.append(n);
                            c.dst.fwdC = null;
                        }
                        if (c.dst.bwdC) |n| {
                            try bwdList.append(n);
                            c.dst.bwdC = null;
                        }
                        continue :running;
                    }

                    // dispatch a pending anyinput
                    if (c.dst.state == .anyinput and c.in != .ok and c.in != .sever and c.dst.commit == self.commit) {
                        //std.debug.print("a",.{});
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
                        //_ = list.swapRemove(i);
                        resume c.dst.frame;
                        if (c.dst.fwdC) |n| {
                            try fwdList.append(n);
                            c.dst.fwdC = null;
                        } 
                        if (c.dst.bwdC) |n| {
                            try bwdList.append(n);
                            c.dst.bwdC = null;
                        }
                        continue :running;
                    }
                   
                    // dispatch a pending output
                    if (c == c.src.outC and c.src.state == .output and c.in != .data and c.src.commit == self.commit) {
                        //std.debug.print("o",.{});
                        if (debugLoop) {
                            if (state == 1)
                                loop += 1
                            else {
                                state = 1;
                                loop = 0;
                            }
                        }
                        if (debugDisp) std.log.info("pre o src.state {} {}_{s} sout {} in {} {}\n\n", .{ c.src.state, c.src.i, c.src.name, c.sout, c.in, c.data });
                        //_ = list.swapRemove(i);
                        resume c.src.frame;
                        if (c.src.fwdC) |n| {
                            try fwdList.append(n);
                            c.src.fwdC = null;                            
                        } 
                        if (c.src.bwdC) |n| {
                            try bwdList.append(n);
                            c.src.bwdC = null;
                        }
                        continue :running;
                    } 
                    
                    // compete a sever after any data in the node is consumed
                    //if ((c.src.state == .sever and c.src.outC == null and c.in == .ok) or 
                    //    (c.dst.state == .sever and c.dst.inC == null and c.in == .ok)
                    if ((c.src.state == .sever and c.in == .ok) or 
                        (c.dst.state == .sever and c.in == .ok)
                       ) {
                        // std.debug.print("v",.{});
                        if (debugDisp) std.log.info("pre sever {} {}_{s} sin {} \n\n", .{ c.dst.state, c.dst.i, c.dst.name, c.sin });
                        if (c.in != .sever)
                            try fwdList.append(c);
                        c.in = .sever;
                        self.severed += 1;
                        continue :running;
                    
                    }
                    
                    // special server logic for selectAnyInput
                    if (c.dst.state == .anyinput and c.src.state == .sever) {
                        if (c.dst.countInstreams() == 0) {
                            //std.debug.print("r",.{});
                            c.in = .sever;
                            resume c.dst.frame;
                            self.severed += 1;
                            if (c.dst.fwdC) |n| {
                                try fwdList.append(n);
                                c.dst.fwdC = null;
                            } 
                            if (c.dst.bwdC) |n| {
                                try fwdList.append(n);  // yes fwd list here
                                c.dst.bwdC = null;
                            }
                            continue :running;
                        }
                    }                                                            
                };
                
                // if (runState < 2)
                //     std.debug.print("~",.{});
            
                runState += 1;
                                    
                if (runState < 2 or fwdList.items.len>0)
                    continue :running;

                // std.debug.print(":",.{});
                
                var i = nodes.len;
                if (loop<10) while (i>0) { // (nodes) |*c| {
                    i -= 1;
                    var c = &nodes[i];
                    
                //if (loop<10) for (nodes) |*c| {
                    
                    // std.debug.print(":",.{});
                
                    // dispatch a pending peekTo or readTo
                    if (c == c.dst.inC and (c.dst.state == .peekTo or c.dst.state == .readTo) and c.in != .ok and c.dst.commit == self.commit) {
                        //std.debug.print("{c}",.{@tagName(c.dst.state)[0]});
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
                        if (c.dst.fwdC) |n| {
                            try fwdList.append(n);
                            c.dst.fwdC = null;
                        }
                        if (c.dst.bwdC) |n| {
                            try bwdList.append(n);
                            c.dst.bwdC = null;
                        }
                        continue :running;
                    }

                 //   // dispatch a pending anyinput
                 //   if (c.dst.state == .anyinput and c.in != .ok and c.in != .sever and c.dst.commit == self.commit) {
                 //   //if (c.dst.state == .anyinput and c.in != .ok and c.dst.commit == self.commit) {
                 //       std.debug.print("a",.{});
                 //       if (debugLoop) {
                 //           if (state == 3)
                 //               loop += 1
                 //           else {
                 //               state = 3;
                 //               loop = 0;
                 //           }
                 //       }
                 //       c.dst.inC = c;
                 //       if (debugDisp) std.log.info("pre a dst.state {} {}_{s} sin {} \n\n", .{ c.dst.state, c.dst.i, c.dst.name, c.sin });
                 //       resume c.dst.frame;
                 //       if (c.dst.fwdC) |n| {
                 //           try fwdList.append(n);
                 //           c.dst.fwdC = null;
                 //       }
                 //       if (c.dst.bwdC) |n| {
                 //           try bwdList.append(n);
                 //           c.dst.bwdC = null;
                 //       }
                 //       continue :running;
                 //   }
                 //  
                 //   // dispatch a pending output
                 //   if (c == c.src.outC and c.src.state == .output and c.in != .data and c.src.commit == self.commit) {
                 //       std.debug.print("o",.{});
                 //       if (debugLoop) {
                 //           if (state == 1)
                 //               loop += 1
                 //           else {
                 //               state = 1;
                 //               loop = 0;
                 //           }
                 //       }
                 //       if (debugDisp) std.log.info("pre o src.state {} {}_{s} sout {} in {} {}\n\n", .{ c.src.state, c.src.i, c.src.name, c.sout, c.in, c.data });
                 //       resume c.src.frame;
                 //       if (c.src.fwdC) |n| {
                 //           try fwdList.append(n);
                 //           c.src.fwdC = null;
                 //       }
                 //       if (c.src.bwdC) |n| {
                 //           try bwdList.append(n);
                 //           c.src.bwdC = null;
                 //       }
                 //       continue :running;
                 //   } 
                   
                    // compete a sever after any data in the node is consumed
                    if ((c.src.state == .sever and c.in == .ok) or 
                        (c.dst.state == .sever and c.in == .ok)
                       ) {
                        //std.debug.print("v",.{});
                        if (debugDisp) std.log.info("pre sever {} {}_{s} sin {} \n\n", .{ c.dst.state, c.dst.i, c.dst.name, c.sin });
                        c.in = .sever;
                        self.severed += 1;
                        continue :running;
                
                    }
                    
                    // special server logic for selectAnyInput
                    if (c.dst.state == .anyinput and c.src.state == .sever) {
                        if (c.dst.countInstreams() == 0) {
                            //std.debug.print("r",.{});
                            c.in = .sever;
                            resume c.dst.frame;
                            self.severed += 1;
                            if (c.dst.fwdC) |n| {
                                try fwdList.append(n);
                                c.dst.fwdC = null;
                            }
                            if (c.dst.bwdC) |n| {
                                try bwdList.append(n);
                                c.dst.bwdC = null;
                            }
                            continue :running;
                        }
                    }
                    
                    //std.debug.print(".call dst {} {}\n      src {} {} - {} {}\n",.{c.dst.state, c.dst.commit, c.src.state, c.src.commit, self.commit, c.in});
                    if ((c.dst.state == .call or c.src.state == .call) and c.in != .sever ) {
                        const stage = if (c.dst.state == .call) c.dst else c.src;         
                        if (stage.commit == self.commit) {
                            resume stage.frame;
                            if (stage.fwdC) |n| {
                                try fwdList.append(n);
                                stage.fwdC = null;
                            } 
                            if (c.dst.fwdC) |n| {
                                try fwdList.append(n);
                            c.dst.fwdC = null;
                            }
                            continue :running;
                        }
                    }

                    // start a stage fliter from connection destination
                    if (c.dst.state == .start and c.dst.commit == self.commit) {
                        c.dst.state = .run;
                        c.dst.commit = 0;
                        comptime var j = 0;
                        inline while (j < p.len) : (j += 1) {
                            if (@TypeOf(self.theArgs[j][0]) != void) { // prevent inline from generating void calls
                                if (c.dst.i == j) {
                                    const f = pipe[j][if (@typeInfo(@TypeOf(pipe[j][0])) == .EnumLiteral) 1 else 0];
                                    // @frameSize can be buggy...
                                    _ = @asyncCall(allocator.alignedAlloc(u8, 16, @sizeOf(@Frame(f))) catch
                                        unreachable, {}, f, self.theArgs[j][1]);
                                }
                            }
                        }
                        if (c.dst.fwdC) |n| {
                            try fwdList.append(n);
                            c.dst.fwdC = null;
                        }
                        if (c.dst.bwdC) |n| {
                            try bwdList.append(n);
                            c.dst.bwdC = null;
                        }
                        continue :running;
                    }

                    // start a stage filter from connection source
                    if (c.src.state == .start and c.src.commit == self.commit) {
                        c.src.state = .run;
                        c.src.commit = 0;
                        comptime var j = 0;
                        inline while (j < p.len) : (j += 1) {
                            if (@TypeOf(self.theArgs[j][0]) != void) { // prevent inline from generating void calls
                                if (c.src.i == j) {
                                    const f = pipe[j][if (@typeInfo(@TypeOf(pipe[j][0])) == .EnumLiteral) 1 else 0];
                                    // @frameSize can be buggy...
                                    _ = @asyncCall(allocator.alignedAlloc(u8, 16, @sizeOf(@Frame(f))) catch
                                        unreachable, {}, f, self.theArgs[j][1]);
                                }
                            }
                        }
                        if (c.src.fwdC) |n| {
                            try fwdList.append(n);
                            c.src.fwdC = null;
                        }
                        if (c.src.bwdC) |n| {
                            try bwdList.append(n);
                            c.src.bwdC = null;
                        }                   
                        continue :running;
                    }
                    
                    // track commit levels
                    if (c.dst.commit < temp) temp = c.dst.commit;
                    if (c.src.commit < temp) temp = c.src.commit;
                };

                // is it time to commit to a higher level?
                if (debugDisp) std.debug.print("commit {} {}\n", .{ self.commit, temp });
                if (temp > self.commit) {
                    self.commit = temp;
                    if (what == .prep and self.commit >= 0)
                        return;
                    continue :running;
                }
                
                // when/if we use os threads in stages we will need to wait for them here.

                if (debugLoop) if (loop >= 10) {
                    std.debug.print("looping\n", .{});
                    for (nodes) |*c| {
                        std.debug.print("src {}_{s} {} {} {} {} dst {}_{s} {} {} {} {} in {} {}\n", .{ c.src.i, c.src.name, c.sout, c.src.state, c.src.err,  c.src.commit, c.dst.i, c.dst.name, c.sin, c.dst.state, c.dst.err, c.dst.commit, c.in,  c.data.type });
                    }
                    break :running;
                };
                
                // detect stalls
                for (p) |*s| {
                    if (debugDisp) std.log.info("{s} {}", .{ s.name, s.rc });
                    if (s.err != error.ok) {
                        if (s.err == error.endOfStream or s.commit == -90909090) {
                            continue;
                        } else if (self.severed < nodes.len) {
                            std.debug.print("\nStalled! {s} rc {}\n", .{ s.name, s.err });
                            for (nodes) |*c| {
                                std.debug.print("src {}_{s} {} {} {} dst {}_{s} {} {} {} in {} {}\n", .{ c.src.i, c.src.name, c.sout, c.src.state, c.src.commit, c.dst.i, c.dst.name, c.sin, c.dst.state, c.dst.commit, c.in,  c.data.type });
                            }
                            self.commit = 90909090;
                            self.rc = error.pipeStall;
                            return self.rc;
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

            self.commit = 90909090;
            return;
        }
    };
}

