const std = @import("std");

const build_options = @import("build_options");
const debugDisp = build_options.debugDisp;
const debugLoop = build_options.debugLoop;
const debugStages = build_options.debugStages;
const debugCmd = build_options.debugCmd;
const debugStart = build_options.debugStart;

// using std.meta.argsTuple implies that no filter can use generic functions.  We use Filter to generate filter functions
// that are not generic.  We do not have the functions in StageType since we generate the args for the functions via
// PipeInstance.args and can set the first arg of the non generic filter function to the StageType used by the pipe.        

pub fn Filters(comptime StageType: type, comptime Selected: type) type {

    std.debug.assert(StageType.TU.inUnion(Selected));

    return struct {
        pub const S = StageType;
        pub const T = Selected;

        // we need to return the global error set. When used, the fn type includes the name of the function.  We use this
        // to set the name of the stage...

        pub fn exactdiv(self: *S, d: T) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            while (true) {
                if (debugStages) std.log.info("div pre peek {}_{s} {*}", .{ self.i, self.name, self.outC });
                var i = try self.peekTo(T);
                //if (debugStages) std.log.info("div post peek {}_{s} {}",.{self.i, self.name, i});
                if (i % d == 0) {
                    try self.selectOutput(0);
                } else {
                    self.selectOutput(1) catch |err| 
                        if (err != error.noOutStream) return err;  
                }
                if (debugStages) std.log.info("div out {}_{s} {*}", .{ self.i, self.name, self.outC });
                    _ = self.output(i) catch |err| 
                if (err != error.noOutStream) return err;
                
                _ = try self.readTo(T);
            }
        }
        
        // send items to selected output stream using the index of the type in the items typeUnion
        pub fn collateTypes(self: *S) callconv(.Async) !void {
            defer 
                self.endStage();
                
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
                       
            while (true) {
                const tu = try self.peekTo(S.TU);
                //std.debug.print("{any} {}\n",.{tu,@enumToInt(tu.type)});
                self.selectOutput(@enumToInt(tu.type)) catch |err|
                     if (err != error.noOutStream) return err;
                if (self.outC != null) { // if an output stream is selected
                    // std.debug.print("try output {} {any}\n",.{@enumToInt(tu.type),tu});
                    _ = try self.output(tu);
                }
                _ = try self.readTo(S.TU);
            }        
        }
        
        pub fn sink(self: *S) callconv(.Async) !void {
            defer 
                self.endStage();
                
            while (true) {
                _ = try self.readTo(S.TU);
            }
        }

        pub fn locate(self: *S, d: T) callconv(.Async) !void {
            defer 
                self.endStage();
                
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            std.debug.assert(T == []const u8 or T == ?[]const u8 or
                             T == [:0]const u8 or T == ?[:0]const u8);
            
            while (true) {
                var i = try self.peekTo(T);
                if (std.mem.indexOfPos(u8, i, 0, d)) {
                    try self.selectOutput(0);
                } else {
                    self.selectOutput(1) catch |err| 
                        if (err != error.noOutStream) return err;   
                }
                _ = self.output(i) catch |err| 
                    if (err != error.noOutStream) return err;
                
                _ = try self.readTo(S.TU);
            }
        
        }
        
        pub fn typeUnion(self: *S, slc: *[]S.TU) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            for (slc.*) |d| {
                _ = try self.output(d);
            }
            while (true) {  // copy rest stream to output
                const tu = self.peekTo(S.TU) catch break;
                self.output(tu) catch break;
                _ = self.readTo(S.TU) catch break;
            }
            return self.ok();
        }
        
        // put the contents of the slice into the pipe, then copy any inputs to output
        pub fn slice(self: *S, slc: ?[]T) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            if (slc == null) {
                while (true) {
                    const d = try self.peekTo([]T);
                    for (d) |i| {
                        _ = try self.output(i);
                    }
                    _ = try self.readTo([]T);
                }                
            } else {
                for (slc.?) |d| {
                    _ = try self.output(d);
                }
                while (true) {  // copy rest stream to output
                    const d = self.peekTo(S.TU) catch break;
                    self.output(d) catch break;
                    _ = self.readTo(S.TU) catch break;
                }
            }
            return self.ok();
        }

        // .read input ArrayList putting elements onto the pipe
        // .write clear the ArrayList, read elements from the pipe, appending to the ArrayList
        // .append read elements from the pipe, appending to the ArrayList
        // .pipe read an ArrayList(s) from the pipe and write the elements to the pipe.  Arguement should be null
        pub fn arrayList(self: *S, al: ?*std.ArrayList(T), e:enum{read, write, append, pipe}) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            if (e != .pipe) {
                if (val: { self.selectInput(0) catch break :val true; break :val false; }) { // output items of arrayList arg
                    std.debug.assert(e == .read);
                    for (al.?.items) |i| {
                        _ = try self.output(i);
                    }
                    
                } else { // read elements from pipe and append to passed arrayList
                    std.debug.assert(e == .write or e == .append);
                    self.err = error.ok;
                    loop: { 
                        var i: u32 = 0;
                        if (e == .write)
                            try al.?.resize(0);
                        while (true) : (i += 1) {
                            const d = self.peekTo(T) catch {
                                break :loop;
                            };
                            try al.?.append(d);
                            _ = try self.readTo(T);
                        }
                    }
                    // if output stream connected out a copy of the arrayList onto it
                    if (val: { self.selectOutput(0) catch break :val false; break :val true; }) {
                        var an = std.ArrayList(T).init(al.?.allocator); // use the original arraylist's allocator
                        try an.appendSlice(al.?.items);   
                        _ = try self.output(an);
                    } else
                        self.err = error.ok;
                
                }  
            } else { // read arrayList(s) from pipe and output elements
                std.debug.assert(al == null);
                self.err = error.ok;
                while (true) {
                    //std.debug.print("peekto ArrayList\n",.{});
                    const d = try self.peekTo(std.ArrayList(T));                    
                    // std.debug.print("ArrayList {any}\n",.{d});
                    for (d.items) |i| {
                        _ = try self.output(i);
                    }
                    _ = try self.readTo(std.ArrayList(T));
                }                
            } 
            return self.ok();
        }
        
        pub fn variable(self: *S, v: *T) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            if (val: { self.selectInput(0) catch { break :val true; }; break :val false; }) { 
                 _ = try self.output(v.*);
            } else {
                v.* = try self.peekTo(T);                
                while (true) {
                    const d = self.peekTo(T) catch break;
                    self.output(d) catch break;
                    _ = self.readTo(T) catch break;
                }
            }
            return self.ok();
        }

        pub fn console(self: *S) callconv(.Async) !void {
            defer 
                self.endStage();
            
            const stdout = std.io.getStdOut().writer();
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            while (true) {
                if (debugStages) std.log.info("console in {}_{s} {*}", .{ self.i, self.name, self.inC });
                if (try self.typeIs(T)) {
                    const e = try self.peekTo(T);
                    try stdout.print("{any} ", .{e});
                    _ = self.output(e) catch { self.err = error.ok; };
                    _ = try self.readTo(T);
                } else if (try self.typeIs(*[]T)) {
                    const e = try self.peekTo(*[]T);
                    try stdout.print("{any} ", .{e});
                    _ = self.output(e) catch { self.err = error.ok; };
                    _ = try self.readTo(*[]T);
                } else if (try self.typeIs(std.ArrayList(T))) {
                    const e = try self.peekTo(std.ArrayList(T));
                    try stdout.print("{any} ", .{e});
                    _ = self.output(e) catch { self.err = error.ok; };
                    _ = try self.readTo(std.ArrayList(T));
                } else {
                    const e = try self.peekTo(S.TU);
                    try stdout.print("{any} ", .{e});
                    _ = self.output(e) catch { self.err = error.ok; };
                    _ = try self.readTo(std.ArrayList(T));
                }
                
            }
            return self.ok();
        }
        
        pub fn fanin(self: *S) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });

            var done:bool = false;
            var s:u32 = try self.inStream();
            self.selectInput(s) catch |e| {
                if (e == error.noInStream) done = true;
            };
            while (!done) {
                if (debugStages) std.log.info("fanin {}_{s} {*} {*}", .{ self.i, self.name, self.inC, self.outC });
                while (true) {
                    const tmp = try self.peekTo(S.TU) catch |e| {
                        if (e == error.endOfStream) break else return e;
                    };
                    try self.output(tmp);
                    _ = try self.readTo(S.TU);
                }
                s += 1;
                self.selectInput(s) catch {
                    done = true;
                };
            }
            return;
        }
        
        pub fn faninany(self: *S) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });

            while (true) {
                _ = self.selectAnyInput() catch |e| {
                    if (e == error.endOfStream) continue else return e;
                };
                if (debugStages) std.log.info("faninany {}_{s} {*} {*}", .{ self.i, self.name, self.inC, self.outC });
                const tmp = try self.peekTo(S.TU);
                try self.output(tmp);
                _ = try self.readTo(S.TU);
            }
        }

        pub fn copy(self: *S) callconv(.Async) !void {
            defer
                self.endstage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });

            while (true) {
                const tmp = self.readTo(S.TU) catch break;
                self.output(tmp) catch break;
            }
            return self.ok();
        }

        pub fn gen(self: *S, limit: T) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });

            var i: T = 0;
            while (i < limit) : (i += 1) {
                if (debugStages) std.log.info("gen out {}_{s} {*}", .{ self.i, self.name, self.outC });
                try self.output(i);
            }
        }
    };
}
