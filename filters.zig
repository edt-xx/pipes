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

pub fn Filters(comptime StageType: type, comptime T: type) type {

    std.debug.assert(StageType.TU.inUnion(T));

    return struct {
        pub const S = StageType;

        // we need to return the global error set. When used, the fn type includes the name of the function.  We use this
        // to set the name of the stage...

        pub fn exactdiv(self: *S, d: T) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            while (true) {
                if (debugStages) std.log.info("div pre peek {}_{s} {*}", .{ self.i, self.name, self.outC });
                var i = try self.peekTo(u64);
                //if (debugStages) std.log.info("div post peek {}_{s} {}",.{self.i, self.name, i});
                if (i % d == 0)
                    try self.selectOutput(0)
                else
                    self.selectOutput(1) catch |err| 
                        if (err != error.noOutStream) return err;                    
                if (debugStages) std.log.info("div out {}_{s} {*}", .{ self.i, self.name, self.outC });
                _ = self.output(i) catch |err| 
                    if (err != error.noOutStream) return err;
                
                _ = try self.readTo(u64);
            }
        }

        pub fn slice(self: *S, slc: *[]T) callconv(.Async) !void {
            defer 
                self.endStage();
            
            var input: bool = false;
            _ = self.selectInput(0) catch |err| {
                if (err == error.noInStream) input = true;
            };
            if (input) {
                for (slc.*) |d| {
                    _ = try self.output(d);
                }
            } else {
                loop: { // probably should use an arraylist here and slice the results
                    var i: u32 = 0;
                    const max = slc.len; // when updating, do not exceed the initial length of the slice
                    slc.len = i;
                    while (i < max) : (i += 1) {
                        const d = self.peekTo(T) catch {
                            break :loop;
                        };
                        slc.len = i + 1;
                        slc.*[i] = d;
                        _ = try self.readTo(T);
                    }
                    return error.outOfBounds;
                }
                var cpy: @TypeOf(slc) = try self.allocator.create(@TypeOf(slc.*));
                for (slc.*) |e, i| {
                    cpy.*[i] = e;
                }
                _ = self.output(cpy) catch {};
            }
            return;
        }

        // if in has elements of type T assemble them into an ArrayList, save in al and pass a copy al to out 0
        // if in is not connected, output the elements in al to out 0
        // if in has elements of type ArrayList(T), read arrayList and output elements.  al should be null.
        pub fn arrayList(self: *S, al: ?*std.ArrayList(T)) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (val: { self.selectInput(0) catch break :val true; break :val false; }) { // output elements of passed arrayList
                for (al.?.items) |d| {
                    _ = try self.output(d);
                }
                
            } else if (try self.typeIs(T)) { // read elements from pipe and append to passed arrayList
                loop: { 
                    var i: u32 = 0;
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
                }
               
            } else if (try self.typeIs(std.ArrayList(T))) { // read arrayList(s) from pipe and output elements
                std.debug.assert(al == null);
                loop: { 
                    while (true) {
                        const d = self.peekTo(std.ArrayList(T)) catch {
                            break :loop;
                        };
                        for (d.items) |e| {
                            _ = try self.output(e);
                        }
                        _ = try self.readTo(std.ArrayList(T));
                    }
                } 
                
            } else
                unreachable;
            return;
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
                    _ = self.output(e) catch {};
                    _ = try self.readTo(T);
                } else if (try self.typeIs(*[]T)) {
                    const e = try self.peekTo(*[]T);
                    try stdout.print("{any} ", .{e});
                    _ = self.output(e) catch {};
                    _ = try self.readTo(*[]T);
                } else if (try self.typeIs(std.ArrayList(T))) {
                    const e = try self.peekTo(std.ArrayList(T));
                    try stdout.print("{any} ", .{e});
                    _ = self.output(e) catch {};
                    _ = try self.readTo(std.ArrayList(T));
                } else 
                    unreachable;
                
            }
        }

        pub fn fanin(self: *S) callconv(.Async) !void {
            defer 
                self.endStage();
            
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });

            var done:bool = false;
            var s:u32 = 0;
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
                const tmp = try self.readTo(S.TU);
                try self.output(tmp);
            }
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
