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

pub fn Filters(comptime StageType:type, comptime T:type) type {

    std.debug.assert(StageType.TU.inUnion(T)); 
    
    return struct {
    
        pub const S = StageType; 
        
// we need to return the global error set. When used, the fn type includes the name of the function.  We use this
// to set the name of the stage... 
        
        pub fn exactdiv(self:*S, d: T) callconv(.Async) !void {
            defer { self.endStage(); }
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            while (true) {
                if (debugStages) std.log.info("div pre peek {}_{s} {*}", .{ self.i, self.name, self.outC });
                var i = try self.peekTo(u64);
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
                _ = try self.readTo(u64);
            }
        }
        
        pub fn slice(self:*S, slc:*[]T) callconv(.Async) !void {
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
                        const d = self.peekTo(T) catch { break :loop; };
                        slc.len = i+1;
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
        
        pub fn arrayList(self:*S, al:*T) callconv(.Async) !void {
            defer { self.endStage(); }
            var input:bool = false;
            _ = self.selectInput(0) catch |err| { if (err == error.noInStream) input = true; };
            const D = std.meta.Elem(al.items);
            if (input) {
                for (al.items) |d| {
                    _ = try self.output(d); 
                }
            } else { 
                loop: {                     // probably should use an arraylist here and slice the results
                    var i:u32 = 0;
                    al.resize(0);
                    while (true) : (i += 1) {
                        const d = self.peekTo(D) catch { break :loop; };
                        try al.append(d);
                        _ = try self.readTo(D);
                    }                
                    return error.outOfBounds;
                }
                var an = try std.arrayList(D).init(al.allocator);   // use the original arraylist's allocator
                an.appendSlice(al.items);
                _ = self.output(an) catch {};
            }
            return;
        }

        pub fn console(self:*S) callconv(.Async) !void {
            defer { self.endStage(); }
            const stdout = std.io.getStdOut().writer();
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            while (true) {
                if (debugStages) std.log.info("con in {}_{s} {*}", .{ self.i, self.name, self.inC });
                const e = try self.peekTo(T);
                try stdout.print("{any} ",.{e});
                _ = self.output(e) catch {};
                _ = try self.readTo(T);
            }
        }

        pub fn fanin(self:*S) callconv(.Async) !void {
            defer { self.endStage(); }
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            while (true) {
                _ = self.selectAnyInput() catch |e| {
                    if (e == error.endOfStream) continue else return e;
                };
                if (debugStages) std.log.info("fan {}_{s} {*} {*}", .{ self.i, self.name, self.inC, self.outC });
                const tmp = try self.peekTo(S.TU);
                try self.output(tmp);
                _ = try self.readTo(S.TU);
            }
        }
        
        pub fn copy(self:*S) callconv(.Async) !void {
            defer { self.endstage(); }
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            while (true) {
                const tmp = try self.readTo(S.TU);
                try self.output(tmp);
            }
        }

        pub fn gen(self:*S, limit: T) callconv(.Async) !void {
            defer { self.endStage(); }
            if (debugStart) std.log.info("start {}_{s}", .{ self.i, self.name });
            
            var i:T = 0;
            while (i < limit) : (i += 1) {
                if (debugStages) std.log.info("gen out {}_{s} {*}", .{ self.i, self.name, self.outC });
                try self.output(i);
            }
        }
    
    };
}
 
