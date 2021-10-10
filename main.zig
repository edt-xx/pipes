const std = @import("std");

const pipe = struct {
    usingnamespace @import("pipes");
    usingnamespace @import("filters");
};

// two context structs (must be pub for cross file access)
pub const x = struct {
    pub var xxx: u16 = 5;
    pub var aaa: u16 = 100;
    pub var ar = [_]u64{ 11, 22, 33, 44 };
    pub var sss: []u64 = ar[0..];
};

pub var testme: u64 = 50;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    x.aaa += 3;

    // create pipe commands and stages for type u128
    //const pTypes = .{ u64, *[]u64 };
    const f = pipe.Filters(pipe.Stage(.{ u64, *[]u64 }), u64);
    //const g = pipe.myFilters(f.S, u64);
    
    //var xxx:u64 = 75;

    // a sample pipe using uP stages, note that ._ indicates stage or connector's output is not connected
    const aPipe = .{
        .{ f.gen, .{"aaa"} },           // generate a steam of numbers and pass each to the next stage
        .{ .a, f.exactdiv, .{"xxx"} },  // exact goes to fanin, otherwise to exactdiv(7) via .a
        .{ .b, f.faninany },            // take input from any stream and pass it to console
        .{ f.console, ._ },             // output to console
        .{.a},
        .{ .c, f.exactdiv, .{7} },      // exact goes via label .b to fanin, otherwise to exactdiv(11) via .c
        .{ .b, ._ },
        .{.c},
        .{ f.exactdiv, .{11} },         // exact goes via label b to fanint, otherwise ignore number
        .{ .b, ._ },
    };

    // play with a vars
    x.xxx += 1;
    x.aaa += 7;

    // create a container for this pipe with type uP and setup the pipe's structure
    var myPipe = pipe.Mint(aPipe).init(allocator);

    // run the pipe with the above args
    try myPipe.run(x);
    std.debug.print("\n", .{});   
    
    try pipe.run(x,aPipe);
    std.debug.print("\n", .{});

    x.aaa = 50;
    x.xxx = 13;

    // run again with updated args
    try myPipe.run(x);
    std.debug.print("\n", .{});

    const y = struct {
        pub var xxx: u16 = 17; // must be pub
        pub var aaa: u16 = 80;
    };

    // and using a different context structure
    try myPipe.run(y);

    const pSlicer = .{
        .{ f.slice, .{"sss"} }, // extract elements of slice sss and pass to console
        .{ f.console, ._ },
    };

    std.debug.print("\n", .{});

    var sPiper = pipe.Mint(pSlicer).init(allocator);
    try sPiper.run(x);

    // const g = Filters(uP, *[]u64);      // for later
    const pSlicew = .{ 
        .{ f.gen, .{"ar.len"} },
        .{ f.slice, .{"sss"}, ._ }, // update slice with generated values
    };

    std.debug.print("\n", .{});

    var sPipew = pipe.Mint(pSlicew).init(allocator);

    // update the slice
    try sPipew.run(x);

    // output the updated slice
    try sPiper.run(x);

    _ = testme;
    const pNoContect = .{ 
        .{ f.gen, .{"testme"} },
        .{ f.console, ._ }, // update slice with generated values
    };

    try pipe.Mint(pNoContect).init(allocator).run(@This());

    std.debug.print("\n", .{});
}
