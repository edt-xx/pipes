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
    pub var sss: []u64 = &ar;
};

pub var testme: u64 = 50;

pub var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
pub var arena2 = std.heap.ArenaAllocator.init(std.heap.page_allocator);

pub fn main() !void {
    
    defer arena.deinit();
    defer arena2.deinit();
    const allocator = &arena.allocator;     // pipe allocator
    const allocator2= &arena2.allocator;    // misc storage allocator

    x.aaa += 3;
    
    comptime var Make = pipe.Z{};

    // create pipe commands and stages for type u128
    //const pTypes = .{ u64, *[]u64 };
    const F = pipe.Filters(Make.Stage(.{ u64, []u64, std.ArrayList(u64) }), u64);
    const A = pipe.Filters(F.S, std.ArrayList(u64)); // set of filters with std.ArrayList(u64) as type
    
    //var xxx:u64 = 75;

    // a sample pipe using uP stages, note that ._ indicates stage or connector's output is not connected
    const aPipe = .{ 
        .{ F.gen, .{"aaa"} },           // generate a steam of numbers and pass each to the next stage
        .{ .a, F.exactdiv, .{"xxx"} },  // exact goes to fanin, otherwise to exactdiv(7) via .a
        .{ .b, F.faninany },            // take input from any stream and pass it to console
        .{ F.console, ._ },             // output to console
        .{.a },
        .{ .c, F.exactdiv, .{7} },      // exact goes via label .b to fanin, otherwise to exactdiv(11) via .c
        .{ .b, ._ },
        .{.c },
        .{ F.exactdiv, .{11} },         // exact goes via label b to fanint, otherwise ignore number
        .{ .b, ._ },
    };

    // play with a vars
    x.xxx += 1;
    x.aaa += 7;

    // create a container for this pipe 
    var myPipe = Make.Mint(aPipe).init(allocator);

    // run the pipe with the above args
    try myPipe.run(x);
    std.debug.print("\n", .{});   
    
    try Make.run(x,aPipe);
    std.debug.print("\n", .{});

    x.aaa = 50;
    x.xxx = 13;

    // run again with updated args

    const y = struct {
        pub var xxx: u16 = 17; // must be pub
        pub var aaa: u16 = 80;
        pub var alist = std.ArrayList(u64).init(allocator2);
        pub var blist = std.ArrayList(u64).init(allocator2);
        pub var prime = [_]u64{1, 2, 3, 5, 7, 11, 13};
        pub var init: []u64 = prime[1..];
        pub var list: []F.S.TU = undefined;
    };
    //defer y.alist.deinit();
    //defer y.alist.deinit();
    

    // and using a different context structure
    try myPipe.run(y);
    std.debug.print("\n", .{});

    //arena.deinit();
    
    const pSlicer = .{
        .{ F.slice, .{"sss"} }, // extract elements of slice sss and pass to console
        .{ F.console, ._ },
    };

    var sPiper = Make.Mint(pSlicer).init(allocator);
    
    try sPiper.run(x);
    std.debug.print("\n", .{});

    const pSlicew = .{ 
        .{ F.gen, .{"sss.len"} },
        .{ F.slice, .{"sss"} }, // update slice with generated values
        .{ F.console, ._ }
    };

    var sPipew = Make.Mint(pSlicew).init(allocator);
    // update the slice
    try sPipew.run(x);
    std.debug.print("\n", .{});
    
    _ = testme;
    const pNoContect = .{ 
        .{ F.gen, .{"testme"} },
        .{ F.console, ._ }, // update slice with generated values
    };

    try Make.Mint(pNoContect).init(allocator).run(@This());
    std.debug.print("\n", .{});
    
    try y.alist.appendSlice(&y.prime);
    const pReadAL = .{
        .{ F.arrayList, .{"alist", .read} },
        .{ F.slice, .{"init"} },        // output elements of alist into pipe
        .{ F.arrayList, .{"blist", .write} },   // assemble blist from pipe
        .{ F.arrayList, .{null, .pipe} },      // read copy of blist from pipe and output elements
        .{ F.console, ._ },
    };
    
    try Make.run(y, pReadAL);
    std.debug.print("\n", .{});
    
    try Make.run(y, .{
        .{ A.variable, .{"blist"} },           // put arraylist blist into pipe 
        .{ F.arrayList, .{null, .pipe} },      // output elements of blist 
        .{ F.console, ._ },
    });
    std.debug.print("\n", .{});
    
    y.list = F.S.TU.list(allocator2, .{u64, []u64, @TypeOf(y.alist)}, .{51, y.prime[1..4], y.alist});
    //defer gpaAlloc.free(y.list);
    // std.debug.print("{any}\n",.{y.list});
    
    try Make.run(y, .{
        .{ F.typeUnion, .{ "list" } },  // put the elements of the array of TypeUnions onto the pipe
        .{ .a, F.collateTypes },        // split by types in typeunion (u64 is stream 0)
        .{ .b, F.faninany },
        .{ F.console, ._ },             // display the number
        .{ .a, },                       // seconday output of collateTypes is ignored ([]u64)
        .{ F.slice, .{null} },          // read the slice and output elements
        .{ .b, ._ },
        .{ .a },                        // trinary output of collateTypes (std.ArrayList(u64))
        .{ F.arrayList, .{null, .pipe} },      // elements of the arraylist are put onto the pipe
        .{ .b, ._},                     // and sent to secondary input of faninany
    });
    std.debug.print("\n", .{});
        
    try Make.run(y, .{
        .{ F.typeUnion, .{ "list" } },  // put the elements of the array of TypeUnions onto the pipe
        .{ .a, F.collateTypes, ._ },    // split by types in typeunion (u64 is stream 0)
        .{ .b, F.faninany },
        .{ F.console, ._ },             // display the number
        .{ .a, ._},                     // seconday output of collateTypes is ignored ([]u64)
        .{ .a },                        // trinary output of collateTypes (std.ArrayList(u64))
        .{ F.arrayList, .{null, .pipe} },      // elements of the arraylist are put onto the pipe
        .{ .b, ._},                     // and sent to secondary input of faninany
    });
    std.debug.print("\n", .{});    
        
}
