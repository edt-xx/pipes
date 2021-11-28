const std = @import("std");

const pipe = struct {
    usingnamespace @import("pipes");
    usingnamespace @import("filters");
};   

// two context structs (must be pub for cross file access)
pub const x = struct {
    pub var xxx: u16 = 5;
    pub var aaa: u16 = 50;
    pub var ar = [_]u64{ 11, 22, 33, 44 };
    pub var sss: []u64 = &ar;
};

pub var testme: u64 = 38;

pub var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
pub var arena2 = std.heap.ArenaAllocator.init(std.heap.page_allocator);

pub fn main() !void {
    
    defer arena.deinit();
    defer arena2.deinit();
    const allocator = &arena.allocator;     // pipe allocator
    const allocator2= &arena2.allocator;    // misc storage allocator

                                    // stage instances are always 'owned' by a pipe instance, but the PipeType and instance
    comptime var Make = pipe.Z{};   // are defined after the Stages.  This structure is used to save PipeTypes as they are
                                    // created and is used by Stage to reconstruct the PipeType and instance pointer.

    // create a Stage type with U64, []u64 and ArrayList(u64) as a TypeUnion
    const F = pipe.Filters(Make.Stage(.{ u64, []u64, std.ArrayList(u64), []const u8 }), u64);
    const S = pipe.Filters(F.S, []const u8);
    
    // create a varient Stage type with ArrayList(u64) as its primary type, sharing the TypeUnion defined by F
    const A = pipe.Filters(F.S, std.ArrayList(u64));
    
    const y = struct {
        pub var xxx: u16 = 17; // must be pub
        pub var aaa: u16 = 80;
        pub var alist = std.ArrayList(u64).init(allocator2);
        pub var blist = std.ArrayList(u64).init(allocator2);
        pub var prime = [_]u64{1, 2, 3, 5, 7, 11, 13};
        pub var fib = [_]u64{1, 1, 2, 3, 5, 8, 13, 22, 35};
        pub var init: []u64 = prime[1..];
        pub var list: []F.S.TU = undefined;
    };
    
    try y.alist.appendSlice(&y.prime);
    y.list = F.S.TU.list(allocator2, .{u64, []u64, @TypeOf(y.alist), []const u8}, .{51, y.fib[0..], y.alist, "test"});
    
        try Make.run(@This(), .{
        .{ F.gen, .{35} }, 
        .{ .not3, F.exactdiv, .{3} },
        .{ .by3not5, F.exactdiv, .{5} },
        .{ S.replace, .{ @as([]const u8, "\'FizzBuzz")} },    // Divisible by 3 and 5
        .{ .all, F.faninany },
        .{ F.console, ._ },
        .{ .by3not5 },
        .{ S.replace, .{ @as([]const u8, "\'Fizz")} },        // Divisible by 3 but not 5
        .{ .all, ._ },
        .{ .not3 },
        .{ .not3not5, F.exactdiv, .{5} },                     // not divisible by 3, check 5
        .{ S.replace, .{ @as([]const u8, "\'Buzz")} },
        .{ .all, ._ },
        .{ .not3not5 },                                       // route numbers back to faninany 
        .{ .all, ._ },
    });
    std.debug.print("\n", .{}); 
    
    try Make.run(y, .{
        .{ F.typeUnion, .{ "list" } },      // put the elements of the array of TypeUnions onto the pipe
        .{ .a, F.collateTypes },            // split by types in typeunion (u64 is stream 0)
        .{ .b, F.faninany },
        .{ F.console, ._ },                 // display the number
        .{ .a },                        
        .{ F.slice, .{null} },              // read the slice and output elements
        .{ .b, ._ },
        .{ .a },                            // trinary output of collateTypes (std.ArrayList(u64))
        .{ F.arrayList, .{null, .pipe} },   // elements of the arraylist are put onto the pipe
        .{ .b, ._ },                        // and sent to secondary input of faninany
        .{ .a },                            // just pass a string to faninany
   //     .{ F.copy},
        .{ .b, ._ },
    });
    std.debug.print("\n", .{});
    
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

    // create a new PipeType and create an instance using allocator
    var myPipe = Make.Mint(aPipe).init(allocator);

    // run the pipe with the above args, the values from x are assigned to the pipe stages in .run(x)
    // x.aaa=50, x.xxx=5
    try myPipe.run(x);
    std.debug.print("\n", .{});  
    
    // play with vars in pipe context structures
    x.xxx += 1;
    x.aaa += 50;
    
    // call using helper function
    // x.aaa=110, x.xxx=6
    try Make.run(x,aPipe);
    std.debug.print("\n", .{});
    
    // and using a different context structure
    // y.aaa=80, y.xxx=17
    try myPipe.run(y);
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
    
        const pSlicer = .{
        .{ F.slice, .{"sss"} }, // extract elements of slice sss and pass to console
        .{ F.console, ._ },
    };

    var sPiper = Make.Mint(pSlicer).init(allocator);
    
    try sPiper.run(x);
    std.debug.print("\n", .{});

    _ = testme;
    const pNoContect = .{ 
        .{ F.gen, .{"testme"} },
        .{ F.console, ._ }, // update slice with generated values
    };

    try Make.Mint(pNoContect).init(allocator).run(@This());
    std.debug.print("\n", .{});
    
    //try y.alist.appendSlice(&y.prime);
    const pReadAL = .{
        .{ F.arrayList, .{"alist", .read} },
        .{ F.slice, .{"init"} },                // output elements of alist into pipe
        .{ F.arrayList, .{"blist", .write} },   // assemble blist from pipe
        .{ F.arrayList, .{null, .pipe} },       // read copy of blist from pipe and output elements
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
        
    try Make.run(y, .{
        .{ F.typeUnion, .{ "list" } },  // put the elements of the array of TypeUnions onto the pipe
        .{ .a, F.collateTypes },        // split by types in typeunion (u64 is stream 0)
        .{ .b, F.faninany },
        .{ F.console, ._ },             // display the number
        .{ .a, ._},                     // seconday output of collateTypes is ignored ([]u64)
        .{ .a },                        // trinary output of collateTypes (std.ArrayList(u64))
        .{ F.arrayList, .{null, .pipe} },      // elements of the arraylist are put onto the pipe
        .{ .b, ._},                     // and sent to secondary input of faninany
    });
    std.debug.print("\n", .{});    
    
}
