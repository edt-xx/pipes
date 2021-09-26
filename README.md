So what are pipes?  They are a set of co-routines that pass values via connections.  In the simplest case there is only one connection.  An example, ignoring types for now:
```
const simplePipe = .{ .{gen, .{100}},  .{console, ._}}
```
Here we have a filter (co-routine) call gen which gets started with a argument of 100.  The code of gen just 'output's integers from 0 to 99, passing each value, in turn, to the connection.  The console filter waits for a value, reads it  from the connection and writes it to the console .   A few more details.  Each step in a pipe is called a stage and consists of the filter, arguments and connections.  When a filter is started it is passed an additional argument, a pointer to the Stage structure.  The Stage structure has methods like 'output' which puts a value into the current output connection.  There can be several output connections and you can select which one is active using 'selectOutput'.  The same is true for input connections, which can be looked at with 'typeTo' to see the type waiting to be examined, 'peekTo' to get the value with consuming  it, and 'readTo' which gets and consumes the value allowing another stage's filter to 'output' another value to the connection.  With input connectons you can also use 'selectInput' to choose the active connection.  You can also ask the pipeline to 'selectAnyInput' which select a connection with input (see filter fanin).

We add multiple connections using labels.  Lets look at an example:
```
const pipe = .{ 
        .{ gen, .{30} },              // generate a steam of numbers and pass each to the next stage
        .{ .a, exactdiv, .{5}},       // exact goes to fanin, otherwise to exactdiv(7) via .a
        .{ .b, fanin },               // take input from any stream and pass it to console
        .{ console, ._ },             // output to console
        .{ .a },
        .{ .c, exactdiv, .{7}},       // exact goes via label .b to fanin
        .{ .b, ._ },
};
```
Here we generate integers from 0 to 29 and pass them to the exactdiv filter.  This filter will output to its primary output connection (also called a stream) when an interger is divisible by the argument, and to its secondary stream when there is a remainder.  The second stage starts with a label (.a) this allows us to connect a secondary stream.  which is what is happening in the last couple  of lines of the pipe, .{.a} connects the secondary output of exactdiv(5) to the primary input of exactdiv(7) and the .{.b, ._} connects the primary output of exactdiv(7) to the secondary input of .{ .b, fanin}.  The ._  indicates an end of pipe, or as in the console stage, that there is no connection to the primary output stream.  The end result get passed to console and we end up with: 5 7 10 14 15 20 21 25 28 on the console.

Of course zig is heavily typed.  So its not as simple to call a pipe as it is in CMS Pipelines.  So here is a real example.  First we setup a struct to contain variables to be read by the pipe.  Then we tell the pipe that we are going to use type u64 or i64 (uP) and create a instance of the filters using u64 (f).  Next comes the pipe definition as above using the (f.) prefix.  The last statement creates a PipeInstance with the StageType uP for our pipe, sets it up with an allocator, and runs it passing the context struct.   The results will be the same as above.
```
const context = struct {
    pub var xxx: u16 = 5;
    pub var aaa: u16 = 30;
};
const uP = StageType(.{u64, i64});
const f = Filters(uP,u64);
const pipe = .{ 
        .{ f.gen, .{"aaa"} },           // generate a steam of numbers and pass each to the next stage
        .{ .a, f.exactdiv, .{"xxx"}},   // exact goes to fanin, otherwise to exactdiv(7) via .a
        .{ .b, f.fanin },               // take input from any stream and pass it to console
        .{ f.console, ._ },             // output to console
        .{ .a },
        .{ .c, f.exactdiv, .{7}},       // exact goes via label .b to fanin
        .{ .b, ._ },
};  
try PipeInstance(uP, pipe).setup(allocator).myPipe.run(context);
```

Zig imposes constraints.  To create pipes within these limits requires a bit of work.  We need to define what gets passed on connections.  The connection structure (Conn) must be runtime so it cannot use anytype.  Since we will want to send different types thru the same connections I created the TypeUnion struct (tagged Unions did not quite fit).   The StageType defines the types required for Connections and the TypeUnion they use.  We cannot use std.meta.ArgsTuple with generic functions.  This, and the lack of function name overloading (not a bad thing) is what triggers the requirement to setup Filters.  To process variables in pipes @field is used.  This requires a struct with pub variables.  Alternately you can use pub global vars and @This() for context.   PipeInstance returns a type so the setup function is need to create the an instance which cannot be allocated on the stack so an allocator is needed.   

The PipeInstance args function also gets complicated due to known problems with runtime values in multilevel tuples in Stage 1.

Does this Help?  What questions does it raise?
