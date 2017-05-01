# Tiger Compiler
One more Tiger compiler written in SML, as an exercise for the book "Modern Compiler Implementation in ML" from Andrew W. Appel.

# Usage
The following will create a tig.s, runnable on spim.
```sml
$ sml
>> CM.make "sources.cm";
>> Main.compile "file.tig";
```
If you want debugging info,
```sml
$ sml
>> CM.make "sources.cm";
>> Main.compileverb "file.tig" 3;
```

# Optimizations List
We included the following optimizations:

## Constant Folding
All basic operations between constants are handled at compile time rather runtime. Including
* Plus
* Minus
* Multiply
* Divide
* And
* Or
* Xor
* LShift
* RShift
* ARShift

## Register Coalescing & Spilling
We implement coalescing and spilling.
Our spilling goes through all instructions and checks for all defs and uses
of a temporary - then we fetch them after every def and store them before every
use. This isn't the most efficient way of spilling but it's effective in solving
spilling problems.
We pick spills choosing the nodes with the least amount of interferences, so
that the nodes with the maximum amount of interferings go on top of stack and
definitely spilled.
We also add moves of $s regs on prologues and epilogues. The register allocator
will get rid of those moves if there's a need for a $s register, and will get
use them first if spilling requires survival of registers accross procedure
calls (more efficient than fetching/storing all the time).

## Bounds & nil checking
Bounds checking in array access and nill checking on record access.
So far, no error message is given, but the program exits silently.

## Zero constant & heuristic optimizations per architecture
We use SML pattern matching to optimize a series of instruction selection, whenever possible.
For instance, we match specifically MOVE(MEM(BINOP(oper, e1, CONST i)), e2) to come out "sw e2 i(e1)"

## Basic Blocks during Control Flow
We construct our flow graph using basic blocks rather than instruction by instruction for increased efficiency running control flow algorithms.

## Support for String Comparison
Including =, <>, >=, >, <=, <. We made our own strcmp for that.

## Optimized runtime
We compiled our own runtime with -O3, making sure all calls were external
so that calling convetions would be obeyed. We also removed the "clutter" of
directives not recognized by spim - that reduced our runtime to about 440 lines
of instructions, favoring instruction cache.
Note: Debugging stuff with -O3 somehow put a lot of instructions out of order,
ie jump before updating the counter. Not sure why this happened, but it was a
lot of trouble debugging the mips :( plz rewards us

# Known bugs
Our bounds checking creates a CJUMP, but it creates the label at creation time
rather than at usage time (as happens when unEx (Cx _)). As such, having nested
accesses, as is the case in test42.tig, will crash - you try to jump to the same
labels from when you created the expression, rather than new ones.
I arduously tried to fix it but just couldn't on time.