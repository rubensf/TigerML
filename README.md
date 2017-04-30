# Tiger Compiler
One more Tiger compiler written in SML, as an exercise for the book "Modern Compiler Implementation in ML" from Andrew W. Appel.

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
