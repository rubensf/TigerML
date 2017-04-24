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

## Basic Blocks during Control Flow
We construct our flow graph using basic blocks rather than instruction by instruction for increased efficiency running control flow algorithms.

## Coalescing and Spilling
We try to remove all unnecessary move operations, and spill some registers to the frame when you can't color all conflicting registers.
