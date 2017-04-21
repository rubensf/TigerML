signature REGALLOC = 
sig
  structure Frame : FRAME
  structure C : COLOR
  type allocation = C.allocation
  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc :> REGALLOC = 
struct
  structure Frame = MipsFrame
  structure T = Temp 
  structure C = Color(Frame)
  type allocation = C.allocation

  fun alloc (instrs, frame:Frame.frame) = 
    let
      val flowgraph = MakeGraph.instrs2graph instrs
      val (igraph, liveOutMap) = Liveness.interferenceGraph flowgraph
      val (allocation, spills) = C.color {igraph=igraph, initial=C.F.tempMap, spillCost=(fn x => 1), registers=C.F.registers}
    in
      (instrs, allocation)
    end
end