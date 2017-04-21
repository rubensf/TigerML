signature REGALLOC = 
sig
  structure F : FRAME
  structure C : COLOR
  type allocation = C.allocation
  val alloc : Assem.instr list * F.frame -> Assem.instr list * allocation
end

functor RegAlloc(F: FRAME) :> REGALLOC = 
struct
  structure F = F
  structure C = Color(F)
  type allocation = C.allocation

  fun alloc (instrs, frame) =
    let
      val flowgraph = MakeGraph.instrs2graph instrs
      val (igraph, liveOutMap) = Liveness.interferenceGraph flowgraph
      val (allocation, spills) = C.color {igraph=igraph, initial=C.emptyAlloc, spillCost=(fn x => 1), registers=C.F.registers}
    in
      (instrs, allocation)
    end
end
