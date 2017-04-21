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
      fun spillCost temp = 1
        (*let 
          fun f (s,t) = if List.exists (fn x => x=t) s then 1 else 0
          val Liveness.IGRAPH{graph=graph,...} = igraph
          val dus = foldl (fn ({def,use}, ans) => ans + f(def,temp) + f(use,temp)) 0 graph
          val Liveness.I.NODE{temp,adj,status} : Liveness.I.node =
            case List.find
              (fn Liveness.I.NODE{temp=t,...} => t = temp) igraph
            of SOME n => n
             | NONE  => ErrorMsg.impossible("Spill Cost.")
          val interferes = List.length !adj
        in
          (Real.fromInt(num_du) / Real.fromInt(interferes))
        end*)
      val (allocation, spills) = C.color {igraph=igraph, 
                                          initial=C.emptyAlloc,
                                          spillCost=spillCost, 
                                          registers=C.F.colorRegisters}
    in
      (instrs, allocation)
    end
end
