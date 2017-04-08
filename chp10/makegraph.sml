structure MakeGraph :
sig
  val instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
end = 
struct
  structure G = Graph
  structure A = Assem

  fun instrs2graph instrs = 
    let
      val g = G.newGraph()
    in

    end
end
