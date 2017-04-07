structure Liveness :
sig
  structure G: GRAPH
  structure T: Temp
  datatype igraph = 
    IGRAPH of {graph: G.graph,
               tnode: T.temp -> G.node,
               gtemp: G.node -> T.temp,
               moves: (G.node * G.node) list }
  val interferenceGraph:
    Flow.flowgraph ->
      igraph * (G.node -> T.temp list)
  val show: TextIO.outstream * igraph -> unit
end =
struct
	(* Body *)
end