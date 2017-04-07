structure Liveness :
sig
  structure G: GRAPH
  structure T: TEMP
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
  structure G = Graph
  structure T = Temp
  datatype igraph = 
    IGRAPH of {graph: G.graph,
               tnode: T.temp -> G.node,
               gtemp: G.node -> T.temp,
               moves: (G.node * G.node) list }
  type liveSet = unit T.Table.table * T.temp list
  type liveMap = liveSet G.Table.table

  fun show(outstream, IGRAPH {graph = graph, tnode = tnode, gtemp = gtemp, moves = moves}) =
    let
    in
      TextIO.output(outstream, )
    end

  fun interferenceGraph(Flow.FGRAPH{control, def, use, ismove}) = 
end