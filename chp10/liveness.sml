structure Liveness :
sig
  structure TempOrdKey: ORD_KEY
  structure FG: FUNCGRAPH
  structure T: TEMP
  datatype igraph = 
    IGRAPH of {graph: T.temp FG.graph,
               tnode: T.temp -> T.temp FG.node,
               gtemp: T.temp FG.node -> T.temp,
               moves: (T.temp FG.node * T.temp FG.node) list }
  (*val interferenceGraph: Flow.flowgraph -> igraph * (FG.node -> T.temp list)*)
  val show: TextIO.outstream * igraph -> unit
end =
struct
  structure TempOrdKey = struct type ord_key = Temp.temp
                                val compare = Temp.compare 
                         end
  structure LabelOrdKey = struct type ord_key = Temp.label
                                 val compare = Symbol.compare
                          end
  structure NodeMap = SplayMapFn(LabelOrdKey)
  structure FG = FuncGraph(TempOrdKey)
  structure T = Temp
  datatype igraph = 
    IGRAPH of {graph: T.temp FG.graph,
               tnode: T.temp -> T.temp FG.node,
               gtemp: T.temp FG.node -> T.temp,
               moves: (T.temp FG.node * T.temp FG.node) list }
  (* Set of live variables *)
  type liveSet = T.Set.set
  (* Map from flow graph node to live temporaries at that node *)
  type liveMap = liveSet NodeMap.map
  (* TODO *)
  fun show(outstream, IGRAPH {graph = graph, tnode = tnode, gtemp = gtemp, moves = moves}) =
    let
    in
      TextIO.output(outstream, "hello")
    end
  (* TODO *)
  (*fun interferenceGraph(Flow.FGRAPH{control, def, use, ismove}) = *)
end