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
  structure NodeMap = Flow.NodeMap
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

  fun computeLiveSets(Flow.FGRAPH{control, def, use}) = 
    let
      val nodes = Flow.FG.nodes control
      val liveIn:liveMap = foldl (fn (x,ans) => NodeMap.insert(ans, Flow.FG.getNodeID x, T.Set.empty)) NodeMap.empty nodes
      val liveOut:liveMap = foldl (fn (x,ans) => NodeMap.insert(ans, Flow.FG.getNodeID x, T.Set.empty)) NodeMap.empty nodes
      fun loopUntilDone(nodes, liveIn, liveOut, true) = {inMap=liveIn, outMap=liveOut}
        | loopUntilDone(nodes, liveIn, liveOut, false) = 
            let
              val liveIn' = foldl (fn (x, ans) => NodeMap.insert(ans, Flow.FG.getNodeID x, Option.valOf(NodeMap.find(liveIn, Flow.FG.getNodeID x)))) NodeMap.empty nodes
              val liveOut' = foldl (fn (x,ans) => NodeMap.insert(ans, Flow.FG.getNodeID x, Option.valOf(NodeMap.find(liveOut, Flow.FG.getNodeID x)))) NodeMap.empty nodes
              fun updateSets (x, {li, lo}) = 
                let
                  val useSet = Option.valOf(NodeMap.find(use, Flow.FG.getNodeID x))
                  val outSet = Option.valOf(NodeMap.find(lo, Flow.FG.getNodeID x))
                  val defSet = Option.valOf(NodeMap.find(def, Flow.FG.getNodeID x))
                  val newInSet = T.Set.union(useSet, T.Set.difference(outSet, defSet))
                  val li' = NodeMap.insert(li, Flow.FG.getNodeID x, newInSet)
                  val succIDs = Flow.FG.succs x
                  val newOutSet = foldl (fn (xID, ans) => T.Set.union(ans, Option.valOf(NodeMap.find(li', xID)))) T.Set.empty succIDs
                  val lo' = NodeMap.insert(lo, Flow.FG.getNodeID x, newOutSet)
                in
                  {li=li, lo=lo}
                end
              val both = foldl updateSets {li=liveIn, lo=liveOut} nodes
              val liveIn'' = #li both
              val liveOut'' = #lo both
              val checkIn = foldl (fn (x,ans) => T.Set.equal(Option.valOf(NodeMap.find(liveIn'', Flow.FG.getNodeID x)), Option.valOf(NodeMap.find(liveIn', Flow.FG.getNodeID x))) andalso ans) true nodes
              val checkOut = foldl (fn (x,ans) => T.Set.equal(Option.valOf(NodeMap.find(liveOut'', Flow.FG.getNodeID x)), Option.valOf(NodeMap.find(liveOut', Flow.FG.getNodeID x))) andalso ans) true nodes
            in
              loopUntilDone(nodes, liveIn'', liveOut'', checkIn andalso checkOut)
            end
    in
      loopUntilDone (nodes, liveIn, liveOut, false)
    end
  fun createInterferenceGraph (liveOut:liveMap, def, control) = 
    let
      val nodes = Flow.FG.nodes control
      fun f (x, ans: T.temp FG.graph) = 
        let
          val defs = Option.valOf(NodeMap.find(def, Flow.FG.getNodeID x))
          val live = Option.valOf(NodeMap.find(liveOut, Flow.FG.getNodeID x))
          val temps = map FG.nodeInfo (FG.nodes ans)
          fun add (t, ans) = 
            let
              fun eq x = 
                case T.compare(x,t) of
                  EQUAL => true
                | _     => false
            in
              case List.find eq temps of 
                NONE    => FG.addNode(ans, t, t)
              | SOME x  => ans
            end
          val defsAdded = T.Set.foldl add ans defs
          val liveAdded = T.Set.foldl add defsAdded live
        in
          liveAdded
        end
    in
      foldl f FG.empty nodes
    end

  (* TODO *)
  fun show(outstream, IGRAPH {graph = graph, tnode = tnode, gtemp = gtemp, moves = moves}) =
    let
    in
      TextIO.output(outstream, "hello")
    end
   (*TODO *)
  (*fun interferenceGraph(Flow.FGRAPH{control, def, use}) = *)
end