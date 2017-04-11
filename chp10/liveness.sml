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
  val interferenceGraph: Flow.flowgraph -> igraph * (Assem.instr list Flow.FG.node -> T.Set.set)
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
                  (*val _ = print ("Node " ^ T.labelToString(Flow.FG.getNodeID x) ^ "\n")*)
                  val useSet = Option.valOf(NodeMap.find(use, Flow.FG.getNodeID x))
                  (*val _ = print ("useSet = " ^ Int.toString(T.Set.numItems(useSet)) ^ "\n")*)
                  val outSet = Option.valOf(NodeMap.find(lo, Flow.FG.getNodeID x))
                  (*val _ = print ("outSet = " ^ Int.toString(T.Set.numItems(outSet)) ^ "\n")*)
                  val defSet = Option.valOf(NodeMap.find(def, Flow.FG.getNodeID x))
                  (*val _ = print ("defSet = " ^ Int.toString(T.Set.numItems(defSet)) ^ "\n")*)
                  val newInSet = T.Set.union(useSet, T.Set.difference(outSet, defSet))
                  (*val _ = print ("newInSet = " ^ Int.toString(T.Set.numItems(newInSet)) ^ "\n")*)
                  val li' = NodeMap.insert(li, Flow.FG.getNodeID x, newInSet)
                  val succIDs = Flow.FG.succs x
                  (*val _ = print ("successor size = " ^ Int.toString(List.length(succIDs)) ^ "\n")*)
                  val newOutSet = foldl (fn (xID, ans) => T.Set.union(ans, Option.valOf(NodeMap.find(li', xID)))) T.Set.empty succIDs
                  (*val _ = print ("newOutSet = " ^ Int.toString(T.Set.numItems(newOutSet)) ^ "\n")*)
                  val lo' = NodeMap.insert(lo, Flow.FG.getNodeID x, newOutSet)
                in
                  {li=li', lo=lo'}
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
          (*val _ = print ("DEF SIZE: " ^ (Int.toString(T.Set.numItems(defs))) ^ "\n")*)
          val live = Option.valOf(NodeMap.find(liveOut, Flow.FG.getNodeID x))
          (*val _ = print ("LIVE SIZE: " ^ (Int.toString(T.Set.numItems(live))) ^ "\n")*)
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
          fun iterDefs (defTemp, ans) = 
            let
              fun addEdge (liveTemp, ans) = FG.doubleEdge(ans, defTemp, liveTemp)
            in
              T.Set.foldl addEdge ans live
            end
        in
          T.Set.foldl iterDefs liveAdded defs
        end
    in
      foldl f FG.empty nodes
    end

  (* TODO *)
  fun show(outstream, IGRAPH {graph = graph, tnode = tnode, gtemp = gtemp, moves = moves}) =
    let
      val nodes = FG.nodes graph
      fun printNode node = 
        let
          val _ = TextIO.output(outstream, MipsFrame.makestring(gtemp node) ^  " => ")
          fun f n = 
            case FG.isAdjacent(node, n) of
              true => TextIO.output(outstream, MipsFrame.makestring(gtemp n) ^ ", ")
            | false => ()
          val _ = List.app f nodes
        in
          TextIO.output(outstream, "\n")
        end
      val _ = TextIO.output(outstream, "==================Printing Interference Graph==================\n")
      val _ = TextIO.output(outstream, "=========================Adjacent Nodes=========================\n")
    in
      (*FG.printGraph (fn (id,node)=>MipsFrame.makestring(id)) graph*)
      List.app printNode nodes
    end
   (*TODO *)
  fun interferenceGraph(f as Flow.FGRAPH{control, def, use}) = 
    let
      val liveSets = computeLiveSets(f)
      val liveIn = #inMap liveSets
      val liveOut = #outMap liveSets
      val ig = createInterferenceGraph(liveOut, def, control)
      fun mapping node = 
        case NodeMap.find(liveOut, Flow.FG.getNodeID node) of
          NONE => T.Set.empty (* should not happen *)
        | SOME set => set
      fun tnode t = FG.getNode(ig, t)
      fun gtemp n = FG.getNodeID(n)
      val igrph = IGRAPH{graph=ig, tnode=tnode, gtemp=gtemp, moves=[]}
    in
      (igrph, mapping)
    end
end