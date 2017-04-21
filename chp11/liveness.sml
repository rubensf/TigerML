structure Liveness :
sig
  structure TempOrdKey: ORD_KEY
  structure FG: FUNCGRAPH
  structure T: TEMP
  datatype igraph =
    IGRAPH of {graph: T.temp FG.graph,
               tnode: T.temp -> T.temp FG.node,
               gtemp: T.temp FG.node -> T.temp,
               moves: (T.temp FG.node * T.temp FG.node) list}
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
      val liveIn = foldl (fn (x,ans) => NodeMap.insert(ans, Flow.FG.getNodeID x, T.Set.empty)) NodeMap.empty nodes
      val liveOut = foldl (fn (x,ans) => NodeMap.insert(ans, Flow.FG.getNodeID x, T.Set.empty)) NodeMap.empty nodes
      fun loopUntilDone(nodes, liveIn, liveOut, true) = {inMap=liveIn, outMap=liveOut}
        | loopUntilDone(nodes, liveIn, liveOut, false) =
            let
              val liveIn' = foldl (fn (x, ans) => NodeMap.insert(ans, Flow.FG.getNodeID x, Option.valOf(NodeMap.find(liveIn, Flow.FG.getNodeID x)))) NodeMap.empty nodes
              val liveOut' = foldl (fn (x,ans) => NodeMap.insert(ans, Flow.FG.getNodeID x, Option.valOf(NodeMap.find(liveOut, Flow.FG.getNodeID x)))) NodeMap.empty nodes
              fun updateSets (x, {li, lo}) =
                let
                  val defSet = Option.valOf(NodeMap.find(def, Flow.FG.getNodeID x))
                  val useSet = Option.valOf(NodeMap.find(use, Flow.FG.getNodeID x))

                  val outSet = Option.valOf(NodeMap.find(lo, Flow.FG.getNodeID x))

                  val newInSet = T.Set.union(useSet, T.Set.difference(outSet, defSet))
                  val newOutSet =
                    foldl (fn (succID, ans) =>
                             T.Set.union(ans,
                                         Option.valOf(NodeMap.find(li, succID))))
                          T.Set.empty
                          (Flow.FG.succs x)

                  val li' = NodeMap.insert(li, Flow.FG.getNodeID x, newInSet)
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
          val live = Option.valOf(NodeMap.find(liveOut, Flow.FG.getNodeID x))
          val instrs:Assem.instr list = Flow.FG.nodeInfo x
          fun handleInstr(instr, {live, graph}) =
            let
              val deflist = case instr of
                  Assem.OPER{assem=_, src=_, dst=d, jump=_} => d
                | Assem.LABEL l => []
                | Assem.MOVE{assem=_, src=_, dst=d} => [d]
              val uselist = case instr of
                  Assem.OPER{assem=_, src=s, dst=_, jump=_} => s
                | Assem.LABEL l => []
                | Assem.MOVE{assem=_, src=s, dst=_} => [s]
              val ismove = case instr of
                  Assem.MOVE m => true
                | _            => false
              val defSet = T.Set.addList(T.Set.empty, deflist)
              val useSet = T.Set.addList(T.Set.empty, uselist)
              val liveBefore = T.Set.union(useSet, T.Set.difference(live, defSet))
              fun handleDefs (def, ans) =
                let
                  fun tempEqual(t1, t2) = case T.compare(t1, t2) of
                    EQUAL => true
                  | _     => false
                  fun handleLiveOutTemps(liveOutTemp, ans) =
                    let
                      val usedTemp = (T.Set.exists (fn x => tempEqual(x, liveOutTemp)) useSet)
                      val shouldAdd = (not ismove) orelse (ismove andalso (not usedTemp))
                      val temps = map FG.nodeInfo (FG.nodes ans)
                      fun addEdge(graph, t1, t2) =
                        let
                          val g' = case List.exists (fn x => tempEqual(x, t1)) temps of
                            true => graph
                          | false => FG.addNode(graph, t1, t1)
                          val g'' = case List.exists (fn x => tempEqual(x, t2)) temps of
                            true => g'
                          | false => FG.addNode(g', t2, t2)
                        in
                          FG.doubleEdge(g'', t1, t2)
                        end
                      val result = case shouldAdd of
                        false => ans
                      | true  => addEdge(ans, def, liveOutTemp)
                    in
                      result
                    end
                in
                  T.Set.foldl handleLiveOutTemps ans live
                end
                val g = T.Set.foldl handleDefs graph defSet
            in
              {live=liveBefore, graph=g}
            end
            val last = foldr handleInstr {live=live, graph=ans} instrs (*make sure this is going in the right direction*)
        in
          #graph last
        end
    in
      foldl f FG.empty nodes
    end

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
      val _ = TextIO.output(outstream, "=========================Adjacent Nodes=========================\n")
    in
      (*FG.printGraph (fn (id,node)=>MipsFrame.makestring(id)) graph*)
      List.app printNode nodes
    end

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
