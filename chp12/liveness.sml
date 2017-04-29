structure Liveness :
sig
  structure TempOrdKey: ORD_KEY
  structure FG: FUNCGRAPH
  structure T: TEMP
  datatype igraph =
    IGRAPH of {graph: T.temp FG.graph,
               moves: (T.temp FG.node * T.temp FG.node) list}
  val interferenceGraph: Flow.flowgraph -> igraph
  val show: TextIO.outstream * igraph * (FG.nodeID -> string) -> unit
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
               moves: (T.temp FG.node * T.temp FG.node) list}
  (* Set of live variables *)
  type liveSet = T.Set.set
  (* Map from flow graph node to live temporaries at that node *)
  type liveMap = liveSet NodeMap.map

  (* All the moves *)
  val moves : (T.temp FG.node * T.temp FG.node) list ref = ref []

  fun computeLiveSets (Flow.FGRAPH{control, def, use}) =
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
          val instrs = Flow.FG.nodeInfo x
          fun handleInstr(instr, {live, graph}) =
            let
              val deflist =
                case instr of
                  Assem.OPER{assem=_, src=_, dst=d, jump=_} => d
                | Assem.LABEL l => []
                | Assem.MOVE{assem=_, src=_, dst=d} => [d]
              val uselist =
                case instr of
                  Assem.OPER{assem=_, src=s, dst=_, jump=_} => s
                | Assem.LABEL l => []
                | Assem.MOVE{assem=_, src=s, dst=_} => [s]
              val isMove =
                case instr of
                  Assem.MOVE{assem=_, src=s, dst=d} => true
                | _                                 => false
              val defSet = T.Set.addList(T.Set.empty, deflist)
              val useSet = T.Set.addList(T.Set.empty, uselist)
              val liveBefore = T.Set.union(useSet, T.Set.difference(live, defSet))

              fun newNode (x, ans) =
                if List.exists (fn y => x = y)
                   (List.map FG.getNodeID (FG.nodes ans))
                then ans
                else FG.addNode(ans, x, x)

              val g' = List.foldl newNode graph deflist
              val g'' = List.foldl newNode g' uselist
              fun handleDefs (def, ans) =
                let
                  fun handleLiveOutTemps(liveOutTemp, ans) =
                    let
                      val usedTemp = (T.Set.exists (fn x => x = liveOutTemp) useSet)
                      val shouldAdd = (not isMove) orelse (isMove andalso (not usedTemp))

                      val ans' = newNode (liveOutTemp, ans)
                    in
                      if shouldAdd
                      then FG.doubleEdge(ans', def, liveOutTemp)
                      else ans
                    end
                in
                  T.Set.foldr handleLiveOutTemps ans live
                end

              val g = T.Set.foldl handleDefs g'' defSet
              val _ =
                case instr of
                  Assem.MOVE{assem=_, src=s, dst=d} =>
                    let
                      val s' = FG.getNode(g, s)
                      val d' = FG.getNode(g, d)
                    in
                      moves := (s', d')::(!moves)
                    end
                | _                                 => ()
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

  fun show(outstream, IGRAPH {graph = graph, moves = moves}, makeStrF) =
    let
      val nodes = FG.nodes graph
      fun printNode node =
        let
          val _ = TextIO.output(outstream, makeStrF (FG.getNodeID node) ^  " => ")
          fun f n =
            case FG.isAdjacent(node, n) of
              true => TextIO.output(outstream, makeStrF (FG.getNodeID n) ^ ", ")
            | false => ()
          val _ = List.app f nodes
        in
          TextIO.output(outstream, "\n")
        end
      fun printMove (move: Temp.temp FG.node * Temp.temp FG.node) =
        let
          val v1 = makeStrF (FG.getNodeID (#1 move))
          val v2 = makeStrF (FG.getNodeID (#2 move))
        in
          TextIO.output(outstream, v1 ^ "-" ^ v2 ^ "\n")
        end
    in
      TextIO.output(outstream, "==========Adjacent Nodes==========\n");
      List.app printNode nodes;
      TextIO.output(outstream, "==========Moves==========\n");
      List.app printMove moves
    end

  fun interferenceGraph(f as Flow.FGRAPH{control, def, use}) =
    let
      val _ = (moves := [])
      val liveSets = computeLiveSets f
      val liveIn = #inMap liveSets
      val liveOut = #outMap liveSets
      val ig = createInterferenceGraph(liveOut, def, control)
      val igrph = IGRAPH{graph=ig, moves=(!moves)}
    in
      igrph
    end
end
