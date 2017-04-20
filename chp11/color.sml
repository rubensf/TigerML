signature COLOR = 
sig
  structure Frame : FRAME
  structure FG : FUNCGRAPH
  type allocation = Frame.register Temp.Map.map
  val color: {
    igraph: Liveness.igraph,
    initial: allocation,
    spillCost: Temp.temp FG.node -> int,
    registers: Frame.register list,
    moves: (Temp.temp FG.node * Temp.temp FG.node) list
  } -> allocation * Temp.temp list
end

structure Color :> COLOR = 
struct
  structure Frame = MipsFrame
  structure T = Temp
  structure TempOrdKey = struct type ord_key = Temp.temp
                                val compare = Temp.compare
                         end
  structure FG = FuncGraph(TempOrdKey)
  type allocation = Frame.register Temp.Map.map
  fun color {igraph, initial, spillCost, registers, movelist} =
    let
      val (graph, tnode, gtemp, moves) = case igraph of Liveness.IGRAPH{graph=g, tnode=t, gtemp=gt, moves=m} => (g, t, gt, m)
      fun precolored n = (case Temp.Map.find(initial, FG.nodeInfo n) of
                SOME(r) => true
              | _       => false)

      fun simplify(graph) =
        let
          fun removeNodes(g, stack) = 
            let
              fun processNode(n, stack) = (case FG.degree(n) < 22 of 
                                true  => n::stack
                              | false => stack)
              val stack' = foldl processNode stack (List.filter (fn n => not (precolored n)) (FG.nodes g))
              fun removeNode(n, graph) = FG.removeNode'(graph, FG.getNodeID n)
              val g' = foldl removeNode g stack'
              val ret = (case (List.length stack = List.length stack') of
                                true  => (g, stack)
                              | false => removeNodes(g', stack'))
            in
              ret
            end
          val (graph', stack') = removeNodes(graph, [])
        in
          stack'
        end
    in
      ()
    end
end