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
  structure FG = Liveness.FG
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

      fun needToSpill(stack, alreadySpilled) = 
        let
          val nodes = FG.nodes graph
          val stackIDs = map FG.getNodeID stack
          val spilledIDs = map FG.getNodeID alreadySpilled
          fun onStack node = 
            let
              val nodeID = FG.getNodeID node
              val opt = List.find (fn x => x = nodeID) stackIDs
              val ret = (case opt of
                                SOME x => true
                              | NONE   => false)
            in
              ret
            end
          fun onSpilled node = 
            let
              val nodeID = FG.getNodeID node
              val opt = List.find(fn x => x = nodeID) spilledIDs
              val ret = (case opt of 
                                SOME x => true
                              | NONE   => false)
            in
              ret
            end
          val spillChoices = List.filter (fn node => not (precolored node) andalso (not (onStack node)) andalso (not (onSpilled node))) nodes
          val firstNode = case List.length spillChoices of
              0 => NONE
            | _ => SOME (List.nth(spillChoices, 0))
        in
          firstNode
        end

      fun simplifySpill(stack, graph, alreadySpilled) =
        let
          val stack' = stack @ simplify(graph) 
          val nodeToSpill = needToSpill(stack, alreadySpilled)
          val shouldSpill = Option.isSome nodeToSpill
          val graph' = (case shouldSpill of 
                        true  => FG.removeNode(graph, FG.getNodeID (Option.valOf nodeToSpill))
                      | false => graph) 
          val ret = (case shouldSpill of
                        true  => simplifySpill(stack', graph', (Option.valOf nodeToSpill)::alreadySpilled)
                      | false => (stack', alreadySpilled))
        in
          ret
        end
      val (stack, spills) = simplifySpill([], graph, [])
      fun allocate(map, stack) = 
        let
          val actualSpills = ref []
          fun assignColor(node, allocation) = 
            let
              fun availableColor(unavailable, color::rest) = (case (List.find (fn x => x = color) unavailable) of 
                                  SOME c => availableColor(unavailable, rest)
                                | NONE   => color)
                | availableColor(unavailable, []) = "NO_COLOR"
              fun createUnavailable node = List.mapPartial (fn neighbor => Temp.Map.find(allocation, neighbor)) (FG.succs node)
              val assigned = availableColor(createUnavailable node, registers)
            in
              assigned
            end

          fun pushColorToMap(node, allocation) = 
            let
              val color = assignColor(node, allocation)
              val newAllocation = (case color <> "NO_COLOR" of
                                  true  => Temp.Map.insert(allocation, FG.nodeInfo node, color)
                                | false => (actualSpills:=(node::(!actualSpills));allocation))
            in
              newAllocation
            end

          val nonPrecolored = (List.filter (fn node => not (Option.isSome (Temp.Map.find(map, FG.nodeInfo node)))) stack)
        in
          (foldl pushColorToMap map nonPrecolored, !actualSpills)
        end  
      val (alloc, _) = allocate(initial, stack)
      val (alloc', actualSpills) = allocate(alloc, spills)
      val spills' = map G.nodeInfo actualSpills
    in
      (alloc', spills')
    end
end