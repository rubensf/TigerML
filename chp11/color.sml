signature COLOR =
sig
  type allocation
  type registerlist
  type node

  val emptyAlloc : allocation

  val color: {
    igraph: Liveness.igraph,
    initial: allocation,
    spillCost: node -> int,
    registers: registerlist
  } -> allocation * Temp.temp list
end

functor Color(F: FRAME) :> COLOR
where
  type allocation = F.register Temp.Map.map and
  type registerlist = F.register list and
  type node = Temp.temp Liveness.FG.node
=
struct
  structure F = F
  structure T = Temp
  structure FG = Liveness.FG

  type allocation = F.register Temp.Map.map
  type registerlist = F.register list
  type node = Temp.temp Liveness.FG.node

  val emptyAlloc = Temp.Map.empty
  val precoloredNodes = List.foldl
                          (fn (x, m) =>
                             (Temp.Map.insert (m, (F.getRegTemp x), x)))
                          Temp.Map.empty F.allRegisters

  fun color {igraph, initial, spillCost, registers} =
    let
      val (graph, tnode, gnode, moves) =
        case igraph of
          Liveness.IGRAPH{graph=g, tnode=t, gtemp=gt, moves=m} => (g, t, gt, m)

      fun isPrecolored n =
        case F.getTempReg (FG.nodeInfo n) of
          SOME(r) => true
        | _       => false

      fun simplify(graph) =
        let
          fun removeNodes(g, stack) =
            let
              fun canRemoveNode(n, stack) =
                if FG.degree(n) < (List.length F.callerRegs)
                then n::stack
                else stack

              val stack' = foldl canRemoveNode
                                 stack
                                 (List.filter
                                    (fn n => not (isPrecolored n))
                                    (FG.nodes g))

              fun removeNode(n, g') = FG.removeNode'(g', FG.getNodeID n)
              val g' = foldl removeNode g stack'
            in
              if List.length stack = List.length stack'
              then (g, stack)
              else removeNodes(g', stack')
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
            Option.isSome
              (List.find (fn x => x = (FG.getNodeID node)) stackIDs)

          fun onSpilled node =
            Option.isSome
              (List.find (fn x => x = (FG.getNodeID node)) spilledIDs)
        in
          List.find
            (fn node => (not (isPrecolored node)) andalso
                        (not (onStack node))      andalso
                        (not (onSpilled node)))
            nodes
        end

      fun simplifySpill(stack, graph, alreadySpilled) =
        let
          val stack' = stack @ simplify(graph)
          val nodeToSpill = needToSpill(stack, alreadySpilled)

          val graph' = if Option.isSome nodeToSpill
                       then FG.removeNode
                              (graph,
                               FG.getNodeID (Option.valOf nodeToSpill))
                       else graph
        in
          if Option.isSome nodeToSpill
          then simplifySpill(stack',
                             graph',
                             (Option.valOf nodeToSpill)::alreadySpilled)
          else (stack', alreadySpilled)
        end

      val (stack, spills) = simplifySpill([], graph, [])

      fun allocate(map, stack) =
        let
          val actualSpills = ref []

          fun assignColor(node, allocation) =
            let
              fun availableColor(unavailable, color::rest) =
                (case (List.find (fn x => x = color) unavailable) of
                   SOME c => availableColor(unavailable, rest)
                 | NONE   => SOME color)
                | availableColor(unavailable, []) = NONE

              val unavailableNodes =
                List.mapPartial (fn x => Temp.Map.find(allocation, x))
                                (FG.succs node)
            in
              availableColor(unavailableNodes, registers)
            end

          fun pushColorToMap(node, allocation) =
            let
              val color = assignColor(node, allocation)
              val newAllocation =
                (case color of
                   SOME c => Temp.Map.insert(allocation, FG.nodeInfo node, c)
                 | NONE => (actualSpills:=(node::(!actualSpills));allocation))
            in
              newAllocation
            end

          val nonPrecolored =
            (List.filter
               (fn node =>
                  not (Option.isSome (Temp.Map.find(map, FG.nodeInfo node))))
             stack)
        in
          (foldl pushColorToMap map nonPrecolored, !actualSpills)
        end

      val initial' = if (Temp.Map.numItems initial) = 0
                     then List.foldl
                            (fn (x, m) =>
                               (Temp.Map.insert (m, (F.getRegTemp x), x)))
                            precoloredNodes F.allRegisters
                     else initial
      val (alloc, _) = allocate(initial', stack)
      val (alloc', actualSpills) = allocate(alloc, spills)
      val spills' = map FG.nodeInfo actualSpills
    in
      (alloc', spills')
    end
end
