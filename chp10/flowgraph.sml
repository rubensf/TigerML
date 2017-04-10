structure Flow =
struct
  structure LabelOrdKey = struct type ord_key = Temp.label
                                 val compare = Symbol.compare
                          end
  structure FG = FuncGraph(LabelOrdKey)
  structure NodeMap = SplayMapFn(LabelOrdKey)
  structure TempSet = Temp.Set

  structure A = Assem

  datatype flowgraph = FGRAPH of {
          control: Assem.instr list FG.graph,
          def: TempSet.set NodeMap.map,
          use: TempSet.set NodeMap.map}

  fun makeDefUse g =
    let
      fun handleBlock block =
        let
          fun doThing (origSrc, origDst, {def, use}) =
            let
              fun shouldAdd src =
                case TempSet.find (fn x => x = src) def of
                  SOME j => false
                | NONE   => true
              val newUses = List.foldl (fn (x, ans) =>
                                        if (shouldAdd x)
                                        then TempSet.add(ans, x)
                                        else ans)
                                       use origSrc
            in
              {def=TempSet.addList(def, origDst),
               use=newUses}
            end

          fun step (A.LABEL l, defUse) = defUse
            | step (A.OPER oper, defUse) =
                doThing(#src oper, #dst oper, defUse)
            | step (A.MOVE mov, defUse) = 
                doThing([#src mov], [#dst mov], defUse)
        in
          List.foldl step {def=TempSet.empty, use=TempSet.empty} (FG.nodeInfo block)
        end
    in
      List.foldl (fn (x, {def, use}) =>
                          let
                            val id = FG.getNodeID x
                            val blockDefUse = handleBlock x
                          in
                            {def=NodeMap.insert(def, id, (#def blockDefUse)),
                             use=NodeMap.insert(use, id, (#use blockDefUse))}
                          end)
                 {def=NodeMap.empty, use=NodeMap.empty}
                 (FG.nodes g)
    end
  (* Note:  any "use" within the block is assumed to be BEFORE a "def"
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)

end
