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

  val aaaa = 0

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
  
  fun printFlow (FGRAPH {control, def, use}, format, makestr) =
    (print "Printing new frame:\n";
     List.app (fn blockData =>
                 (print ("Printing new block::\n" ^
                         (List.foldl (fn (instr, ans) =>
                                        ans ^ (format instr))
                                     "" blockData))))
              (map FG.nodeInfo (FG.nodes control));
     FG.printGraph (fn (id, data) => Temp.labelToString id) control;
     print "Printing defs and uses::\n";
     List.app (fn x =>
       print ("Frame: " ^ (Temp.labelToString x) ^ "\n" ^
              "Defs: " ^ (TempSet.foldl
                            (fn (x, ans) =>
                               ans ^ (makestr x) ^ ", ")
                            "" (Option.valOf(NodeMap.find(def, x)))) ^ "\n" ^
              "Uses: " ^ (TempSet.foldl
                           (fn (x, ans) =>
                              ans ^ (makestr x) ^ ", ")
                           "" (Option.valOf(NodeMap.find(use, x)))) ^ "\n"))
              (map FG.getNodeID (FG.nodes control)))

end
