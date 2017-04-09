structure MakeGraph :
sig
  val instrs2graph: Assem.instr list -> Flow.flowgraph
end =
struct
  structure FG = Flow.FG
  structure A = Assem

  fun instrs2graph instrs =
    let
      fun handleBlock (g, block) =
        let
        in
          if List.null block
          then g
          else let
                 val blockID = List.hd block
                 val label = case blockID of
                               A.LABEL {lab=lab, ...} => lab
                             | e => (ErrorMsg.error 0 "Internal failure: First instr not label."; Temp.newlabel ())

               in
                 FG.addNode(g, label, block)
               end
        end

      fun step(oper as A.OPER {jump=NONE,...}, accum) =
            (#1 accum, (#2 accum)@[oper])
        | step(oper as A.OPER {jump=SOME jl,...}, accum) =
            let val g = handleBlock((#1 accum), (#2 accum)@[oper]) in (g, []) end
        | step(label as A.LABEL {...}, accum) =
            let val g = handleBlock(accum) in (g, [label]) end
        | step(move as A.MOVE {...}, accum) =
            ((#1 accum), (#2 accum)@[move])

      val (gNoLast, lastLabel) = (foldl step (FG.empty, []) instrs)
      val g = handleBlock(gNoLast, lastLabel)

      val nodes = FG.nodes g

      (* Remove last label only node*)
      val nodes' = FG.nodes gNoLast

      fun findJumpDst (src, g') =
        let
          val srcID = (FG.getNodeID src)
          val _ = print ("doing label " ^ (Temp.labelToString srcID) ^ "\n")
          val lastInstr = List.last (FG.nodeInfo src)

          fun findInstrForLbl lbl =
            List.find (fn x => (FG.getNodeID x) = lbl) nodes
        in
          case lastInstr of
            A.OPER {jump=SOME jl, ...} => List.foldl (fn (j, g'') => case (findInstrForLbl j) of
                                                                       SOME node => FG.addEdge (g'', {from=srcID, to=(FG.getNodeID node)})
                                                                     | NONE => (ErrorMsg.error 0 "Internal Failure."; g''))
                                                     g' jl
          | e                          => (ErrorMsg.error 0 "Internal failure: Last instr not jump."; g)
        end

      val g' = foldl findJumpDst g nodes'
      val defUses = Flow.makeDefUse g'
    in
      Flow.FGRAPH {control=g', def=(#def defUses), use=(#use defUses)}
    end
end
