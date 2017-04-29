structure Main=
struct
  structure F = MipsFrame
  structure CG = MipsGen

  structure R = Translate (F)
  structure S = Semant (R)
  structure C = Color (F)

  val outStream = TextIO.stdOut

  fun compileverb file verbose =
    let
      fun parsefile file =
        let
          val _ = if verbose >= 1
                  then print "Parsing file.\n"
                  else ()
          val abst = Parse.parse file
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with parsing. " ^
                       "Stopping compilation.\n");
                (abst, !err))
          else (if verbose >= 2
                then (print "==========Priting Abstract Syntax Tree==========\n";
                      PrintAbsyn.print (outStream, abst))
                else ();
                (abst, !err))
        end

      fun findescape abst =
        let
          val _ = if verbose >= 1
                  then print "Running Find Escape analysis.\n"
                  else ()
          val _ = FindEscape.findEscape abst
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with find escape. " ^
                       "Stopping compilation.\n");
                !err)
          else !err
        end

      fun semanticanalysis abst =
        let
          val _ = if verbose >= 1
                  then print "Runing Semantic analysis.\n"
                  else ()
          val frags = S.transProg abst
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with semantic analysis. " ^
                       "Stopping compilation.\n");
                (frags, !err))
          else (if verbose >= 2
                then (print "==========Printing Non-Linear IR==========\n";
                      List.app (fn F.PROC{body, frame} =>
                                    Printtree.printtree(outStream, body)
                                 | F.STRING (lb, str) =>
                                    print ((Temp.labelToString lb) ^ ": " ^ str ^ "\n"))
                               frags)
                else ();
                (frags, !err))
        end

      fun linearize frags =
        let
          val _ = if verbose >= 1
                  then print "Simplifying Intermediate Representation.\n"
                  else ()
          fun liner (F.PROC{body, frame}, (prs, strs)) =
                (prs@[(Canon.traceSchedule
                         (Canon.basicBlocks
                            (Canon.linearize body)),
                       frame)],
                 strs)
            | liner (s as F.STRING(lab, str), (prs, strs)) =
                (prs, strs@[s])
          val (procs, strs) = List.foldl liner ([], []) frags
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with simplifying IR. " ^
                       "Stopping compilation.\n");
                       ([], [], !err))
          else (if verbose >= 2
                then (print "==========Printing IR==========\n";
                      List.app (fn (procs', _) =>
                                  List.app (fn y =>
                                              Printtree.printtree(outStream, y))
                                           procs')
                               procs)
                else ();
                (procs, strs, !err))
        end

      fun codegen procsFrame =
        let
          val _ = if verbose >= 1
                  then print "Generating assembly.\n"
                  else ()
          val instrs =
            List.foldl (fn ((procs, frame), ans) =>
                          ans@[(F.procEntryExit2(frame,
                                                 List.concat
                                                   (map (CG.codegen frame)
                                                   procs)),
                                frame)])
                       [] procsFrame
          val format = Assem.format (F.makeString) (* For printing below. *)
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with generating assembly. " ^
                       "Stopping compilation.\n");
                ([], !err))
          else (if verbose >= 2
                then (print "==========Printing Pre-Register Allocation Assembly==========\n";
                      List.app (fn (instrs', _) =>
                                  List.app
                                    (fn y => TextIO.output(outStream, format y))
                                    instrs')
                               instrs)
                else ();
                (instrs, !err))
        end

      fun makeflowgraph instrsFrameList =
        let
          val _ = if verbose >= 1
                  then print "Making flow graph.\n"
                  else ()
          val instrsFlowFrameList =
            List.foldl (fn ((instrs, frame), ans) =>
                          ans@[(instrs,
                                MakeGraph.instrs2graph(instrs),
                                frame)])
                       [] instrsFrameList
          val format = Assem.format (F.makeString) (* For printing below. *)
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with making flow graph. " ^
                       "Stopping compilation.\n");
                ([], !err))
          else (if verbose = 2
                then print "For printing the flow graph, please use verbose 3.\n"
                else if verbose > 2
                then (print "==========Printing Flow Graphs==========\n";
                      List.app Flow.printFlow
                               (map (fn (instr, flow, frame) =>
                                       (flow, format, F.makeString))
                                    instrsFlowFrameList))
                else ();
                (instrsFlowFrameList, !err))
        end

      fun liveness instrflowframelist =
        let
          val _ = if verbose >= 1
                  then print "Making interferece graph.\n"
                  else ()
          val instrflowigraphframelist =
            List.foldl (fn ((instrs, flow, frame), ans) =>
                          let
                            val (igraph, gettemps) =
                              Liveness.interferenceGraph(flow)
                          in
                            ans@[(instrs,
                                  flow,
                                  igraph,
                                  gettemps,
                                  frame)]
                          end)
                       [] instrflowframelist
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with making interference graph. " ^
                       "Stopping compilation.\n");
                ([], !err))
          else (if verbose = 2
                then print "For printing the liveness graph, please use verbose 3.\n"
                else if verbose > 2
                then (print "==========Printing Interference Graph==========\n";
                      List.app (fn (_, _, igraph, _, _) =>
                                  Liveness.show(outStream,
                                                igraph,
                                                F.makeString))
                               instrflowigraphframelist)
                else ();
                (instrflowigraphframelist, !err))
        end

      fun regalloc (instrflowigraphframelist, strs) =
        let
          val _ = if verbose >= 1
                  then print "Coloring registers.\n"
                  else ()

          fun pickRegister alloc temp =
            case Temp.Map.find (alloc, temp) of
              SOME r => F.regToString r
            | NONE   => F.makeString temp

          fun f ((instrs, flow, igraph, gettemps, frame), ans) =
            let
              val (alloc, spills) =
                C.color {igraph=igraph,
                         spillCost=(fn x => 1)}
              val didSpill = (List.length spills) <> 0
              val body = F.procEntryExit3(frame, instrs)
              val noMove = List.filter
                             (fn x => case x of
                                        Assem.LABEL l   => true
                                      | Assem.OPER oper => true
                                      | Assem.MOVE {assem, src, dst} =>
                                          pickRegister alloc src <>
                                          pickRegister alloc dst)
                             body
            in
              ans @ [{ins=noMove,
                      alloc=alloc,
                      spill=didSpill}]
            end
          val colorings = foldl f [] instrflowigraphframelist
          val err = ErrorMsg.anyErrors

          fun printCode {ins, alloc, spill} =
            let
              (* should check for spill here *)
              val tmpStr = pickRegister alloc
              val format = Assem.format(tmpStr)
            in
              List.app (fn x => TextIO.output(outStream, format x)) ins
            end
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with making coloring registers. " ^
                       "Stopping compilation.\n");
                ([], !err))
          else (if verbose > 1
                then (print "==========Printing Final Assembly==========\n";
                      print (F.getTextHdr ());
                      List.app printCode colorings;
                      print (F.getDataHdr ());
                      List.app (fn x => print (F.strAssembly x)) strs)
                else ();
                (colorings, !err))
        end
    in
      Temp.reset();
      R.resetFrags();
      F.resetRegs();
      case (parsefile file) of
        (abst, true) => false
      | (abst, false) =>
      case (findescape abst) of
        true => false
      | false =>
      case (semanticanalysis abst) of
        (frags, true) => false
      | (frags, false) =>
      case (linearize frags) of
        (procs, strs, true) => false
      | (procs, strs, false) =>
      case (codegen procs) of
        (instrs, true) => false
      | (instrs, false) =>
      case (makeflowgraph instrs) of
        (instrflowframelist, true) => false
      | (instrflowframelist, false) =>
      case (liveness instrflowframelist) of
        (instrflowigraphframelist, true) => false
      | (instrflowigraphframelist, false) =>
      case (regalloc (instrflowigraphframelist, strs)) of
        (allocation, true)  => false
      | (allocation, false) => true
    end

  fun compile file = compileverb file 0
end

