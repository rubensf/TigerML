structure Main =
struct
  structure CG = MipsGen
  structure MG = MakeGraph
  structure FG = Flow.FG
  structure TempSet = Flow.TempSet
  structure R = Translate (MipsFrame)
  structure S = Semant (R)
  structure C = Canon
  structure A = Assem
  structure F = MipsFrame

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
                      PrintAbsyn.print (TextIO.stdOut, abst))
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
          val _ = R.resetFrags()
          val frags = S.transProg abst
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with semantic analysis. " ^
                       "Stopping compilation.\n");
                (frags, !err))
          else (if verbose >= 2
                then (print "==========Printing Non-Linear IR==========\n";
                      List.app (fn F.PROC{body, frame} => Printtree.printtree(TextIO.stdOut, body)
                                 | F.STRING (lbl, str) => print ((Temp.labelToString lbl) ^ ": " ^ str))
                               frags)
                else ();
                (frags, !err))
        end

      fun linearize frags =
        let
          val _ = if verbose >= 1
                  then print "Simplifying Intermediate Representation.\n"
                  else ()
          fun liner (F.PROC{body, frame}, ans) =
                ans@[(C.traceSchedule(C.basicBlocks (C.linearize body)), frame)]
            | liner (F.STRING(lab, str), ans) =
                ans
          val frags' = List.foldl liner [] frags
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with simplifying IR. " ^
                       "Stopping compilation.\n");
                       ([], !err))
          else (if verbose >= 2
                then (print "==========Printing IR==========\n";
                      List.app (fn (frags'', _) => List.app (fn y =>
                                                             Printtree.printtree(TextIO.stdOut, y))
                                                            frags'')
                               frags')
                else ();
                (frags', !err))
        end

      fun codegen fragsframelist =
        let
          val _ = if verbose >= 1
                  then print "Generating assembly.\n"
                  else ()
          val instrs = List.foldl (fn ((frags, frame), ans) =>
                                     ans@[(F.procEntryExit2(frame,
                                                          List.concat (map (CG.codegen frame) frags)),
                                           frame)])
                                  [] fragsframelist
          val format = A.format (F.makestring) (* For printing below. *)
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with generating assembly. " ^
                       "Stopping compilation.\n");
                ([], !err))
          else (if verbose >= 2
                then (print "==========Printing Pre-Register Allocation Assembly==========\n";
                      List.app (fn (instrs', _) => List.app (fn y => TextIO.output(TextIO.stdOut, format y))
                                                            instrs')
                               instrs)
                else ();
                (instrs, !err))
        end

      fun makeflowgraph instrframelist =
        let
          val _ = if verbose >= 1
                  then print "Making flow graph.\n"
                  else ()
          val instrflowframelist =
            List.foldl (fn ((instrs, frame), ans) =>
                          ans@[(instrs,
                                MG.instrs2graph(instrs),
                                frame)])
                       [] instrframelist
          val format = A.format (F.makestring) (* For printing below. *)
          val err = ErrorMsg.anyErrors
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors with making flow graph. " ^
                       "Stopping compilation.\n");
                ([], !err))
          else (if verbose = 2
                then print "For printing the flow graph, please use verbose 3.\n"
                else if verbose > 2
                then (print "==========Printing Flow Graph==========\n";
                      List.app (fn (_, Flow.FGRAPH {control, def, use}, _) =>
                                  (print "Printing new frame::\n";
                                   List.app (fn blockData =>
                                               (print ("Printing new block::\n" ^
                                                       (List.foldl
                                                          (fn (ist, ans) =>
                                                             ans ^ (format ist))
                                                          "" blockData))))
                                            (map FG.nodeInfo
                                                (FG.nodes control));
                                   FG.printGraph (fn (id, data) =>
                                                    Temp.labelToString id)
                                                 control;
                                   print "Printing defs and uses::\n";
                                   List.app (fn x =>
      print ("Frame: " ^ (Temp.labelToString x) ^ "\n" ^
             "Defs: " ^ (TempSet.foldl (fn (x, ans) => ans ^ (F.makestring x) ^
             ", ") "" (Option.valOf(Flow.NodeMap.find(def, x)))) ^ "\n" ^
             "Uses: " ^ (TempSet.foldl (fn (x, ans) => ans ^ (F.makestring x) ^
             ", ") "" (Option.valOf(Flow.NodeMap.find(use, x)))) ^ "\n"))
                                            (map FG.getNodeID (FG.nodes
                                                 control))))
                               instrflowframelist)
                else ();
                (instrflowframelist, !err))
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
                                  Liveness.show(TextIO.stdOut, igraph))
                               instrflowigraphframelist)
                else ();
                (instrflowigraphframelist, !err))
        end
    in
      case (parsefile file) of
        (abst, true) => ()
      | (abst, false) =>
      case (findescape abst) of
        true => ()
      | false =>
      case (semanticanalysis abst) of
        (frags, true) => ()
      | (frags, false) =>
      case (linearize frags) of
        (frags', true) => ()
      | (frags', false) =>
      case (codegen frags') of
        (instrs, true) => ()
      | (instrs, false) =>
      case (makeflowgraph instrs) of
        (instrflowframelist, true) => ()
      | (instrflowframelist, false) =>
      case (liveness instrflowframelist) of
        (instrflowigraphframelist, true) => ()
      | (instrflowigraphframelist, false) => ()
    end

  fun compile file = compileverb file 0
end

