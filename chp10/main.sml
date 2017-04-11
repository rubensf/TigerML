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

  fun resetAll () = R.resetFrags()

  fun printFlow (Flow.FGRAPH {control, def, use}) =
    (print "Printing Flow Graph\n";
     FG.printGraph (fn (id, instrsX) =>
                      let
                        val format' = A.format (MipsFrame.makestring)
                      in
                        foldl (fn (x, ans) => ans ^ (format' x)) "" instrsX
                      end) control;
     List.app (fn x => let
                         val defEntry = Flow.NodeMap.find(def, x)
                         val useEntry = Flow.NodeMap.find(use, x)

                         val defEntry' = Option.valOf(defEntry)
                         val useEntry' = Option.valOf(useEntry)
                       in
                         print ("Frame " ^ (Temp.labelToString x) ^ "\n" ^
                                "Defs: " ^ (TempSet.foldl (fn (x, ans) => ans ^ (MipsFrame.makestring x) ^ ", ") "" defEntry') ^ "\n" ^
                                "Uses: " ^ (TempSet.foldl (fn (x, ans) => ans ^ (MipsFrame.makestring x) ^ ", ") "" useEntry') ^ "\n")
                       end)
              (map FG.getNodeID (FG.nodes control)))


  fun emitproc (F.PROC{body,frame}) =
    let
      val _       = print("emit " ^ (Temp.labelToString (F.name frame)) ^ "\n")
      val stms    = C.linearize body
      val stms'   = C.traceSchedule(C.basicBlocks stms)
      val instrs  = List.concat(map (CG.codegen frame) stms')

      (* Printing instr selection *)
      val format' = A.format (F.makestring)
      val _       = app (fn i => TextIO.output(TextIO.stdOut, format' i)) instrs

      (* Continue *)
      val instrs' = F.procEntryExit2(frame, instrs)
      val flow    = MG.instrs2graph(instrs')

      val _       = printFlow flow
    in
      ()
    end
    | emitproc (F.STRING (lab, str)) = TextIO.output(TextIO.stdOut, (Temp.labelToString lab) ^ ": " ^ str ^ "\n")
  fun compile file =
    let
      val ast = Parse.parse file
    in
      if !ErrorMsg.anyErrors
        then print "Errors with file syntax. Stopping compilation.\n"
        else (print ("Parsing file: " ^ file ^ "\n");
              PrintAbsyn.print (TextIO.stdOut, ast);
              print "Semanting Analysis: \n";
              FindEscape.findEscape ast;
              resetAll();
              let
                val frags = S.transProg ast;
              in
                if !ErrorMsg.anyErrors
                then print "Errors with Semantic analysis. Stopping compilation.\n"
                else (print "Generating IR Tree: \n";
                      List.app R.printFrag frags;
                      print "Going to step 2 :)\n";
                      List.app emitproc frags)
              end;
              ())
    end
end

val _ = Main.compile "../tests/test1.tig";

