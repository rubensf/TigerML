structure Main =
struct
  structure CG = MipsGen
  structure R = Translate (MipsFrame)
  structure S = Semant (R)
  structure C = Canon
  structure A = Assem
  structure F = MipsFrame

  fun resetAll () = (R.resetFrags(); Temp.reset())

  fun emitproc out (F.PROC{body,frame}) =
    let
      val _       = print("emit " ^ (Temp.labelToString (F.name frame)) ^ "\n")
      val stms    = C.linearize body
      val stms'   = C.traceSchedule(C.basicBlocks stms)
      val instrs  = List.concat(map (CG.codegen frame) stms')
      val format0 = A.format (F.makestring)
    in
      app (fn i => TextIO.output(out, format0 i)) instrs
    end
    | emitproc out (F.STRING (lab, str)) = TextIO.output(out, (Temp.labelToString lab) ^ ": " ^ str ^ "\n")
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
                else (List.app R.printFrag (R.getResult ());(app (emitproc TextIO.stdOut) frags))
              end;
              ())
    end
end