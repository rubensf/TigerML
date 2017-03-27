structure Main =
struct
	structure C = Canon
	structure R = Translate

	fun compile file =
		let
			val ast = Parse.parse file

			fun printFrag (MipsFrame.PROC {body, frame}) =
				  List.app (fn s => Printtree.printtree (TextIO.stdOut, s))
				           (C.traceSchedule (C.basicBlocks (C.linearize body)))
				| printFrag (MipsFrame.STRING lbl) = print ((Temp.getlabeltxt lbl) ^ "\n")
		in
			if !ErrorMsg.anyErrors
			  then print "Errors with file syntax. Stopping compilation.\n"
			  else (print ("Parsing file: " ^ file ^ "\n");
			        PrintAbsyn.print (TextIO.stdOut, ast);
			        print "Semanting Analysis: \n";
			        FindEscape.findEscape ast;
			        Translate.resetFrags ();
			        let
			          val frags = Semant.transProg ast;
			        in
			        	if !ErrorMsg.anyErrors
			        	then print "Errors with Semantic analysis. Stopping compilation.\n"
			        	else List.app printFrag frags
			        end;
			        ())
		end
end