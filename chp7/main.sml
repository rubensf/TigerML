structure Main =
struct
	fun compile file =
		let
			val ast = Parse.parse file

			fun doAll (MipsFrame.PROC {body, frame}) = Printtree.printtree (TextIO.stdOut, body)
				| doAll (MipsFrame.STRING lbl) = print (Temp.getlabeltxt lbl)
		in
			if !ErrorMsg.anyErrors
			  then print "Errors with file syntax. Stopping compilation.\n"
			  else (print ("Parsing file: " ^ file ^ "\n");
			        PrintAbsyn.print (TextIO.stdOut, ast);
			        print "Semanting Analysis: \n";
			        FindEscape.findEscape ast;
			        Semant.transProg ast;

			        ())
		end
end