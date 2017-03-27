structure Main =
struct
	structure R = Translate (MipsFrame)
	structure S = Semant (R)

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
			        R.resetFrags ();
			        let
			          val frags = S.transProg ast;
			        in
			        	if !ErrorMsg.anyErrors
			        	then print "Errors with Semantic analysis. Stopping compilation.\n"
			        	else List.app R.printFrag (R.getResult ())
			        end;
			        ())
		end
end