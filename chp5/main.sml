structure Main =
struct
	fun parse file =
		let
			val ast = Parse.parse file
		in
			print "Parsing file: \n";
			PrintAbsyn.print (TextIO.stdOut, ast);
			print "Semanting Analysis: \n";
			Semant.transProg ast
		end
end