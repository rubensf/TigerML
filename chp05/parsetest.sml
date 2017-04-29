structure Parse : 
	  sig
	      val parse : string -> Absyn.exp
	      val parseTestcase : string -> Absyn.exp
	      val parseAllTests : unit -> Absyn.exp list
        val test : string -> unit
        val testLocal : string -> unit
        val testAll : unit -> unit
	  end =
struct 
structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
fun parse filename =
  let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
      val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
      val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
  in TextIO.closeIn file;
     absyn
  end handle LrParser.ParseError => raise ErrorMsg.Error
					  
fun parseTestcase name =
  parse ("../tests/" ^ name ^ ".tig")

fun test file =
  let val exp = parseTestcase file
      val ostream = TextIO.openOut ("./out/" ^ file ^ ".out")
  in
      PrintAbsyn.print (ostream, exp)
  end

fun testLocal file =
  let val exp = parse file
      val ostream = TextIO.openOut("./out/" ^ file ^ "_local.out")
  in
      PrintAbsyn.print (ostream, exp)
  end

fun getAllTestCases () =
  let val tests = ["merge", "queens"];
      fun addTests (0, tests) = tests
        | addTests (n, tests) = addTests (n - 1, ("test" ^ Int.toString n)::tests)
  in
      addTests (50, tests)
  end

fun parseAllTests () =
  map parseTestcase (getAllTestCases ())

fun testAll () =
  app test (getAllTestCases ())
end
