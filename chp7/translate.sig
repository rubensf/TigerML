signature TRANSLATE =
sig
	type level
	type access
  type exp

	val newLevel   : {parent: level, name: Temp.label, formals: bool list} -> level
	val outermost  : level
	val formals    : level -> access list
	val allocLocal : level -> bool -> access

	val intOpExp : Absyn.oper * exp * exp -> exp
	val strOpExp : Absyn.oper * exp * exp -> exp

	val intExp : int -> exp
	val strExp : string -> exp
	val nilExp : unit -> exp

	val simpleVarAccess : access * level -> exp
	val arrayVarAccess  : exp * exp -> exp
	val fieldVarAccess : exp * exp -> exp

	val letExp        : exp list * exp -> exp
	val whileExp      : exp * exp * Tree.label -> exp
	val forExp        : exp * 'a * exp * exp * exp * Tree.label -> exp
	val breakExp      : Tree.label -> exp
	val ifThenExp     : exp * exp -> exp
	val ifThenElseExp : exp * exp * exp -> exp
	val seqExp        : exp list -> exp
	val assignExp     : exp * exp -> exp

	val errExp : unit -> exp
end