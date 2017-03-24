signature TRANSLATE =
sig
	type level
	type access
  type exp

	val newLevel   : {parent: level, name: Temp.label, formals: bool list} -> level
	val outermost  : level
	val formals    : level -> access list
	val allocLocal : level -> bool -> access

	val simpleVarAccess : access * level -> exp
	val arrayVarAccess  : exp * exp -> exp

	val nilExp : unit -> exp
end