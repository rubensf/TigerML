signature FRAME =
sig
	type frame
	type access

	val newFrame   : {name: Temp.label, formals: bool list} -> frame
	val name       : frame -> Temp.label
	val getOffset  : frame -> int
	val formals    : frame -> access list
	val allocLocal : frame -> bool -> access

	val fp       : Temp.temp
	val wordSize : int
	(* Get's the specific access function. *)
	val expfn    : access -> Tree.exp -> Tree.exp
end