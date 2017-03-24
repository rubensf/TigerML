signature FRAME =
sig
	type frame
	type access

	datatype frag = PROC of {body: Tree.stm, frame: frame}
								| STRING of Temp.label

	val newFrame   : {name: Temp.label, formals: bool list} -> frame
	val name       : frame -> Temp.label
	val getOffset  : frame -> int
	val formals    : frame -> access list
	val allocLocal : frame -> bool -> access

	val wordSize : int

	(* Special registers *)
	val fp       : Temp.temp
	val rv       : Temp.temp

	(* Get's the specific access function. *)
	val expfn    : access -> Tree.exp -> Tree.exp

	(* Function "decorators" - add prologue and epilogue standard mumbo jumbo *)
	val procEntryExit : frame * Tree.exp -> Tree.exp
end