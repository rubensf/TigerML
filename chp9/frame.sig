signature FRAME =
sig
	type access
	type frame
	type register

	datatype frag = PROC of {body: Tree.stm, frame: frame}
								| STRING of Temp.label

	val newFrame   : {name: Temp.label, formals: bool list} -> frame
	val name       : frame -> Temp.label
	val getOffset  : frame -> int
	val formals    : frame -> access list
	val allocLocal : frame -> bool -> access

	val resetFrame : frame -> unit

	val wordSize : int

	(* Special registers *)
	val fp       : Temp.temp
	val rv       : Temp.temp

	(* Auxiliar, arch dependent functions *)
	val expFn        : access -> Tree.exp -> Tree.exp (* Get's the specific access function. *)
	val externCallFn : string * Tree.exp list -> Tree.exp (* Calls an external function, such as C heap management. *)

	(* Function "decorators" - add prologue and epilogue standard mumbo jumbo *)
	val procEntryExit : frame * Tree.exp -> Tree.exp
end