signature TRANSLATE =
sig
	type level
	type access

	val newLevel   : {parent: level, name: Temp.label, formals: bool list} -> level
	val outermost  : level
	val formals    : level -> access list
	val allocLocal : level -> bool -> access

	(* TODO delete later? *)
  type exp = unit
  val nilExp : unit -> unit
end