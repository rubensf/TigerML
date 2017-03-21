structure Translate :> TRANSLATE =
struct
	structure F = MipsFrame (* TODO: How to choose this at runtime? *)

	datatype level = Level of {parent: level,
                             name: Temp.label,
	                           formals: bool list,
								             frame: F.frame}
								 | Outermost
	datatype access = Access of level * F.access

	val outerName = Temp.newlabel ()
	val outermost = Outermost (* TODO: not sure if outermost type is good idea *)

	fun newLevel {parent: level, name: Temp.label, formals: bool list} =
		Level {parent=parent, name=name, formals=formals, frame=F.newFrame {name=name, formals=formals}}

	fun formals (lev: level) =
		case lev of
			Level l => foldl (fn (formal, ans) => (Access (lev, formal))::ans)
			             [] (F.formals (#frame l))

	fun allocLocal (lev: level) esc =
		case lev of
			Level l => Access (lev, F.allocLocal(#frame l) esc)

  type exp = unit
  fun nilExp() = ()
end
