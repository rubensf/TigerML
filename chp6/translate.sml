structure Translate :> TRANSLATE =
struct
	structure F = MipsFrame (* TODO: How to choose this at runtime? *)

	type level = int
	type access = level * F.access

	val outermost = 0

	fun newLevel {parent: level, name: Temp.label, formals: bool list} =
		(F.newFrame {name=name, formals=formals}; parent + 1)
	fun formals l = []
	fun allocLocal (l: level) = (fn (x: bool) => (0, F.allocLocal(F.newFrame {name=Temp.newlabel (), formals=[]})(true)))

  type exp = unit
  fun nilExp() = ()
end
