structure Translate :> TRANSLATE =
struct
	structure F = MipsFrame (* TODO: How to choose this at runtime? *)

	type level = int
	type access = level * F.access

	fun newLevel {parent: level, name: Temp.label, formals: bool list} =
		(F.newFrame {name=name, formals=formals}; parent + 1)

  type exp = unit
  fun nilExp() = ()
end
