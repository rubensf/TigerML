structure Translate :> TRANSLATE =
struct
	structure F = MipsFrame (* TODO: How to choose this at runtime? *)

	type level = int * F.frame
	type access = level * F.access

	val outermost = (0, F.newFrame {name= Temp.newlabel (), formals=[]})

	fun newLevel {parent: level, name: Temp.label, formals: bool list} =
		let
			val newframe = F.newFrame {name=name, formals=formals}
		in
			(#1 parent + 1, newframe)
		end

	(* TODO all of the below *)
	fun formals l = #2 (#2 l) (* get acc list from frame within level? *)
	fun allocLocal (l: level) = (fn (x: bool) => (l, F.allocLocal(F.newFrame {name=Temp.newlabel (), formals=[]})(true)))

  type exp = unit
  fun nilExp() = ()
end
