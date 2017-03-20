structure MipsFrame :> FRAME =
struct
	datatype access = InFrame of int |
										InReg of Temp.temp
	type frame      = Temp.label * access list

	fun name (f: frame)       = #1 f
	fun formals (f: frame)    = #2 f

	(* TODO all of the below *)
	fun newFrame {name: Temp.label, formals: bool list} = (Temp.newlabel (), [])
	fun allocLocal (f: frame) = (fn (x: bool) => InFrame 0)
end