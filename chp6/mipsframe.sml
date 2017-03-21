structure MipsFrame :> FRAME =
struct
	datatype access = InFrame of int |
										InReg of Temp.temp
	type frame      = Temp.label * access list

	fun name (f: frame)       = #1 f
	fun formals (f: frame)    = #2 f

	(* TODO all of the below *)
	fun newFrame {name: Temp.label, formals: bool list} = 
        let
            fun acc_create (addr, nil) = nil
                | acc_create (addr, f::l) = case f of 
                    true    => (InFrame addr)::acc_create(addr-4, l)
                    | false => (InReg (Temp.newtemp()))::acc_create(addr, l)
        in
            (name, acc_create(0, formals))
        end
	fun allocLocal (f: frame) = (fn (x: bool) => InFrame 0)
end