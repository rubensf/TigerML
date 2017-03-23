structure MipsFrame :> FRAME =
struct
  open Tree (* We use it a lot in here. *)

  datatype access = InFrame of int |
                    InReg of Temp.temp
  type frame      = Temp.label * access list * int ref

  val fp = Temp.newtemp ()
  val wordSize = 4

  fun name (f: frame)       = #1 f
  fun formals (f: frame)    = #2 f

  fun newFrame {name: Temp.label, formals: bool list} =
    let
      val empty_addr = ref ~4
      fun acc_create (addr, nil) = (empty_addr:=addr;nil)
        | acc_create (addr, f::l) =
            case f of
              true  => (InFrame addr)::acc_create(addr-4, l)
            | false => (InReg (Temp.newtemp()))::acc_create(addr, l)
    in
      (name, acc_create(0, formals), empty_addr)
    end

  fun getOffset (f: frame) = !(#3 f)

  fun allocLocal (f: frame) (esc: bool) =
    case esc of
      true  => (InReg (Temp.newtemp()))
    | false => ((#3 f) := !(#3 f)-4;InFrame (!(#3 f)+4))

  fun expfn (InFrame offset) = (fn fptr => MEM(BINOP(PLUS, fptr, CONST offset)))
    | expfn (InReg reg) = (fn fptr => TEMP reg)
end