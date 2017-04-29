structure MipsFrame :> FRAME =
struct
  open Tree (* We use it a lot in here. *)

  datatype access = InFrame of int |
                    InReg of Temp.temp

  type frame      = Temp.label * access list * int ref

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label

  val wordSize = 4

  val fp = Temp.newtemp ()
  val rv = Temp.newtemp ()

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
      true  => ((#3 f) := !(#3 f)-wordSize;InFrame (!(#3 f)+wordSize))
    | false => (InReg (Temp.newtemp()))

  fun resetFrame (f: frame) = (#3 f) := 0

  fun expFn (InFrame offset) = (fn fptr => MEM(BINOP(PLUS, fptr, CONST offset)))
    | expFn (InReg reg) = (fn fptr => TEMP reg)

  fun externCallFn (s, args) =
    CALL (NAME (Temp.namedlabel s), args)

  fun procEntryExit (frame, treeExp) = treeExp (* TODO *)
end