structure X86Frame :> FRAME =
struct
  open Tree (* We use it a lot in here. *)

  datatype access = InFrame of int |
                    InReg of Temp.temp
  type frame      = Temp.label * access list * int ref
  datatype frag   = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
  type register   = string

  val wordSize = 4

  val eax = Temp.newtemp ()
  val ebx = Temp.newtemp ()
  val ecx = Temp.newtemp ()
  val edx = Temp.newtemp ()

  val edi = Temp.newtemp () (* Probably never gonna use this *)
  val esi = Temp.newtemp () (* Or this *)

  val eip = Temp.newtemp () (* Also probs never gonna use intruction ptr *)
  val esp = Temp.newtemp () (* Stack pointer *)
  val ebp = Temp.newtemp () (* Frame pointer *)

  val fp = ebp
  val sp = esp
  val rv = eax
  val ra = ebp (* A hack - in fact we want [ebp + 4] *)

  val specialRegs = [
    (eip, "eip"),
    (ebp, "ebp"),
    (esp, "esp"),
  ]

  val argsRegs = [
  ]

  val calleeRegs = [
    (ebx, "ebx"),
    (edi, "edi"),
    (esi, "esi"),
  ]

  val callerRegs = [
    (eax, "eax"),
    (ecx, "ecx"),
    (edx, "edx"),
  ]

  val tempMap =
    let
      fun map_add ((t,s), map) = Temp.Map.insert(map, t, s);
    in
      foldr map_add Temp.Map.empty (specialRegs @ argsRegs @ callerRegs @ calleeRegs)
    end

  fun makestring (t:Temp.temp) = case Temp.Map.find(tempMap, t) of
    SOME(s) => s
  | NONE    => Temp.makestring t
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

  fun getRegTemps l = map (fn (t, r) => t) l

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

  fun procEntryExit2 (frame, body) =
    body @ [Assem.OPER {assem="jr      `s0\n",
                        src=[ra]@getRegTemps (specialRegs @ calleeRegs),
                        dst=[],jump=SOME[]}]

  fun procEntryExit3 ({name, params, locals}, body) = {
    prolog = "PROCEDURE " ^ Symbol.name name ^ "\n",
    body = body,
    epilog = "END " ^ Symbol.name name ^ "\n"
  }
end
