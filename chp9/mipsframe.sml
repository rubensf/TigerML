structure MipsFrame :> FRAME =
struct
  open Tree (* We use it a lot in here. *)

  datatype access = InFrame of int |
                    InReg of Temp.temp
  type frame      = Temp.label * access list * int ref
  datatype frag   = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label
  type register   = string

  val wordSize = 4

  val fp = Temp.newtemp ()
  val sp = Temp.newtemp ()
  val rv = Temp.newtemp ()
  val ra = Temp.newtemp ()

  val specialRegs = [
    (R0, "$r0"),
    (RA, "$ra"),
    (SP, "$sp"),
    (FP, "$fp")
  ]

  val argsRegs = [
    (A0, "$a0"),
    (A1, "$a1"),
    (A2, "$a2"),
    (A3, "$a3")
  ]

  val calleeRegs = [
    (S0, "$s0"),
    (S1, "$s1"),
    (S2, "$s2"),
    (S3, "$s3"),
    (S4, "$s4"),
    (S5, "$s5"),
    (S6, "$s6"),
    (S7, "$s7")
  ]

  val callerRegs = [
    (T0, "$t0"),
    (T1, "$t1"),
    (T2, "$t2"),
    (T3, "$t3"),
    (T4, "$t4"),
    (T5, "$t5"),
    (T6, "$t6"),
    (T7, "$t7"),
    (T8, "$t8"),
    (T9, "$t9")
  ]

  val tempMap = 
    let
      val map_add_ ((t,s), map) = Temp.Table.enter(map, t, s);
    in
      foldr map_add Temp.table.empty specialRegs
    end

  fun makestring (t:Temp.temp) = case Temp.Table.look(tempMap, t) of
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

  fun getRegTemps = map (fn (t, r) => t)

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
    body @ [A.OPER {assem="",
                    src=getRegTemps (specialRegs @ calleeRegs),
                    dst=[],jump=SOME[]}
    ]

  fun procEntryExit3 (FRAME {name, params, locals}, body) = {
    prolog = "PROCEDURE " ^ Symbol.name name ^ "\n",
    body = body,
    epilog = "END " ^ Symbol.name name ^ "\n"
  }
end
