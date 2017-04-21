structure MipsFrame :> FRAME =
struct
  open Tree (* We use it a lot in here. *)

  datatype access = InFrame of int |
                    InReg of Temp.temp
  type frame      = Temp.label * access list * int ref
  datatype frag   = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
  type register   = string

  val wordSize = 4

  val r0 = Temp.newtemp ()
  val at = Temp.newtemp ()
  val rv = Temp.newtemp ()
  val v1 = Temp.newtemp ()
  val k0 = Temp.newtemp ()
  val k1 = Temp.newtemp ()
  val gp = Temp.newtemp ()
  val sp = Temp.newtemp ()
  val fp = Temp.newtemp ()
  val ra = Temp.newtemp ()

  val A0 = Temp.newtemp ()
  val A1 = Temp.newtemp ()
  val A2 = Temp.newtemp ()
  val A3 = Temp.newtemp ()

  val S0 = Temp.newtemp ()
  val S1 = Temp.newtemp ()
  val S2 = Temp.newtemp ()
  val S3 = Temp.newtemp ()
  val S4 = Temp.newtemp ()
  val S5 = Temp.newtemp ()
  val S6 = Temp.newtemp ()
  val S7 = Temp.newtemp ()

  val T0 = Temp.newtemp ()
  val T1 = Temp.newtemp ()
  val T2 = Temp.newtemp ()
  val T3 = Temp.newtemp ()
  val T4 = Temp.newtemp ()
  val T5 = Temp.newtemp ()
  val T6 = Temp.newtemp ()
  val T7 = Temp.newtemp ()
  val T8 = Temp.newtemp ()
  val T9 = Temp.newtemp ()

  val registers1 = ["$r0", "$at", "$rv", "$v1", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra", "$a0", "$a1", "$a2", "$a3"]
  val registers2 = ["$s0","$s1","$s2","$s3","$s4","$s5","$s6","$s7"]
  val registers3 = ["$t0","$t1","$t2","$t3","$t4","$t5","$t6","$t7","$t8","$t9"]
  
  val allRegisters = (registers1 @ registers2) @ registers3
  val colorRegisters = registers2 @ registers3

  val specialRegs = [
    (r0, "$r0"),
    (at, "$at"),
    (rv, "$rv"),
    (v1, "$v1"),
    (k0, "$k0"),
    (k1, "$k1"),
    (gp, "$gp"),
    (sp, "$sp"),
    (fp, "$fp"),
    (ra, "$ra")
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

  fun regToString r = r

  val tempMap =
    let
      fun map_add ((t,s), map) = Temp.Map.insert(map, t, s);
    in
      foldr map_add Temp.Map.empty (specialRegs @ argsRegs @ callerRegs @ calleeRegs)
    end

  fun makestring (t:Temp.temp) =
    case Temp.Map.find(tempMap, t) of
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

  fun procEntryExit (frame, treeExp) = 
    let
      val argTemps = getRegTemps argsRegs
      fun exp (access, addr) = expFn access addr
      fun moveArgs([], seq, offset) = seq
        | moveArgs(a::access, seq, offset) =
            if offset < 4
            then Tree.MOVE(exp(a, Tree.TEMP fp), Tree.TEMP (List.nth(argTemps, offset)))::moveArgs(access, seq, offset + 1)
            else 
              let
                val temp = Temp.newtemp()
              in
                case a of 
                     InFrame off => moveArgs(access, seq, offset + 1)
                   | InReg te => 
                       Tree.MOVE(exp(a, Tree.TEMP fp), Tree.TEMP te)::moveArgs(access, seq, offset + 1)
              end
      val moveArgStms = moveArgs(formals frame, [], 0)
    in
      seq (moveArgStms @ [treeExp])
    end

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
