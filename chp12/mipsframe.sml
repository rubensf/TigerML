structure MipsFrame :> FRAME
where type register = string =
struct
  open Tree (* We use it a lot in here. *)

  datatype access = InFrame of int |
                    InReg of Temp.temp
  type frame      = Temp.label * access list * int ref
  datatype frag   = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
  type register   = string

  val wordSize = 4

  val rv = "$rv"
  val fp = "$fp"
  val ra = "$ra"

  val specialRegs = [
    "$r0",
    "$at",
    "$rv",
    "$v1",
    "$k0",
    "$k1",
    "$gp",
    "$sp",
    "$fp",
    "$ra"
  ]

  val argsRegs = [
    "$a0",
    "$a1",
    "$a2",
    "$a3"
  ]

  val calleeRegs = [
    "$s0",
    "$s1",
    "$s2",
    "$s3",
    "$s4",
    "$s5",
    "$s6",
    "$s7"
  ]

  val callerRegs = [
    "$t0",
    "$t1",
    "$t2",
    "$t3",
    "$t4",
    "$t5",
    "$t6",
    "$t7",
    "$t8",
    "$t9"
  ]

  fun regToString r = r

  val allRegisters = specialRegs @ argsRegs @ callerRegs @ calleeRegs
  val colorRegisters = callerRegs @ calleeRegs

  structure StringKeyOrd = struct type ord_key = string
                                  val compare = String.compare
                           end
  structure StringMap = SplayMapFn(StringKeyOrd)

  val regTempMap : Temp.temp StringMap.map ref = ref StringMap.empty
  val tempRegMap : register Temp.Map.map ref = ref Temp.Map.empty

  fun resetRegs () =
    let
      fun addToMaps (reg, (regTemp, tempReg)) =
        let
          val t = Temp.newtemp ()
        in
          (StringMap.insert(regTemp, reg, t),
           Temp.Map.insert(tempReg, t, reg))
        end

      val (regTempMap', tempRegMap') =
        foldl addToMaps (StringMap.empty, Temp.Map.empty) allRegisters
    in
      regTempMap := regTempMap';
      tempRegMap := tempRegMap'
    end

  (* TODO: This mail fail >_> *)
  fun getRegTemp reg = Option.valOf (StringMap.find (!regTempMap, reg))

  fun getTempReg temp = Temp.Map.find (!tempRegMap, temp)

  fun makestring t =
    case Temp.Map.find(!tempRegMap, t) of
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
                        src=[getRegTemp ra]@List.map getRegTemp (specialRegs @ calleeRegs),
                        dst=[],jump=SOME[]}]

  fun procEntryExit3 ({name, params, locals}, body) = {
    prolog = "PROCEDURE " ^ Symbol.name name ^ "\n",
    body = body,
    epilog = "END " ^ Symbol.name name ^ "\n"
  }
end
