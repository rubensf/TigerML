structure MipsFrame :> FRAME
where type register = string =
struct
  open Tree (* We use it a lot in here. *)

  datatype access = InFrame of int |
                    InReg of Temp.temp
  type frame      = {name: Temp.label,
                     parameters: access list,
                     localsOffset: int ref}
  datatype frag   = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
  type register   = string

  val wordSize = 4

  fun newFrame {name, parameters} =
    let
      val emptyAddr = ref (Int.~ wordSize)
      fun buildFrame (addr, nil) = (emptyAddr := addr; nil)
        | buildFrame (addr, f::l) =
            case f of
              true  => (InFrame addr)::buildFrame(addr-wordSize, l)
            | false => (InReg (Temp.newtemp()))::buildFrame(addr, l)
    in
      {name=name,
       parameters=buildFrame(0, parameters),
       localsOffset=emptyAddr}
    end

  fun name       (f: frame) = #name f
  fun parameters (f: frame) = #parameters f
  fun getOffset  (f: frame) = !(#localsOffset f)

  fun allocLocal ({name, parameters, localsOffset}: frame) esc =
    if esc
    then (localsOffset := (!localsOffset)-wordSize;
          InFrame (!localsOffset))
    else InReg (Temp.newtemp ())

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

  val allRegisters = specialRegs @ argsRegs @ callerRegs @ calleeRegs

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

  fun resetFrame {name, parameters, localsOffset} = localsOffset := 0

  fun regToString r = r
  (* TODO: This mail fail >_> *)
  fun getRegTemp reg = Option.valOf (StringMap.find (!regTempMap, reg))
  fun getTempReg temp = Temp.Map.find (!tempRegMap, temp)
  fun makeString t =
    case Temp.Map.find(!tempRegMap, t) of
      SOME(s) => s
    | NONE    => Temp.makeString t

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
