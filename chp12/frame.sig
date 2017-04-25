signature FRAME =
sig
  eqtype register
  type access
  type frame

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  (* Frame utilities *)
  val wordSize   : int

  val newFrame   : {name: Temp.label, parameters: bool list} -> frame
  val name       : frame -> Temp.label
  val parameters : frame -> access list
  val getOffset  : frame -> int
  val allocLocal : frame -> bool -> access
  val resetFrame : frame -> unit

  (* Default common-place registers *)
  val fp          : register
  val sp          : register
  val rv          : register
  val ra          : register

  (* Lists of registers by category *)
  val specialRegs : register list
  val argsRegs    : register list
  val calleeRegs  : register list
  val callerRegs  : register list

  val allRegisters   : register list

  (* Register Utilities *)
  val regToString : register -> string
  val getRegTemp  : register -> Temp.temp
  val getTempReg  : Temp.temp -> register option
  val makeString  : Temp.temp -> string

  (* MUST be called after a Temp reset *)
  val resetRegs : unit -> unit

  (* Auxiliar, arch dependent functions *)
  val expFn        : access -> Tree.exp -> Tree.exp (* Get's the specific access function. *)
  val externCallFn : string * Tree.exp list -> Tree.exp (* Calls an external function, such as C heap management. *)

  (* Function "decorators" - add prologue and epilogue standard mumbo jumbo *)
  val procEntryExit  : frame * Tree.stm -> Tree.stm
  (* Insert a final "instruction" to make sure that special registers and callee saved
     ones are "live" at the end of a function execution. *)
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

end