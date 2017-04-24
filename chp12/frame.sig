signature FRAME =
sig
  eqtype register
  type access
  type frame

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val newFrame   : {name: Temp.label, formals: bool list} -> frame
  val name       : frame -> Temp.label
  val getOffset  : frame -> int
  val formals    : frame -> access list
  val allocLocal : frame -> bool -> access
  val resetFrame : frame -> unit

  val wordSize : int

  val fp          : register
  val rv          : register
  val ra          : register

  val specialRegs : register list
  val argsRegs    : register list
  val calleeRegs  : register list
  val callerRegs  : register list

  val allRegisters   : register list
  val colorRegisters : register list
  val regToString    : register -> string
  val getRegTemp     : register -> Temp.temp
  val getTempReg     : Temp.temp -> register option

  (* MUST be called after a Temp reset *)
  val resetRegs      : unit -> unit

  (* Auxiliar, arch dependent functions *)
  val expFn        : access -> Tree.exp -> Tree.exp (* Get's the specific access function. *)
  val externCallFn : string * Tree.exp list -> Tree.exp (* Calls an external function, such as C heap management. *)

  (* Function "decorators" - add prologue and epilogue standard mumbo jumbo *)
  val procEntryExit  : frame * Tree.exp -> Tree.exp
  (* Insert a final "instruction" to make sure that special registers and callee saved
     ones are "live" at the end of a function execution. *)
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

  val makestring  : Temp.temp -> string
end
