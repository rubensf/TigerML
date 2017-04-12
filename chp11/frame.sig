signature FRAME =
sig
  type access
  type frame
  type register

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val newFrame   : {name: Temp.label, formals: bool list} -> frame
  val name       : frame -> Temp.label
  val getOffset  : frame -> int
  val formals    : frame -> access list
  val allocLocal : frame -> bool -> access

  val resetFrame : frame -> unit

  val wordSize : int

  (* Special registers *)
  val r0        : Temp.temp
  val at        : Temp.temp
  val rv        : Temp.temp
  val v1        : Temp.temp
  val k0        : Temp.temp
  val k1        : Temp.temp
  val gp        : Temp.temp
  val sp        : Temp.temp
  val fp        : Temp.temp
  val ra        : Temp.temp

  (* General registers *)
  val specialRegs : (Temp.temp * register) list
  val argsRegs    : (Temp.temp * register) list
  val calleeRegs  : (Temp.temp * register) list
  val callerRegs  : (Temp.temp * register) list

  (* Auxiliar, arch dependent functions *)
  val expFn        : access -> Tree.exp -> Tree.exp (* Get's the specific access function. *)
  val externCallFn : string * Tree.exp list -> Tree.exp (* Calls an external function, such as C heap management. *)

  (* Function "decorators" - add prologue and epilogue standard mumbo jumbo *)
  val procEntryExit  : frame * Tree.exp -> Tree.exp
  (* Insert a final "instruction" to make sure that special registers and callee saved
     ones are "live" at the end of a function execution. *)
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

  val tempMap : register Temp.Map.map
  val makestring : Temp.temp -> string
  val getRegTemps : (Temp.temp * register) list -> Temp.temp list
end