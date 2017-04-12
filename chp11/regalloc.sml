signature REGALLOC = 
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc :> REGALLOC = 
struct
  structure Frame = MipsFrame
  structure T = Temp 
  type allocation = Frame.register Temp.Table.table

end