signature CODEGEN =
sig
  type frame
  val codegen : frame -> Tree.stm -> Assem.instr list
end
