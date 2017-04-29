signature CODEGEN =
sig
  structure F : FRAME

  type frame = F.frame

  val codegen : F.frame -> Tree.stm -> Assem.instr list
end