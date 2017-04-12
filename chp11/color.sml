signature COLOR = 
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val color: {
    igraph: Liveness.igraph,
    initial: allocation,
    spillCost: Graph.node -> int,
    registers: Frame.register list,
    moves: (T.temp FG.node * T.temp FG.node) list
  } -> allocation * Temp.temp list
end

structure Color :> COLOR = 
struct
  structure Frame = MipsFrame
  structure T = Temp
  type allocation = Frame.register Temp.Table.table

  fun color {igraph, initial, spillCost, registers, movelist} =
    ()
end