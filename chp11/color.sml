signature COLOR = 
sig
  structure Frame : FRAME
  structure FG : FUNCGRAPH
  type allocation = Frame.register Temp.Map.map
  val color: {
    igraph: Liveness.igraph,
    initial: allocation,
    spillCost: Temp.temp FG.node -> int,
    registers: Frame.register list,
    moves: (Temp.temp FG.node * Temp.temp FG.node) list
  } -> allocation * Temp.temp list
end

structure Color :> COLOR = 
struct
  structure Frame = MipsFrame
  structure T = Temp
  structure TempOrdKey = struct type ord_key = Temp.temp
                                val compare = Temp.compare
                         end
  structure FG = FuncGraph(TempOrdKey)
  type allocation = Frame.register Temp.Map.map
  fun color {igraph, initial, spillCost, registers, movelist} =
    ()
end