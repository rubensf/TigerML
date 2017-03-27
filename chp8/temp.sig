signature TEMP =
sig
  type temp = int
  type label = Symbol.symbol

  val newtemp     : unit -> temp
  val newlabel    : unit -> label
  val makestring  : temp -> string
  val namedlabel  : string -> label
  val getlabeltxt : label -> string
end

