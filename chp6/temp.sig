signature TEMP =
sig
  eqtype temp
  type label = Symbol.symbol

  structure Table : TABLE sharing type Table.key = temp

  val newtemp : unit -> temp
  val newlabel : unit -> label
  val makestring: temp -> string
  val namedlabel : string -> label
end

