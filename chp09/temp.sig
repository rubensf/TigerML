signature TEMP =
sig
  eqtype temp
  type label = Symbol.symbol (* TODO hide this *)

  structure Table : TABLE sharing type Table.key = temp

  val newtemp       : unit -> temp
  val newlabel      : unit -> label
  val makestring    : temp -> string
  val namedlabel    : string -> label
  val labelToString : label -> string
  val reset         : unit -> unit
end

