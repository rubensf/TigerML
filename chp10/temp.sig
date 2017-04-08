signature TEMP =
sig
  eqtype temp
  type label = Symbol.symbol (* TODO hide this *)

  val newtemp       : unit -> temp
  val newlabel      : unit -> label
  val makestring    : temp -> string
  val namedlabel    : string -> label
  val labelToString : label -> string
  val reset         : unit -> unit
  val compare       : temp * temp -> order
  structure Set     : ORD_SET sharing type Set.Key.ord_key = temp
  structure Map     : ORD_MAP sharing type Map.Key.ord_key = temp
end

