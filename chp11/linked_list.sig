signature LINKED_LIST =
sig
  eqtype ''a t

  val empty : ''a t
  val isEmpty : ''a t -> bool

  val addFirst  : (''a t * ''a) -> ''a t
  val addFirst' : (''a * ''a t) -> ''a t
  val peekFirst : ''a t -> ''a
  val popFirst  : ''a t -> ''a t

  val addLast  : (''a t * ''a) -> ''a t
  val addLast' : (''a * ''a t) -> ''a t
  val peekLast : ''a t -> ''a
  val popLast  : ''a t -> ''a t

  val foldl : (''a * 'b -> 'b) -> 'b -> ''a t -> 'b
  val foldr : (''a * 'b -> 'b) -> 'b -> ''a t -> 'b
  val appl  : (''a -> unit) -> ''a t -> unit
  val appr  : (''a -> unit) -> ''a t -> unit

  val has    : ''a t -> ''a -> bool
  val remove : ''a t -> ''a -> ''a t

  val fromList : ''a list -> ''a t
  val toList   : ''a t -> ''a list
end
