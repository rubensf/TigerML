structure DoubleLinkedList :> LINKED_LIST =
struct
  structure Node =
  struct
    datatype 'a t = N of {content: 'a,
                          prev: 'a t option ref,
                          next: 'a t option ref}

    exception NoLink
    fun new n = N {content=n, prev=ref NONE, next=ref NONE}

    fun getPrev (N {prev,...}) = (!prev)
    fun getNext (N {next,...}) = (!next)

    fun getPrevV (N {prev,...}) = if (Option.isSome (!prev))
                                  then Option.valOf (!prev)
                                  else raise NoLink
    fun getNextV (N {next,...}) = if (Option.isSome (!next))
                                  then Option.valOf (!next)
                                  else raise NoLink

    fun setPrev (N {prev,...}, prev') = prev := SOME prev'
    fun setNext (N {next,...}, next') = next := SOME next'

    fun clearPrev (N {prev,...}) = prev := NONE
    fun clearNext (N {next,...}) = next := NONE

    fun appL f (N {content, next=ref NONE,...}) = ()
      | appL f (N {content, next=ref (SOME n),...}) =
          (f content; appL f n)

    fun appR f (N {content, prev=ref NONE,...}) = ()
      | appR f (N {content, prev=ref (SOME p),...}) =
          (f content; appR f p)

    fun foldL f ans (N {content, next=ref NONE,...}) = ans
      | foldL f ans (N {content, next=ref (SOME n),...}) =
          (foldL f (f (content, ans)) n)
    fun foldR f ans (N {content, prev=ref NONE,...}) = ans
      | foldR f ans (N {content, prev=ref (SOME p),...}) =
          (foldR f (f (content, ans)) p)

  end

  exception EmptyLinkedList

  datatype 'a t = T of {first: 'a Node.t ref,
                        last:  'a Node.t ref}
                 | EMPTY

  val empty = EMPTY

  fun isEmpty EMPTY = true
    | isEmpty t     = false

  fun addFirst (T {first, last}, n) =
        let val newN = Node.new n
        in
          Node.setPrev (!first, newN);
          Node.setNext (newN, !first);
          T {first=ref newN, last=last}
        end
    | addFirst (EMPTY, n) =
        let val newN = Node.new n
        in T {first=ref newN, last=ref newN}
        end
  fun addFirst' (n, ll) = addFirst(ll, n)

  fun peekFirst (T {first=ref (Node.N {content,...}),...}) = content
    | peekFirst EMPTY = raise EmptyLinkedList

  fun popFirst (T {first,last}) =
    if first = last
    then EMPTY
    else (Node.clearPrev (Node.getNextV (!first));
          T {first=ref (Node.getNextV (!first)),
             last=last})
    | popFirst EMPTY = raise EmptyLinkedList


  fun addLast (T {first, last}, n) =
        let val newN = Node.new n
        in
          Node.setNext (!last, newN);
          Node.setPrev (newN, !last);
          T {first=first, last=ref newN}
        end
    | addLast (EMPTY, n) =
        let val newN = Node.new n
        in T {first=ref newN, last=ref newN}
        end
  fun addLast' (n, ll) = addLast(ll, n)

  fun peekLast (T {last=ref (Node.N {content,...}),...}) = content
    | peekLast EMPTY = raise EmptyLinkedList

  fun popLast (T {first, last}) =
    if first = last
    then EMPTY
    else (Node.clearNext (Node.getPrevV (!last));
          T {first=first,
             last=ref (Node.getPrevV (!last))})
    | popLast EMPTY = raise EmptyLinkedList

  fun appl f (T {first,...}) =
    Node.appL f (!first)
    | appl f EMPTY = ()
  fun appr f (T {last,...}) =
    Node.appR f (!last)
    | appr f EMPTY = ()

  fun foldl f init (T {first,...}) =
    Node.foldL f init (!first)
    | foldl f init EMPTY = init
  fun foldr f init (T {last,...}) =
    Node.foldR f init (!last)
    | foldr f init EMPTY = init

  fun fromList l =
    List.foldl
      (fn (x, ans) => addLast (ans, x))
      EMPTY
      l

  fun toList (T {last,...}) =
    Node.foldR (op ::) [] (!last)
    | toList EMPTY = []
end

