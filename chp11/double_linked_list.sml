structure DoubleLinkedList :> LINKED_LIST =
struct
  structure Node =
  struct
    datatype 'a t = N of {content: 'a,
                          prev: 'a t option ref,
                          next: 'a t option ref}

    exception NoLink
    fun new n = N {content=n, prev=ref NONE, next=ref NONE}

    fun getValue (N {content,...}) = content

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

    fun searchL (n as N {content, next=ref NONE,...}) v =
          if (v = content)
          then SOME n
          else NONE
      | searchL (n as N {content, next=ref (SOME nx),...}) v =
          if (v = content)
          then SOME n
          else searchL nx v
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
    if (!first) = (!last)
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
    if (!first) = (!last)
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

  fun has (T {first,...}) v =
    (case (Node.searchL (!first) v) of
       SOME r => true
     | NONE   => false)
    | has EMPTY v =
      false

  fun remove (T {first,last}) v =
        if (!first) = (!last) andalso v = (Node.getValue (!first))
        then EMPTY
        else if v = (Node.getValue (!first))
        then let val next = Node.getNextV (!first)
             in Node.clearPrev (next);
                T{first=ref next, last=last}
             end
        else if v = (Node.getValue (!last))
        then let val prev = Node.getPrevV (!last)
             in Node.clearNext (prev);
                T{first=first, last=ref prev}
             end
        else (case (Node.searchL (!first) v) of
                NONE   => T{first=first, last=last}
              | SOME n => let
                            val prevV = Node.getPrevV n
                            val nextV = Node.getNextV n
                          in
                            Node.setNext (prevV, nextV);
                            Node.setPrev (nextV, prevV);
                            T{first=first, last=last}
                          end)
    | remove EMPTY v = EMPTY

  fun fromList l =
    List.foldl
      (fn (x, ans) => addLast (ans, x))
      EMPTY
      l

  fun toList (T {last,...}) =
    Node.foldR (op ::) [] (!last)
    | toList EMPTY = []
end

