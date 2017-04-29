functor RegAlloc(F: FRAME) =
struct
  structure C = Color(F)
  structure R = Translate(F)

  fun allocate (instrs, frame, verbose) =
    let
      val format = Assem.format (F.makeString)

      fun makeflowgraph inst =
        let
          val _ = if verbose >= 1
                  then print "Making a flow graph...\n"
                  else ()
          val flow = MakeGraph.instrs2graph(inst)
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors making a flow graph. " ^
                       "Stopping compilation.\n");
                (flow, !ErrorMsg.anyErrors))
          else (if verbose > 2
                then (print "==========Printing Flow Graph==========\n";
                      Flow.printFlow (flow, format, F.makeString))
                else ();
                (flow, !ErrorMsg.anyErrors))
        end

      fun makeigraph flow =
        let
          val _ = if verbose >= 1
                  then print "Making a interference graph...\n"
                  else ()
          val igraph = Liveness.interferenceGraph(flow)
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors making an interference graph. " ^
                       "Stopping compilation.\n");
                (igraph, !ErrorMsg.anyErrors))
          else (if verbose > 2
                then (print "==========Printing Interference Graph==========\n";
                      Liveness.show (TextIO.stdOut, igraph, F.makeString))
                else ();
                (igraph, !ErrorMsg.anyErrors))
        end

      fun color igraph =
        let
          val _ = if verbose >= 1
                  then print "Coloring registers...\n"
                  else ()
          val (alloc, spills) = C.color {igraph=igraph,
                                         spillCost=(fn x => 1)}
        in
          if !ErrorMsg.anyErrors
          then (print ("Errors coloring graph. " ^
                       "Stopping compilations.\n");
                ((alloc, spills), !ErrorMsg.anyErrors))
          else ((alloc, spills), !ErrorMsg.anyErrors)
        end
    in
      case (makeflowgraph instrs) of
        (flow, true) => NONE
      | (flow, false) =>
      case (makeigraph flow) of
        (igraph, true) => NONE
      | (igraph, false) =>
      case (color igraph) of
        ((alloc, spills), true) => NONE
      | ((alloc, spills), false) =>
      if List.null spills
      then SOME {ins=instrs, alloc=alloc, frame=frame}
      else let
        val _ = print ("doing again? " ^ Int.toString (List.length spills) ^ "!!\n")
        val _ = print ("spilled " ^ Temp.makeString (List.hd spills) ^ "!!\n")
             val regStacks =
               List.foldr (fn (x, ans) => (x, F.allocLocal frame true)::ans)
                          [] spills

             fun addFetch ((t, ac), ans) = (F.loadLocal ac t)::ans
             fun addStore ((t, ac), ans) = (F.storeLocal ac t)::ans

             fun filt l = List.filter
                            (fn (x, ac) => (List.exists (fn y => y = x) l))
                            regStacks

             fun addStoreFetch
               ((a as Assem.OPER {assem, src, dst, jump})::rest) =
                  (List.foldr addFetch [] (filt src)) @
                  [a] @
                  (List.foldr addStore [] (filt dst)) @
                  (addStoreFetch rest)
               | addStoreFetch ((a as Assem.MOVE {assem, src, dst})::rest) =
                  (List.foldr addFetch [] (filt [src])) @
                  [a] @
                  (List.foldr addStore [] (filt [dst])) @
                  (addStoreFetch rest)
               | addStoreFetch ((Assem.LABEL {assem, lab})::rest) =
                  addStoreFetch rest
               | addStoreFetch [] = []
           in
             allocate (addStoreFetch instrs, frame, verbose)
           end
    end
end
