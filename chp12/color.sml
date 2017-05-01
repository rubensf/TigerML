signature COLOR =
sig
  type allocation
  type registerlist
  type node

  val color: {
    igraph: Liveness.igraph,
    spillCost: node -> int
  } -> allocation * Temp.temp list
end

functor Color(F: FRAME) :> COLOR
where
  type allocation = F.register Temp.Map.map and
  type registerlist = F.register list and
  type node = Temp.temp Liveness.FG.node
=
struct
  structure F = F
  structure T = Temp
  structure FG = Liveness.FG

  val gnID = FG.getNodeID

  structure DoubleTempOrdKey =
    struct type ord_key = (Temp.temp * Temp.temp)
           val compare = (fn ((v11, v12), (v21, v22)) =>
                            let
                              val c1 = Temp.compare (v11, v21)
                              val c2 = Temp.compare (v12, v22)
                            in
                              case (c1, c2) of
                                (LESS, _)      => LESS
                              | (EQUAL, LESS)  => LESS
                              | (EQUAL, EQUAL) => EQUAL
                              | _              => GREATER
                            end)
    end

  structure DoubleTempSet = SplaySetFn(DoubleTempOrdKey)

  (* Shortcuts *)
  structure TM = Temp.Map
  structure TS = Temp.Set
  structure DTS = DoubleTempSet

  fun reorderNodes (v1, v2) =
    case Temp.compare (v1, v2) of
      LESS    => (v1, v2)
    | EQUAL   => (v1, v2)
    | GREATER => (v2, v1)

  type allocation = F.register TM.map
  type registerlist = F.register list
  type node = Temp.temp Liveness.FG.node

  fun color {igraph as Liveness.IGRAPH {graph, moves}, spillCost} =
    let
      val avalRegs = F.colorRegisters

      val nColors = (List.length avalRegs)

      val precoloredNodes = List.foldl
                              (fn (x, m) =>
                                 (TM.insert (m, (F.getRegTemp x), x)))
                              TM.empty F.allRegisters

      val allColors = TS.addList(
                        TS.empty,
                        List.map F.getRegTemp avalRegs)

      fun addToMap (mMap, v1, v2) =
        let val assocMove = TM.find (mMap, v1)
        in case assocMove of
             SOME nodeSet => TM.insert(mMap, v1, TS.add(nodeSet, v2))
           | NONE         => TM.insert(mMap, v1, TS.add(TS.empty, v2))
        end

      val (movesSet, movesMap) =
        List.foldl (
          fn ((v1, v2), (mSet, mMap)) =>
            let
              val v1 = (gnID v1)
              val v2 = (gnID v2)
              val mMap'  = addToMap(mMap , v1, v2)
              val mMap'' = addToMap(mMap', v2, v1)
              val mSet'  = DTS.add (mSet, (reorderNodes (v1, v2)))
            in
              (mSet', mMap'')
            end)
          (DTS.empty, TM.empty) moves

      fun isPrecolored nID =
        case F.getTempReg nID of
          SOME(r) => true
        | _       => false

      val graph = ref graph
      fun gNode nID = FG.getNode ((!graph), nID)

      val movesMap = ref movesMap

      val spillWL = ref []
      val freezeWL = ref []
      val simplifyWL = ref []

      val movesWL = ref movesSet
      val movesActive = ref DTS.empty
      val movesFrozen = ref DTS.empty
      val movesConstrained = ref DTS.empty
      val movesCoalesced = ref DTS.empty

      val selectStack = ref []
      val coalescedNodes = ref TS.empty
      val spilledNodes = ref TS.empty

      val alias = ref TM.empty
      val colors = ref (List.foldl
                               (fn (x, m) =>
                                  (TM.insert (m, (F.getRegTemp x),
                                                       (F.getRegTemp x))))
                               TM.empty F.allRegisters)

      fun adjacents nodeID =
        TS.difference(TS.addList(
                         TS.empty,
                         (List.map gnID (FG.adj' (!graph) (gNode nodeID)))),
                       TS.union(TS.addList(TS.empty, (!selectStack)),
                                 (!coalescedNodes)))

      fun filterMoves movesDTS nID =
        DTS.filter
          (fn (v1, v2) =>
             (v1 = nID orelse v2 = nID))
          movesDTS

      fun toFilteredMove movesDTS nID =
        TS.addList
          (TS.empty,
           List.map
             (fn (v1, v2) => if v1 = nID
                             then v2
                             else v1)
             (DTS.listItems (filterMoves movesDTS nID)))

      fun nodeMoves nID =
        let
          val mvs = TM.find ((!movesMap), nID)
          val mvsWL = toFilteredMove (!movesWL) nID
          val mvsAt = toFilteredMove (!movesActive) nID
          val mvsUnion = TS.union (mvsWL, mvsAt)
        in
          case mvs of
            SOME m => TS.intersection (m, mvsUnion)
          | NONE   => TS.empty
        end

      fun moveRelated nodeID =
        not (TS.isEmpty (nodeMoves nodeID))

      fun makeWLs node =
        let val nID = (gnID node)
        in
          if (isPrecolored nID)
          then ()
          else if FG.outDegree (node) >= nColors
          then spillWL := nID::(!spillWL)
          else if moveRelated nID
          then freezeWL := nID::(!freezeWL)
          else simplifyWL := nID::(!simplifyWL)
        end

      fun enableMoves nIDs =
        TS.app
          (fn nID =>
             TS.app
               (fn x =>
                  let
                    val nodeActiveMoves = filterMoves (!movesActive) nID
                  in
                    if (not (DTS.isEmpty nodeActiveMoves))
                    then (movesActive := DTS.difference ((!movesActive),
                                                          nodeActiveMoves);
                          movesWL := DTS.union ((!movesWL), nodeActiveMoves))
                    else ()
                  end)
               (nodeMoves nID))
          nIDs

      fun decrementoutDegreeEffects nID =
          if (FG.outDegree (gNode nID) = nColors) andalso
              not (isPrecolored nID)
          then (enableMoves (TS.add ((adjacents nID), nID));
                spillWL := List.filter (fn x => x <> nID) (!spillWL);
                if (moveRelated nID)
                then freezeWL := nID::(!freezeWL)
                else simplifyWL := nID::(!simplifyWL))
          else ()

      fun simplify () =
        let val nodeID = List.hd (!simplifyWL)
        in
          (*print ("simplifying - " ^ Temp.makeString nodeID ^ "\n");*)
          simplifyWL := List.tl (!simplifyWL);
          selectStack := nodeID::(!selectStack);
          TS.app decrementoutDegreeEffects (adjacents nodeID)
        end

      fun getNodeAlias nID =
        case TM.find ((!alias), nID) of
          SOME n => getNodeAlias(n)
        | NONE   => nID

      fun freezeMoves nID =
        TS.app
          (fn x => let val v = getNodeAlias x
                       val nodeMovesActive = filterMoves (!movesActive) x
                       fun filt x = v <> x
                   in
                     movesActive := DTS.difference ((!movesActive),
                                                     nodeMovesActive);
                     movesFrozen := DTS.union ((!movesFrozen),
                                                nodeMovesActive);
                     if TS.isEmpty (nodeMoves v) andalso
                        (FG.outDegree (gNode v)) < nColors
                     then (freezeWL := List.filter filt (!freezeWL);
                           simplifyWL := v::(!simplifyWL))
                     else ()
                   end)
          (nodeMoves nID)

      fun freeze () =
        let val nodeID = List.hd (!freezeWL)
        in
          (*print ("freeze - " ^ Temp.makeString nodeID ^ "\n");*)
          freezeWL := List.tl (!freezeWL);
          simplifyWL := nodeID::(!simplifyWL);
          freezeMoves nodeID
        end

      fun pickSpill () =
        let
          fun minInterf (x, v) =
            if (FG.outDegree (gNode x) < FG.outDegree (gNode v))
            then x
            else v
          val nodeID = List.foldl minInterf (List.hd (!spillWL))
                                            (List.tl (!spillWL))
          fun fID x = x <> nodeID
        in
          (*print ("spill - " ^ Temp.makeString nodeID ^ "\n");*)
          spillWL := List.filter fID (!spillWL);
          simplifyWL := nodeID::(!simplifyWL);
          freezeMoves nodeID
        end

      fun combine (u, v) =
        let
          (*val _ = print "combining yay\n"*)
          val mvU = Option.valOf (TM.find ((!movesMap), u))
          val mvV = Option.valOf (TM.find ((!movesMap), v))

          val nU = gNode u
          val nV = gNode v

          val adjsV = TS.listItems (adjacents v)
          fun matchU x = x = u
          fun filtrU x = x <> u
          fun matchV x = x = v
          fun filtrV x = x <> v
        in
          if List.exists matchV (!freezeWL)
          then freezeWL := List.filter filtrV (!freezeWL)
          else spillWL := List.filter filtrV (!spillWL);

          coalescedNodes := TS.add((!coalescedNodes), v);
          alias := TM.insert ((!alias), v, u);
          movesMap := TM.insert ((!movesMap), u, TS.union(mvU, mvV));
          enableMoves (adjacents v);
          graph := FG.mergeNodes (!graph) (nU, nV);
          List.app decrementoutDegreeEffects adjsV;

          if not (isPrecolored u) andalso
             (FG.outDegree (gNode u)) >= nColors andalso
             (List.exists matchU (!freezeWL))
          then (freezeWL := List.filter filtrU (!freezeWL);
                spillWL := u::(!spillWL))
          else ()
        end

      fun ok (u, v)=
        (FG.outDegree (gNode u)) < nColors orelse
        (isPrecolored u) orelse
        (FG.isAdjacent ((gNode u), (gNode v)))

      fun addWorkList nID =
        if not (isPrecolored nID) andalso
           not (moveRelated nID) andalso
           ((FG.outDegree (gNode nID)) < nColors)
        then (freezeWL := List.filter (fn x => x <> nID) (!freezeWL);
              simplifyWL := nID::(!simplifyWL))
        else ()

      fun conservative (u, v) =
        (TS.numItems
          (TS.filter
             (fn x => (FG.outDegree (gNode x)) >= nColors)
             (TS.union ((adjacents u), (adjacents v)))))
        < nColors

      fun coalesce () =
        let
          val mv = (List.hd (DTS.listItems (!movesWL)))
          val (u, v) = mv
          val x = getNodeAlias u
          val y = getNodeAlias v

          (*val _ = print ("coalescing : " ^ Temp.makeString x ^ "-" ^ Temp.makeString y ^ "\n");*)
          (*val _ = print ("coalescing (orig): " ^ Temp.makeString u ^ "-" ^ Temp.makeString v ^ "\n");*)
          val (u, v) = if (isPrecolored y)
                       then (y, x)
                       else (x, y)

        in
          movesWL := DTS.delete ((!movesWL), mv);

          if u = v
          then (movesCoalesced := DTS.add ((!movesCoalesced), mv);
                addWorkList u)
          else if (isPrecolored v) orelse FG.isAdjacent ((gNode u), (gNode v))
          then (movesConstrained := DTS.add ((!movesConstrained), mv);
                addWorkList u; addWorkList v)
          else if ((isPrecolored u) andalso
                   (List.all (fn x => ok (x, u)) (TS.listItems (adjacents v))))
                  orelse (conservative (u, v))
          then (movesCoalesced := DTS.add((!movesCoalesced), mv);
                combine(u, v);
                addWorkList u)
          else movesActive := DTS.add ((!movesActive), mv)
        end

      fun assignColors () =
        if not (List.null (!selectStack))
        then (let
                val nID = List.last (!selectStack)
                val actualAdjs = List.map getNodeAlias
                                          (FG.adj (gNode nID))
                val unavColors =
                  TS.addList(
                    TS.empty,
                    List.mapPartial (fn x => TM.find((!colors), x))
                                    actualAdjs);
                val avalColors = TS.difference(allColors, unavColors)
                (*val _ = print ("Getting " ^ Temp.makeString nID ^ "\nunav\n")*)
                (*val _ = TS.app (fn x => print (Temp.makeString x ^ "-")) unavColors*)
                (*val _ = print "\n"*)
              in
                selectStack := List.rev (List.tl (List.rev (!selectStack)));
                if TS.isEmpty avalColors
                then spilledNodes := TS.add((!spilledNodes), nID)
                else colors :=
                       TM.insert(
                         (!colors),
                         nID,
                         (List.hd (TS.listItems avalColors)));
                assignColors ()
              end)
        else ()

      fun printWLs () =
        (print "simplify:\n";
         List.app (fn x => print ((F.makeString x) ^ " - ")) (!simplifyWL);
         print "\nfreeze:\n";
         List.app (fn x => print ((Temp.makeString x) ^ " - ")) (!freezeWL);
         print "\nspill:\n";
         List.app (fn x => print ((Temp.makeString x) ^ " - ")) (!spillWL);
         print "\n"
        )

      fun bodyLoop () =
        ((*printWLs ();*)
        if (List.null (!simplifyWL) andalso
            List.null (!freezeWL) andalso
            List.null (!spillWL) andalso
            DTS.isEmpty (!movesWL))
        then ()
        else if not (List.null (!simplifyWL))
        then (simplify (); bodyLoop ())
        else if not (DTS.isEmpty (!movesWL))
        then (coalesce (); bodyLoop ())
        else if not (List.null (!freezeWL))
        then (freeze (); bodyLoop ())
        else if not (List.null (!spillWL))
        then (pickSpill (); bodyLoop ())
        else ()
        )
    in
      List.app makeWLs (FG.nodes (!graph));
      bodyLoop ();
      assignColors ();
      colors :=
        TS.foldl
          (fn (x, c) =>
             TM.insert(
               c, x,
               Option.valOf (TM.find ((!colors), (getNodeAlias x)))))
          (!colors) (!coalescedNodes);
      ((TM.map (fn k => Option.valOf (F.getTempReg k)) (!colors)),
               TS.listItems (!spilledNodes))
    end
end
