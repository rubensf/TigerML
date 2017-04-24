signature COLOR =
sig
  type allocation
  type registerlist
  type node

  val color: {
    igraph: Liveness.igraph,
    spillCost: node -> int,
    registers: registerlist
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
  structure DLL = DoubleLinkedList

  val gnID = FG.getNodeID

  structure DoubleTempNodeOrdKey =
    struct type ord_key = (Temp.temp FG.node * Temp.temp FG.node)
           val compare = (fn ((v11, v12), (v21, v22)) =>
                            let
                              val c1 = Temp.compare (gnID v11,
                                                     gnID v21)
                              val c2 = Temp.compare (gnID v12,
                                                     gnID v22)
                            in
                              case (c1, c2) of
                                (LESS, _)      => LESS
                              | (EQUAL, LESS)  => LESS
                              | (EQUAL, EQUAL) => EQUAL
                              | _              => GREATER
                            end)
    end

  structure TempNodeOrdKey =
    struct type ord_key = Temp.temp FG.node
           val compare = (fn (v1, v2) =>
                            Temp.compare (gnID v1, gnID v2))
    end

  structure TempNodeMap = SplayMapFn(TempNodeOrdKey)
  structure TempNodeSet = SplaySetFn(TempNodeOrdKey)
  structure DoubleTempNodeSet = SplaySetFn(DoubleTempNodeOrdKey)

  (* Shortcuts *)
  structure TNM = TempNodeMap
  structure TNS = TempNodeSet
  structure DTNS = DoubleTempNodeSet

  fun reorderNodes (v1, v2) =
    case Temp.compare (gnID v1, gnID v2) of
      LESS    => (v1, v2)
    | EQUAL   => (v1, v2)
    | GREATER => (v2, v1)

  type allocation = F.register Temp.Map.map
  type registerlist = F.register list
  type node = Temp.temp Liveness.FG.node



  fun color {igraph as Liveness.IGRAPH {graph, moves}, spillCost, registers} =
    let
      val avalRegs = F.allRegisters

      val nColors = (List.length avalRegs)

      val precoloredNodes = List.foldl
                              (fn (x, m) =>
                                 (Temp.Map.insert (m, (F.getRegTemp x), x)))
                              Temp.Map.empty avalRegs

      val allColors = Temp.Set.addList(
                        Temp.Set.empty,
                        List.map (fn (k, v) => k)
                                 (Temp.Map.listItemsi precoloredNodes))

      fun addToMap (mMap, v1, v2) =
        let val assocMove = TNM.find (mMap, v1)
        in case assocMove of
             SOME nodeSet => TNM.insert(mMap, v1, TNS.add(nodeSet, v2))
           | NONE         => TNM.insert(mMap, v1, TNS.add(TNS.empty, v2))
        end

      val (movesSet, movesMap) =
        List.foldl (
          fn ((v1, v2), (mSet, mMap)) =>
            let
              val mMap'  = addToMap(mMap , v1, v2)
              val mMap'' = addToMap(mMap', v2, v1)
              val mSet'  = DTNS.add (mSet, (reorderNodes (v1, v2)))
            in
              (mSet', mMap'')
            end)
          (DTNS.empty, TNM.empty) moves

      fun isPrecolored n =
        case F.getTempReg (FG.nodeInfo n) of
          SOME(r) => true
        | _       => false

      val graph = ref graph
      fun gNode nID = FG.getNode ((!graph), nID)

      val movesMap = ref movesMap

      val spillWL = ref DLL.empty
      val freezeWL = ref DLL.empty
      val simplifyWL = ref DLL.empty

      val movesWL = ref movesSet
      val movesActive = ref DTNS.empty
      val movesFrozen = ref DTNS.empty
      val movesConstrained = ref DTNS.empty
      val movesCoalesced = ref DTNS.empty

      val selectStack = ref []
      val coalescedNodes = ref TNS.empty
      val spilledNodes = ref TNS.empty

      val alias = ref TNM.empty
      val colors = ref (List.foldl
                               (fn (x, m) =>
                                  (Temp.Map.insert (m, (F.getRegTemp x),
                                                       (F.getRegTemp x))))
                               Temp.Map.empty avalRegs)

      fun adjacents node =
        TNS.difference(TNS.addList(TNS.empty, FG.adj' (!graph) node),
                       TNS.union(TNS.addList(TNS.empty, (!selectStack)),
                                 (!coalescedNodes)))

      fun adjacents' nodeID =
        adjacents (gNode nodeID)

      fun filterMoves movesDTNS nID =
        DTNS.filter
          (fn (v1, v2) =>
             ((gnID v1) = nID orelse (gnID v2) = nID))
          movesDTNS

      fun toFilteredMove movesDTNS nID =
        TNS.addList
          (TNS.empty,
           List.map
             (fn (v1, v2) => if (gnID v1) = nID
                             then v2
                             else v1)
             (DTNS.listItems (filterMoves movesDTNS nID)))

      fun nodeMoves node =
        let
          val nID = gnID node
          val mvs = TNM.find ((!movesMap), node)
          val mvsWL = toFilteredMove (!movesWL) nID
          val mvsAt = toFilteredMove (!movesActive) nID
          val mvsUnion = TNS.union (mvsWL, mvsAt)
        in
          case mvs of
            SOME m => TNS.intersection (m, mvsUnion)
          | NONE   => TNS.empty
        end

      fun moveRelated node =
        not (TNS.isEmpty (nodeMoves node))

      fun makeWLs node =
        if (isPrecolored node)
        then ()
        else if FG.outDegree (node) >= nColors
        then spillWL := DLL.addLast ((!spillWL), (gnID node))
        else if moveRelated node
        then freezeWL := DLL.addLast ((!freezeWL), (gnID node))
        else simplifyWL := DLL.addLast ((!simplifyWL), (gnID node))

      fun enableMoves nodes =
        TNS.app
          (fn n =>
             TNS.app
               (fn x =>
                  let
                    val nodeActiveMoves = filterMoves (!movesActive) (gnID n)
                  in
                    if (not (DTNS.isEmpty nodeActiveMoves))
                    then (movesActive := DTNS.difference ((!movesActive),
                                                          nodeActiveMoves);
                          movesWL := DTNS.union ((!movesWL), nodeActiveMoves))
                    else ()
                  end)
               (nodeMoves n))
          nodes

      fun decrementoutDegreeEffects node =
          if FG.outDegree node = nColors
          then (enableMoves (TNS.add ((adjacents node), node));
                spillWL := DLL.remove (!spillWL) (gnID node);
                if (moveRelated node)
                then freezeWL := DLL.addLast ((!freezeWL), (gnID node))
                else simplifyWL := DLL.addLast ((!simplifyWL), (gnID node)))
          else ()

      fun simplify () =
        let val nodeID = DLL.peekFirst (!simplifyWL)
        in
          simplifyWL := DLL.popFirst (!simplifyWL);
          selectStack := (gNode nodeID)::(!selectStack);
          TNS.app decrementoutDegreeEffects (adjacents' nodeID)
        end

      fun getNodeAlias node =
        case TNM.find ((!alias), node) of
          SOME n => getNodeAlias(n)
        | NONE   => node

      fun freezeMoves node =
        TNS.app
          (fn x => let val v = getNodeAlias x
                       val nodeMovesActive = filterMoves (!movesActive) (gnID x)
                   in
                     movesActive := DTNS.difference ((!movesActive),
                                                     nodeMovesActive);
                     movesFrozen := DTNS.union ((!movesFrozen),
                                                nodeMovesActive);
                     if TNS.isEmpty (nodeMoves v) andalso
                        (FG.outDegree v) < nColors
                     then (freezeWL := DLL.remove (!freezeWL) (gnID v);
                           simplifyWL := DLL.addLast ((!simplifyWL), (gnID v)))
                     else ()
                   end)
          (nodeMoves node)

      fun freeze () =
        let val nodeID = DLL.peekFirst (!freezeWL)
        in
          freezeWL := DLL.popFirst (!freezeWL);
          simplifyWL := DLL.addLast ((!simplifyWL), nodeID);
          freezeMoves (gNode nodeID)
        end

      fun pickSpill () =
        let val nodeID = DLL.peekFirst (!spillWL)
        in
          spillWL := DLL.popFirst (!spillWL);
          simplifyWL := DLL.addLast ((!simplifyWL), nodeID);
          freezeMoves (gNode nodeID)
        end

      fun combine (u, v) =
        let
          val mvU = Option.valOf (TNM.find ((!movesMap), u))
          val mvV = Option.valOf (TNM.find ((!movesMap), v))

          val adjsV = List.map (fn x => (gnID x))
                               (TNS.listItems (adjacents v))
        in
          if DLL.has (!freezeWL) (gnID v)
          then freezeWL := DLL.remove (!freezeWL) (gnID v)
          else simplifyWL := DLL.remove (!simplifyWL) (gnID v);
          coalescedNodes := TNS.add((!coalescedNodes), v);
          alias := TNM.insert ((!alias), v, u);
          movesMap := TNM.insert ((!movesMap), u, TNS.union(mvU, mvV));

          graph := FG.mergeNodes (!graph) (u, v);
          List.app decrementoutDegreeEffects
                   (List.map gNode adjsV);

          if (FG.outDegree u) >= nColors andalso
             (DLL.has (!freezeWL) (gnID u))
          then (freezeWL := DLL.remove (!freezeWL) (gnID u);
                spillWL := DLL.addLast ((!simplifyWL), (gnID u)))
          else ()
        end

      fun ok (u, v)=
        (FG.outDegree u) < nColors orelse
        (isPrecolored u) orelse
        (FG.isAdjacent (u, v))

      fun addWorkList node =
        if not (isPrecolored node) andalso
           not (moveRelated node) andalso
           ((FG.outDegree node) < nColors)
        then (freezeWL := DLL.remove (!freezeWL) (gnID node);
              simplifyWL := DLL.addLast((!simplifyWL), (gnID node)))
        else ()

      fun briggs (u, v) =
        (TNS.numItems (TNS.union((adjacents u),
                                 (adjacents v))))
        < nColors

      fun george (u, v) =
        (TNS.numItems
          (TNS.filter
             (fn x => (FG.outDegree x) >= nColors)
             (TNS.union ((adjacents u), (adjacents v)))))
        < nColors

      fun coalesce () =
        let
          val mv = (List.hd (DTNS.listItems (!movesWL)))
          val (u, v) = mv
          val x = getNodeAlias u
          val y = getNodeAlias v

          val (u, v) = if (isPrecolored y)
                       then (y, x)
                       else (x, y)

        in
          movesWL := DTNS.delete ((!movesWL), mv);

          if (gnID u) = (gnID v)
          then (movesCoalesced := DTNS.add ((!movesCoalesced), mv);
                addWorkList u)
          else if (isPrecolored v) orelse FG.isAdjacent (u, v)
          then (movesConstrained := DTNS.add ((!movesConstrained), mv);
                addWorkList u; addWorkList v)
          else if ((isPrecolored u) andalso
                   (List.all (fn x => ok (x, u)) (TNS.listItems (adjacents v))))
                  orelse (george (u, v))
          then (movesCoalesced := DTNS.add((!movesCoalesced), mv);
                combine(u, v);
                addWorkList u)
          else movesActive := DTNS.add ((!movesActive), mv)
        end

      fun assignColors () =
        if not (List.null (!selectStack))
        then (let
                val node = List.last (!selectStack)
                val actualAdjs = List.map getNodeAlias (FG.adj' (!graph) node)
                val actualAdjs = List.map gnID actualAdjs
                val unavColors =
                  Temp.Set.addList(
                    Temp.Set.empty,
                    List.mapPartial (fn x => Temp.Map.find((!colors), x))
                                    actualAdjs);
                val avalColors = Temp.Set.difference(allColors, unavColors)
              in
                selectStack := List.rev (List.tl (List.rev (!selectStack)));
                if Temp.Set.isEmpty avalColors
                then spilledNodes := TNS.add((!spilledNodes), node)
                else colors :=
                       Temp.Map.insert(
                         (!colors),
                         (gnID node),
                         (List.hd (Temp.Set.listItems avalColors)));
                assignColors ()
              end)
        else ()

      fun bodyLoop () =
        if (DLL.isEmpty (!simplifyWL) andalso
            DLL.isEmpty (!freezeWL) andalso
            DLL.isEmpty (!spillWL) andalso
            DTNS.isEmpty (!movesWL))
        then ()
        else if not (DLL.isEmpty (!simplifyWL))
        then (simplify (); bodyLoop ())
        else if not (DTNS.isEmpty (!movesWL))
        then (coalesce (); bodyLoop ())
        else if not (DLL.isEmpty (!freezeWL))
        then (freeze (); bodyLoop ())
        else if not (DLL.isEmpty (!spillWL))
        then (pickSpill (); bodyLoop ())
        else ()
    in
      List.app makeWLs (FG.nodes (!graph));
      bodyLoop ();
      assignColors ();
      colors :=
        TNS.foldl
          (fn (x, c) =>
             Temp.Map.insert(
               c, gnID x,
               Option.valOf (Temp.Map.find ((!colors), (gnID (getNodeAlias x))))))
          (!colors) (!coalescedNodes);
      ((Temp.Map.map (fn k => Option.valOf (F.getTempReg k)) (!colors)),
       (List.map gnID (TNS.listItems (!spilledNodes))))
    end
end
