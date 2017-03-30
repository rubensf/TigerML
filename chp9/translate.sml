functor Translate(F: FRAME) : TRANSLATE =
struct
  structure F = F

  structure A = Absyn
  structure C = Canon
  structure T = Tree

  val error = ErrorMsg.error

  datatype level = Level of {parent: level,
                             name: Temp.label,
                             formals: bool list,
                             frame: F.frame} * unit ref
                 | Outermost
  datatype access = Access of level * F.access
                  | NilAccess (* Not sure if necessary *)
  datatype exp = Ex of T.exp
               | Nx of T.stm
               | Cx of Temp.label * Temp.label -> T.stm
  type frag = F.frag

  val outername = Temp.newlabel ()
  val outermost = Level ({parent=Outermost,
                          name=outername,
                          formals=[],
                          frame=F.newFrame {name=outername, formals=[]}},
                         ref ())
  val frags = ref ([] : F.frag list)

  fun newLevel {parent: level, name: Temp.label, formals: bool list} =
    Level ({parent=parent, name=name, formals=formals, frame=F.newFrame {name=name, formals=formals}}, ref ())

  fun formals (lev: level) =
    case lev of
      Level l   => foldl (fn (formal, ans) => (Access (lev, formal))::ans)
                     [] (F.formals (#frame (#1 l)))
    | Outermost => (error 0 "Internal Failure: cannot get formals of outermost level."; [])

  fun frame (lev: level) =
    case lev of
      Level l   => (#frame (#1 l))
    | Outermost => (error 0 "Internal Failure: cannot get frame of outermost level."; F.newFrame {name=outername, formals=[]})

  fun seq (l: T.stm list) =
    case List.length l of
      0 => T.EXP (T.CONST 0)
    | 1 => (hd l)
    | 2 => T.SEQ ((hd l), (hd (tl l)))
    | _ => T.SEQ ((hd l), seq (tl l))

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
        let
          val r = Temp.newtemp()
          val t = Temp.newlabel()
          val f = Temp.newlabel()
        in
          T.ESEQ (seq [T.MOVE(T.TEMP r, T.CONST 1),
                       genstm(t, f),
                       T.LABEL f,
                       T.MOVE(T.TEMP r, T.CONST 0),
                       T.LABEL t],
                  T.TEMP r)
        end
    | unEx (Nx s) = T.ESEQ(s, T.CONST 0)

  fun unCx (Ex (T.CONST 0)) = (fn (t, f) => T.JUMP(T.NAME f, [f]))
    | unCx (Ex (T.CONST _)) = (fn (t, f) => T.JUMP(T.NAME t, [t]))
    | unCx (Ex e) = (fn (t, f) => T.CJUMP(T.NE, T.CONST 0, e, t, f))
    | unCx (Cx genstm) = genstm
    | unCx (Nx _) = (fn (t, f) => (error 0 "Internal Failure."; T.EXP (T.CONST 0))) (* change to error message if possible *)

  fun unNx (Ex e) = T.EXP e
    | unNx (Nx s) = s
    | unNx (Cx genstm) = let val tf = Temp.newlabel() in T.SEQ(genstm(tf,tf), T.LABEL tf) end

  fun convertAopTop (A.PlusOp)   = T.PLUS
    | convertAopTop (A.MinusOp)  = T.MINUS
    | convertAopTop (A.TimesOp)  = T.MUL
    | convertAopTop (A.DivideOp) = T.DIV
    | convertAopTop _            = (error 0 "Internal Failure."; T.PLUS)

  fun convertAopTrelop (A.EqOp)  = T.EQ
    | convertAopTrelop (A.NeqOp) = T.NE
    | convertAopTrelop (A.LtOp)  = T.LT
    | convertAopTrelop (A.LeOp)  = T.LE
    | convertAopTrelop (A.GtOp)  = T.GT
    | convertAopTrelop (A.GeOp)  = T.GE
    | convertAopTrelop _         = (error 0 "Internal Failure."; T.EQ)

  fun intOpExp (oper as (A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp), l, r) =
        Ex (T.BINOP((convertAopTop oper), unEx l, unEx r))
    | intOpExp (oper as (A.EqOp | A.NeqOp | A.LtOp | A.LeOp | A.GtOp | A.GeOp), l, r) =
        Cx (fn (t, f) => T.CJUMP((convertAopTrelop oper), unEx l, unEx r, t, f))

  fun strOpExp (A.EqOp,  l, r) = Ex (F.externCallFn("mystrcmp", [unEx l] @ [unEx r] @ [T.CONST 0]))
    | strOpExp (A.NeqOp, l, r) = Ex (F.externCallFn("mystrcmp", [unEx l] @ [unEx r] @ [T.CONST 1]))
    | strOpExp (A.LtOp,  l, r) = Ex (F.externCallFn("mystrcmp", [unEx l] @ [unEx r] @ [T.CONST 2]))
    | strOpExp (A.LeOp,  l, r) = Ex (F.externCallFn("mystrcmp", [unEx l] @ [unEx r] @ [T.CONST 3]))
    | strOpExp (A.GtOp,  l, r) = Ex (F.externCallFn("mystrcmp", [unEx l] @ [unEx r] @ [T.CONST 4]))
    | strOpExp (A.GeOp,  l, r) = Ex (F.externCallFn("mystrcmp", [unEx l] @ [unEx r] @ [T.CONST 5]))
    | strOpExp _               = (error 0 "Internal Failure."; Ex (T.CONST 0))

  fun intExp i = Ex (T.CONST i)

  fun strExp str =
    let
      val lab = Temp.newlabel ()
    in
      frags := (F.STRING lab) :: !frags;
      Ex (T.NAME lab)
    end

  fun lvEqual (Level(_, uref1), Level(_, uref2)) = uref1 = uref2
    | lvEqual (_,_) = false

  fun staticLinking (Level deflevel, Level curlevel) =
    if lvEqual (Level deflevel, Level curlevel)
    then T.TEMP F.fp
    else T.MEM (T.BINOP (T.PLUS,
                         T.CONST (F.getOffset (#frame (#1 curlevel))),
                         staticLinking (Level deflevel, (#parent (#1 curlevel)))))
    | staticLinking (_,_) = T.CONST 0

  fun allocLocal (lev: level) esc =
    case lev of
      Level l   => Access (lev, F.allocLocal(#frame (#1 l)) esc)
    | Outermost => NilAccess

  fun arrCreation (size, init) =
    let
      val arr = F.externCallFn ("initArray",
                                [T.BINOP(T.PLUS,
                                         (unEx size),
                                          T.CONST 1)] @
                                [unEx init])
      val tmp = T.TEMP (Temp.newtemp ())
      val save = T.MOVE (tmp, arr)
      val savesize = T.MOVE (T.MEM tmp, (unEx size))
    in
      Ex (T.ESEQ (T.SEQ (save, savesize), tmp))
    end
  fun recCreation (args) =
    let
      val argsSize = List.length args

      val r = Temp.newtemp ()
      val alloc = T.MOVE (T.TEMP r, F.externCallFn ("allocRecord", [T.CONST argsSize]))

      val posArgs = foldl (fn (arg, ans) => ans @ [(arg, length ans)]) [] args
      fun pushArg ((arg, off), res) =
        res @ [T.MOVE (T.MEM (T.BINOP (T.PLUS,
                                       T.TEMP r,
                                       T.CONST (off * F.wordSize))),
                      (unEx arg))]
      val argsCmnds = foldl pushArg [] posArgs
      val argsCmndsSeq = seq ([alloc] @ argsCmnds)
    in
      Ex (T.ESEQ (argsCmndsSeq, T.TEMP r))
    end


  fun whileExp (test, body, done) =
    let
      val test      = unCx test
      val testLabel = Temp.newlabel()
      val body      = unNx body
      val bodyLabel = Temp.newlabel()
    in
      Nx (seq [T.LABEL testLabel,
               test (bodyLabel, done),
               T.LABEL bodyLabel,
               body,
               T.JUMP (T.NAME testLabel, [testLabel]),
               T.LABEL done])
    end

  fun forExp (var, break, lo, hi, body) =
    let
      val _         = allocLocal
      val var       = unEx var
      val lo        = unEx lo
      val hi        = unEx hi
      val body      = unNx body
      val bodyLabel = Temp.newlabel()
      val incLabel  = Temp.newlabel()
    in
      Nx (seq [T.MOVE(var, lo),
               T.CJUMP(T.LE, var, hi, bodyLabel, break),
               T.LABEL bodyLabel,
               body,
               T.CJUMP(T.LT, var, hi, incLabel, break),
               T.LABEL incLabel,
               T.MOVE(var, T.BINOP(T.PLUS, var, T.CONST 1)),
               T.JUMP(T.NAME bodyLabel, [bodyLabel]),
               T.LABEL break])
    end

  fun nilExp() = Ex (T.CONST 0)

  fun intExp(i) = Ex (T.CONST i)

  fun seqExp(exps : exp list) =
    case List.length exps of
      0 => Ex (T.CONST 0)
    | 1 => Ex (unEx (hd exps))
    | _ => Ex (T.ESEQ (seq (map unNx (List.take (exps, ((List.length exps) - 1)))), unEx (List.last exps)))

  fun assignExp(var, exp) = Nx (T.MOVE (unEx var, unEx exp))

  fun breakExp(break) = Nx (T.JUMP (T.NAME break, [break]))

  fun letExp([], body) = body
    | letExp(decs, body) =
        let
          val d = seq (map unNx decs)
        in
          Ex (T.ESEQ (d, unEx body))
        end
  fun ifThenElseExp(test, then', else') =
    let
      val tlabel = Temp.newlabel()
      val flabel = Temp.newlabel()
      val done   = Temp.newlabel()
      val r      = Temp.newtemp()
    in
      case (then', else') of
        (Cx _, Cx _) => Cx (fn (t,f) => seq [(unCx test) (tlabel, flabel),
                            T.LABEL tlabel,
                            (unCx then') (t, f),
                            T.LABEL flabel,
                            (unCx else') (t, f)])
      | (Ex _, Ex _) => Ex (T.ESEQ (seq [(unCx test) (tlabel, flabel),
                                          T.LABEL tlabel,
                                          T.MOVE (T.TEMP r, unEx then'),
                                          T.JUMP (T.NAME done, [done]),
                                          T.LABEL flabel,
                                          T.MOVE (T.TEMP r, unEx else'),
                                          T.LABEL done], T.TEMP r))
      | (Nx _, Nx _) => Nx (seq [(unCx test) (tlabel, flabel),
                                  T.LABEL tlabel,
                                  unNx then',
                                  T.JUMP (T.NAME done, [done]),
                                  T.LABEL flabel,
                                  unNx else',
                                  T.LABEL done])
      | (_, _)       => Ex (T.CONST 0) (*should never get here*)
    end
  fun ifThenExp(test, then') =
    let
      val done = Temp.newlabel()
      val tlabel = Temp.newlabel()
      val r = Temp.newtemp()
    in
      case then' of
        (Cx _) => Cx (fn (t, f) => seq [(unCx test) (tlabel, done),
                                        T.LABEL tlabel,
                                        (unCx then') (t, f),
                                        T.LABEL done])
      | (Ex _) => Ex (T.ESEQ (seq [(unCx test) (tlabel, done),
                                    T.LABEL tlabel,
                                    T.MOVE (T.TEMP r, unEx then'),
                                    T.LABEL done], T.TEMP r))
      | (Nx _) => Nx (seq [(unCx test) (tlabel, done),
                            T.LABEL tlabel,
                            unNx then',
                            T.LABEL done])
    end
  fun callExp(deflevel as Level ({parent=parent,...}, uniqref), curlevel, label, exps) =
    let
      val link = staticLinking (parent, curlevel)
      val expCalls = map unEx exps
    in
      Nx (T.MOVE ((T.TEMP F.rv), (T.CALL (T.NAME label, link::expCalls))))
    end
    | callExp _ = (error 0 "Top Level Exception"; Ex (T.CONST 0))
  fun packExps(exps, mainexp) = Ex (T.ESEQ ((seq (map unNx exps)), (unEx mainexp)))

  fun simpleVarAccess (Access ac, Level l) = Ex (F.expFn (#2 ac) (staticLinking ((#1 ac), Level l)))
    | simpleVarAccess (_, _) = (error 0 "Internal Failure."; Ex (T.CONST 0))

  fun arrayVarAccess (var, subscr) =
      let
        val var = unEx var
      in
        ifThenElseExp(
          Ex (T.BINOP(T.AND,
                      unEx (intOpExp(A.GeOp, subscr, Ex (T.CONST 0))),
                      unEx (intOpExp(A.LeOp, subscr, Ex (T.MEM var))))),
          Ex (T.MEM (T.BINOP (T.PLUS,
                              var,
                              T.BINOP (T.MUL,
                                       T.BINOP (T.PLUS, unEx subscr, T.CONST 1),
                                       T.CONST (F.wordSize))))),
          Ex (T.ESEQ (T.ERROR (T.OUTOFBOUNDS), T.CONST 0)))
      end

  fun fieldVarAccess(v, off) =
      let
        val v = unEx v
      in
        ifThenElseExp(
          intOpExp(A.EqOp,
                   Ex (T.CONST 0),
                   Ex (v)),
          Ex (T.ESEQ (T.ERROR (T.NILDEREFERENCE), T.CONST 0)),
          Ex (T.MEM (T.BINOP (T.PLUS,
                              v,
                              T.BINOP (T.MUL,
                                       unEx off,
                                       T.CONST F.wordSize)))))
      end

  fun procEntryExit({level = level, body = body}) =
    let
      val frame' = case level of
        Outermost => (error 0 "Top Level Exception";
                      F.newFrame {name = Temp.newlabel(), formals = []})
        | Level l => (#frame (#1 l))
      val body' = unNx body
      val frag' = F.PROC({body = body', frame = frame'})
    in
      frags := frag'::(!frags)
    end

  fun resetFrags () = (F.resetFrame (frame outermost); frags := [])
  fun getResult () = !frags

  fun errExp () = Ex (T.CONST 0)

  fun printFrag (F.PROC {body, frame}) =
        (print "New body:\n";
         List.app (fn s => Printtree.printtree (TextIO.stdOut, s))
                   (C.traceSchedule (C.basicBlocks (C.linearize body))))
      | printFrag (F.STRING lbl) = print ((Temp.labelToString lbl) ^ "\n")
end
