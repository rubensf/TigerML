structure Translate :> TRANSLATE =
struct
  structure A = Absyn
  structure F = MipsFrame
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

  val outername = Temp.newlabel ()
  val outermost = Level ({parent=Outermost,
                          name=outername,
                          formals=[],
                          frame=F.newFrame {name=outername, formals=[]}},
                         ref ())

  fun newLevel {parent: level, name: Temp.label, formals: bool list} =
    Level ({parent=parent, name=name, formals=formals, frame=F.newFrame {name=name, formals=formals}}, ref ())

  fun formals (lev: level) =
    case lev of
      Level l   => foldl (fn (formal, ans) => (Access (lev, formal))::ans)
                     [] (F.formals (#frame (#1 l)))
    | Outermost => (error 0 "Internal Failure: cannot get formals of outermost level."; [])

  fun allocLocal (lev: level) esc =
    case lev of
      Level l   => Access (lev, F.allocLocal(#frame (#1 l)) esc)
    | Outermost => NilAccess

  fun seq (l: T.stm list) =
    case List.length l of
      0 => T.EXP (T.CONST 0)
    | 1 => hd l
    | 2 => T.SEQ((hd l), (hd (tl l)))
    | _ => T.SEQ((hd l), seq (tl l))

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
    | unCx (Nx _) = (fn (t, f) => T.EXP(T.CONST 0)) (* change to error message if possible *)
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

(*  fun relopExp (oper, left, right) =
    Cx(fn(t, f) => T.CJUMP(oper, unEx left , unEx right , t, f))
*)
(*  fun relopStrExp (oper, left, right, str) =
    Ex (F.externCallFn (str, unEx left, unEx right))
*)
  fun lvEqual (Level(_, uref1), Level(_, uref2)) = uref1 = uref2
    | lvEqual (_,_) = false

  fun staticLinking (Level deflevel, Level curlevel) =
    if lvEqual (Level deflevel, Level curlevel)
    then T.TEMP F.fp
    else T.MEM (T.BINOP (T.PLUS,
                         T.CONST (F.getOffset (#frame (#1 curlevel))),
                         staticLinking (Level deflevel, (#parent (#1 curlevel)))))
    | staticLinking (_,_) = T.CONST 0

  fun simpleVarAccess (Access ac, Level l) = Ex (F.expFn (#2 ac) (staticLinking ((#1 ac), Level l)))
    | simpleVarAccess (_, _) = (error 0 "Internal Failure."; Ex (T.CONST 0))

  fun arrayVarAccess (var, subscr) =
    let
       val varEx = unEx var
       val subscrEx = unEx subscr
     in
       Ex (T.MEM (T.BINOP (T.PLUS,
                           T.MEM (varEx),
                           T.BINOP (T.MUL,
                                    subscrEx,
                                    T.CONST (F.wordSize)))))
     end

  fun whileExp (test, body, done) =
    let
      val test      = unCx test
      val testLabel = Temp.newlabel()
      val body      = unNx body
      val bodyLabel = Temp.newlabel()
    in
      Nx (seq[T.LABEL testLabel,
              test (bodyLabel, done),
              T.LABEL bodyLabel,
              body,
              T.JUMP (T.NAME testLabel, [testLabel]),
              T.LABEL done])
    end

  fun forExp (var, escape, lo, hi, body, break) =
    let
      val var       = unEx var
      val lo        = unEx lo
      val hi        = unEx hi
      val body      = unNx body
      val bodyLabel = Temp.newlabel()
      val forLabel  = Temp.newlabel()
    in
      Nx (seq[T.MOVE(var, lo),
              T.CJUMP(T.LE, var, hi, bodyLabel, break),
              T.LABEL(bodyLabel),
              body,
              T.CJUMP(T.LE, var, hi, forLabel, break),
              T.LABEL forLabel,
              T.MOVE(var, T.BINOP(T.PLUS, var, T.CONST 1)),
              T.JUMP(T.NAME bodyLabel, [bodyLabel]),
              T.LABEL break])
    end
  fun nilExp() = Ex (T.CONST 0)
  fun intExp(i) = Ex (T.CONST i)
  fun seqExp([]) = Ex (T.CONST 0)
    | seqExp([exp]) = exp
    | seqExp(exp::more) = Ex (T.ESEQ (unNx exp, unEx (seqExp more)))
  fun assignExp(var, exp) = Nx (T.MOVE (unEx var, unEx exp))
  fun breakExp(break) = Nx (T.JUMP (T.NAME break, [break]))
  fun letExp([], body) = body
    | letExp(decs, body) =
        let
          val d = seq (map unNx decs)
        in
          Ex (T.ESEQ (d, unEx body))
        end
  fun ifExp(test, then', else') =
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
end
