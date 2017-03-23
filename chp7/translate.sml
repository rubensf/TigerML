structure Translate :> TRANSLATE =
struct
  structure F = MipsFrame
  structure T = Tree

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
    | Outermost => []

  fun allocLocal (lev: level) esc =
    case lev of
      Level l   => Access (lev, F.allocLocal(#frame (#1 l)) esc)
    | Outermost => NilAccess

  fun lvEqual (Level(_, uref1), Level(_, uref2)) = uref1 = uref2
    | lvEqual (_,_) = false

  fun staticLinking (Level deflevel, Level curlevel) =
    if lvEqual (Level deflevel, Level curlevel)
    then T.TEMP F.fp
    else T.MEM (T.BINOP (T.PLUS,
                         T.CONST (F.getOffset (#frame (#1 curlevel))),
                         staticLinking (Level deflevel, (#parent (#1 curlevel)))))
    | staticLinking (_,_) = T.CONST 0

  fun simpleVar (Access ac, Level l) = Ex (F.expfn (#2 ac) (staticLinking ((#1 ac), Level l)))
    | simpleVar (_, _) = Ex (T.CONST 0) (* TODO How to report internal error? *)

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

  fun nilExp() = Ex (T.CONST 0)
end
