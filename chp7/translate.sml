structure Translate :> TRANSLATE =
struct
  structure F = MipsFrame
  structure T = Tree

  datatype level = Level of {parent: level,
                             name: Temp.label,
                             formals: bool list,
                             frame: F.frame}
                 | Outermost
  datatype access = Access of level * F.access
                  | NilAccess (* Not sure if necessary *)
  datatype exp = Ex of T.exp
               | Nx of T.stm
               | Cx of Temp.label * Temp.label -> T.stm

  val outername = Temp.newlabel ()
  val outermost = Level {parent=Outermost,
                         name=outername,
                         formals=[],
                         frame=F.newFrame {name=outername, formals=[]}}

  fun newLevel {parent: level, name: Temp.label, formals: bool list} =
    Level {parent=parent, name=name, formals=formals, frame=F.newFrame {name=name, formals=formals}}

  fun formals (lev: level) =
    case lev of
      Level l   => foldl (fn (formal, ans) => (Access (lev, formal))::ans)
                     [] (F.formals (#frame l))
    | Outermost => []

  fun allocLocal (lev: level) esc =
    case lev of
      Level l   => Access (lev, F.allocLocal(#frame l) esc)
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

  type exp = unit
  fun nilExp() = ()
end
