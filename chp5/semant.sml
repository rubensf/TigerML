(* Dummy Translate*)
structure Translate =
struct
  type exp = unit
  fun nil() = ()
end

signature SEMANT =
sig
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  type expty = {exp: Translate.exp, ty: Types.ty}

  val transProg : Absyn.exp -> unit
  val transVar  : venv * tenv * Absyn.var -> expty
  val transExp  : venv * tenv * Absyn.exp -> expty
  val transDec  : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transTy   :        tenv * Absyn.ty  -> Types.ty
end

structure Semant =
struct
  structure A = Absyn
  structure E = Env
  structure P = PrintAbsyn
  structure R = Translate
  structure S = Symbol
  structure T = Types

  val error = ErrorMsg.error

  fun checkInt({exp, ty}, pos) =
      case ty of
        T.INT => ()
      | _ => error pos "Integer required."

  fun checkString({exp, ty}, pos) =
      case ty of
        T.String => ()
      | _ => error pos "String required."

  fun checkUnit({exp, ty}, pos) =
      case ty of
        T.UNIT => ()
      | _ => error pos "Unit required."

  fun transExp(venv, tenv, exp) =
    let
      fun trexp (A.VarExp var) =
            transVar(venv, tenv, var)
        | trexp (A.NilExp) =
            {exp=R.nil(), ty=T.NIL}
        | trexp (A.IntExp i) =
            {exp=R.nil(), ty=T.INT}
        | trexp (A.StringExp (str,pos)) =
            {exp=R.nil(), ty=T.STRING}
        | trexp (A.CallExp {func, args, pos}) =

        | trexp (A.OpExp {left, oper, right, pos})) =
          case oper of
            (A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp |
             A.LtOp | A.LeOp | A.GtOp | A.GeOp) =>
              (checkInt(trexp left, pos); checkInt(trexp right, pos);
               {exp=R.exp,ty=T.INT})
          | (A.EqOp | A.NeqOp) => (* Strings, Ints, Arrays, Records *)
        | trexp (A.RecordExp{fields, typ, pos}) =
        | trexp (A.SeqExp exps) = 
        | trexp (A.AssignExp{var, exp, pos}) =
        | trexp (A.IfExp{test, then', else', pos}) =
            (case else' of 
              SOME else' =>
                checkInt(trexp(test), pos);
                checkUnit(trexp(then'), pos);
                checkUnit(trexp(else'), pos);
                {exp = T.nil(), ty = T.UNIT}
              | NONE =>
            )
        | trexp (A.WhileExp{test, body, pos}) =
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
        | trexp (A.BreakExp pos) = {exp=R.exp, ty = T.UNIT}
        | trexp (A.LetExp {decs, body, pos}) = 
            let val {venv=venv',tenv=tenv'} =
              transDecs(venv,tenv,decs)
            in transExp(venv',tenv') body
            end
        | trexp (A.ArrayExp{typ, size, init, pos}) =
      in
          trexp exp
      end
    and transVar(venv, tenv, var) =
      let
        fun trvar (A.SimpleVar (id, pos)) =
            case Symbol.look (venv, id) of
              SOME (E.VarEntry evrty) => {exp=R.exp, ty=#ty evrty}
            | SOME (E.FunEntry _)     => (error pos (Symbol.name id ^ " is function, not a variable.");
                                          {exp=R.nil(), ty=T.NIL})
            | _                       => (error pos ("Undefined variable: " ^ Symbol.name id);
                                          {exp=R.nil(), ty=T.NIL})
          | trvar (A.FieldVar(var, id, pos)) =
            case (trvar var) of
              {exp, ty=record as T.RECORD (fields, _)} => {exp=R.nil(), ty=record}
            | _ => (err pos "no such var"; {exp=R.nil(), ty=T.UNIT})
          | trvar (A.SubscriptVar(var, exp, pos)) =
            (checkInt(trexp exp, pos);
             {exp=R.nil(), ty=T.UNIT})
      in
        trvar var
      end

    and transDec (venv, tenv, dec) = 
      let
        fun trdec (A.VarDec{name,escape,typ=NONE,init,pos}) =
          let val {exp,ty} = transExp(venv,tenv,init)
          in {tenv=tenv,
              venv=S.enter(venv,name,E.VarEntry{ty=ty})}
          end
          | trdec (A.TypeDec[{name,ty}]) = 
              {venv=venv,
               tenv=S.enter(tenv,name,transTy(tenv,ty))}
      in
        trdec dec
      end

    and transDecs (venv, tenv, decs) =

    fun transProg(absyn) = (transExp(E.base_venv, E.base_tenv) absyn; ())

end