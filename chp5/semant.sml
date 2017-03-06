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

  val nest = ref 0

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

  fun checkTypeMatch({exp1, ty1}, {exp2, ty2}, pos) =
      case ty1 = ty2 of
        true => true
      | false => (error pos "Type mismatch."; false)

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
            {exp=R.nil(), ty=T.UNIT}
        | trexp (A.AssignExp{var, exp, pos}) =
            let
              val var' = transVar(venv, tenv, var)
              val exp' = trexp exp
            in
              checkTypeMatch(var', exp', pos);
              {exp=R.nil(), ty=T.UNIT}
            end
        | trexp (A.IfExp {test, then', else', pos}) =
            (case else' of 
              SOME else' =>
                let
                  val test' = trexp test
                  val then'' = trexp then'
                  val else'' = trexp else'
                  val {exp=if_exp, ty=if_ty} = then''
                in
                  checkInt(test');
                  checkTypeMatch(then'', else'', pos);
                  {exp = T.nil(), ty = if_ty}
                end
              | NONE =>
                checkInt(trexp(test), pos);
                checkUnit(trexp(then'), pos);
                {exp = T.nil(), ty = T.UNIT}
            )
        | trexp (A.WhileExp{test, body, pos}) =
            checkInt(trexp(test), pos);
            checkUnit(trexp(body), pos);
            {exp = T.nil(), ty = T.UNIT}
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
            let
              val venv' = S.enter(venv, var, E.VarEntry{ty=T.INT})
            in
              checkInt(trexp(lo), pos);
              checkInt(trexp(hi), pos);
              checkUnit(transExp(venv', tenv, body), pos);
              {exp = T.nil(), ty = T.UNIT}
            end
        | trexp (A.BreakExp pos) = 
            case nest > 0 of
              true => {exp = T.nil(), ty = T.UNIT}
            | false=> 
                (error pos "Break must be inside a loop";
                {exp = T.nil(), ty = T.UNIT})
        | trexp (A.LetExp {decs, body, pos}) = 
            let 
              val {venv=venv',tenv=tenv'} = transDecs(venv,tenv,decs)
            in 
              transExp(venv',tenv') body
            end
        | trexp (A.ArrayExp{typ, size, init, pos}) =
            {exp = T.nil(), ty = T.UNIT}
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

    and transDec (venv, tenv) = 
    and transDecs (venv, tenv, decs) =

    fun transProg(absyn) = (transExp(E.base_venv, E.base_tenv) absyn; ())

end