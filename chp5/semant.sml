(* Dummy Translate*)
structure Translate =
struct
  type exp = unit
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
      case ty of T.INT => ()
                   | _ => error pos "Integer required"

  fun checkString({exp, ty}, pos) =
      case ty of T.STRING => ()
                      | _ => error pos "String required"

  fun checkUnit({exp, ty}, pos) =
      case ty of T.UNIT => ()
                    | _ => error pos "Unit required"

  fun transExp(venv, tenv, exp) =
    let
      fun trexp (A.VarExp) =
        | trexp (A.NilExp var) = {exp=R.exp, ty=T.NIL}
        | trexp (A.IntExp i) = {exp=R.exp, ty=T.INT}
        | trexp (A.StringExp (str,pos)) = {exp=R.exp, ty= T.STRING}
        | trexp (A.CallExp{func,args,pos}) =
        | trexp (A.OpExp{left,oper,right,pos})) =
          case oper of
            (A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp |
             A.LtOp | A.LeOp | A.GtOp | A.GeOp) =>
              (checkInt(trexp left, pos); checkInt(trexp right, pos);
               {exp=R.exp,ty=T.INT})
          | (A.EqOp | A.NeqOp) => (* Strings, Ints, Arrays, Records *)

        | trexp (A.RecordExp{fields,typ,pos}) =
        | trexp (A.SeqExp exps) =
        | trexp (A.AssignExp{var,exp,pos}) =
        | trexp (A.IfExp{test,then',else',pos}) =
        | trexp (A.WhileExp{test,body,pos}) =
        | trexp (A.ForExp{var,escape,lo,hi,body,pos}) =
        | trexp (A.BreakExp pos) = {exp=R.exp, ty=T.UNIT}
        | trexp (A.LetExp {decs,body,pos}) = 
          let val {venv=venv', tenv=tenv'} = transDecs(venv,tenv,decs)
          in transExp(venv',tenv') body
          end
        | trexp (A.ArrayExp{typ,size,init,pos}) =
      and trvar (A.SimpleVar(id,pos)) = (case Symbol.look(venv,id) of
            SOME(E.VarEntry(ty)) => {exp=(),ty=actual_ty ty}
                          | NONE => (error pos ("undefined variable: " ^ Symbol.name id);
                                     {exp=R.nilExp(), ty=T.INT}))
        | trvar (A.FieldVar(var,id,pos)) = (case (trvar var) of
            {exp, ty=record as T.RECORD (fields, _)} => {exp=R.nilExp(), ty=record}
            | _ => (err pos "no such var"; {exp=R.nilExp(), ty=T.UNIT}))
        | trvar (A.SubscriptVar(var, exp, pos)) =
            (checkInt(trexp exp, pos);
             {exp=R.nilExp(), ty=T.UNIT})

      in
          trexp
      end

    and transDec (venv, tenv) = 
    and transDecs (venv, tenv, decs) =

    fun transProg(absyn) = (transExp(E.base_venv, E.base_tenv) absyn; ())

end