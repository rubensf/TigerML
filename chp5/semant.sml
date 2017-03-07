(* Dummy Translate*)
structure Translate =
struct
  type exp = unit
  fun nilExp() = ()
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

structure Semant :> SEMANT =
struct
  structure A = Absyn
  structure E = Env
  structure P = PrintAbsyn
  structure R = Translate
  structure S = Symbol
  structure T = Types

  type venv = E.enventry S.table
  type tenv = T.ty S.table

  type expty = {exp: R.exp, ty: T.ty}

  val error = ErrorMsg.error
  val nest = ref 0

  fun actual_ty (pos, tenv, ty) =
    case ty of
        T.NAME (s, tyopref) => case (!tyopref) of
                                 SOME (ty') => actual_ty(pos, tenv, ty')
                               | NONE       => T.UNIT
      | _                   => ty

  fun checkInt({exp, ty}, pos) =
      case ty of
        T.INT => ()
      | _     => error pos "Integer required."

  fun checkString({exp, ty}, pos) =
      case ty of
        T.STRING => ()
      | _        => error pos "String required."

  fun checkUnit({exp, ty}, pos) =
      case ty of
        T.UNIT => ()
      | _      => error pos "Unit required."

  fun checkTypeMatch(v1: expty, v2: expty, pos) =
      case (#ty v1) = (#ty v2) of
        true  => true
      | false => (error pos "Type mismatch."; false)

  fun transExp(venv, tenv, exp) =
    let
      fun trexp (A.VarExp var) =
            transVar(venv, tenv, var)
        | trexp (A.NilExp) =
            {exp=R.nilExp(), ty=T.NIL}
        | trexp (A.IntExp i) =
            {exp=R.nilExp(), ty=T.INT}
        | trexp (A.StringExp (str,pos)) =
            {exp=R.nilExp(), ty=T.STRING}
        | trexp (A.CallExp {func, args, pos}) =
            (case S.look(venv, func) of
               SOME (E.FunEntry {formals, result}) =>
                 case (length formals) = (length args) of
                   true  => {exp=R.nilExp(), ty=result}
                 | false =>
                     (error pos ("Arguments mismatch"); {exp=R.nilExp(), ty=T.UNIT})
               | SOME (E.VarEntry {ty}) => (error pos ("Function expected, but variable found");
                       {exp=R.nilExp(), ty=T.UNIT})
               | NONE => (error pos ("Function " ^ S.name func ^ " does not exist.");
                          {exp=R.nilExp(), ty=T.UNIT})
               | _ => (error pos ("Function "^ S.name func ^ " could not be translated");
                       {exp=R.nilExp(), ty=T.UNIT}))
        | trexp (A.OpExp {left, oper, right, pos}) =
            (case oper of
               (A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp |
                A.LtOp | A.LeOp | A.GtOp | A.GeOp) =>
                  (checkInt(trexp left, pos); checkInt(trexp right, pos);
                   {exp=R.nilExp(),ty=T.INT})
             | (A.EqOp | A.NeqOp) =>
                  let
                    val real_left = actual_ty(pos, tenv, #ty (trexp left))
                    val real_right = actual_ty(pos, tenv, #ty (trexp right))
                  in
                    case (real_left, real_right) of
                      (T.INT, T.INT)           => {exp=R.nilExp(), ty=T.INT}
                    | (T.STRING, T.STRING)     => {exp=R.nilExp(), ty=T.INT}
                    | (T.NIL, T.RECORD _)      => {exp=R.nilExp(), ty=T.INT}
                    | (T.RECORD _, T.NIL)      => {exp=R.nilExp(), ty=T.INT}
                    | (T.ARRAY _, T.ARRAY _)   => {exp=R.nilExp(), ty=T.INT}
                    | (T.RECORD _, T.RECORD _) => {exp=R.nilExp(), ty=T.INT}
                    | _                        => (error pos "error"; {exp=R.nilExp(), ty=T.INT})
                  end)
        | trexp (A.RecordExp{fields, typ, pos}) =
            (let
               val typty =
                 case S.look (tenv, typ) of
                   SOME t => t
                 | NONE   => (error pos "Type doesn't exist."; T.UNIT)
             in
               case typty of
                 T.RECORD (stlist, uniqv) =>
                   if (length stlist) = (length fields)
                     then ((foldl (fn (x, ans) =>
                                     let
                                       val defname = (#1 x)
                                       val deftype = (#2 x)
                                       val lhead = (hd ans)
                                       val givenname = (#1 lhead)
                                       val giventype = (#2 lhead)
                                       val givenpos = (#3 lhead)
                                     in
                                       if defname = givenname andalso deftype = giventype
                                         then ()
                                         else error givenpos "Incorrect record type.";
                                       tl ans
                                     end)
                              fields stlist);
                            {exp=R.nilExp(), ty=T.UNIT})
                      else (error pos "Record fields length differ.";
                            {exp=R.nilExp(), ty=T.UNIT})
               | _ => (error pos (S.name typ ^ " isn't an appropriate type.");
                       {exp=R.nilExp(), ty=T.UNIT})
             end)
        | trexp (A.SeqExp exps) =
            let
              val exps' = map trexp (map #1 exps)
              val list_ty = List.last exps'
            in
              {exp=R.nilExp(), ty=(#ty list_ty)}
            end
        | trexp (A.AssignExp{var, exp, pos}) =
            let
              val var' = transVar(venv, tenv, var)
              val exp' = trexp exp
            in
              checkTypeMatch(var', exp', pos);
              {exp=R.nilExp(), ty=T.UNIT}
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
                  checkInt(test', pos);
                  checkTypeMatch(then'', else'', pos);
                  {exp = R.nilExp(), ty = if_ty}
                end
              | NONE =>
                (checkInt(trexp(test), pos);
                 checkUnit(trexp(then'), pos);
                 {exp = R.nilExp(), ty = T.UNIT})
            )
        | trexp (A.WhileExp{test, body, pos}) =
            (checkInt(trexp(test), pos);
            checkUnit(trexp(body), pos);
            {exp = R.nilExp(), ty = T.UNIT})
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
            let
              val venv' = S.enter(venv, var, E.VarEntry{ty=T.INT})
            in
              checkInt(trexp(lo), pos);
              checkInt(trexp(hi), pos);
              checkUnit(transExp(venv', tenv, body), pos);
              {exp = R.nilExp(), ty = T.UNIT}
            end
        | trexp (A.BreakExp pos) =
            let in
            case !nest > 0 of
              true  => {exp = R.nilExp(), ty = T.UNIT}
            | false => (error pos "Break must be inside a loop";
                        {exp = R.nilExp(), ty = T.UNIT})
            end
        | trexp (A.LetExp {decs, body, pos}) =
            let
              val {venv=venv', tenv=tenv'} = transDecs(venv,tenv,decs)
            in
              transExp(venv', tenv', body)
            end
        | trexp (A.ArrayExp{typ, size, init, pos}) =
            let
              val array_ty = actual_ty(pos, tenv, typ)
            in
              checkInt(trexp(size), pos);
              case array_ty of
                T.ARRAY (ty, u) =>
                  (checkTypeMatch({exp=(),ty=ty},trexp init);
                  {exp = R.nilExp(), ty = array_ty})
                | _ => (error pos "Array expected"; {exp = R.nilExp(), ty = T.UNIT})
            end
    in
        trexp exp
    end
    and transVar(venv, tenv, var) =
      let
        fun trvar (A.SimpleVar (id, pos)) =
              (case Symbol.look (venv, id) of
                 SOME (E.VarEntry evrty) => {exp=R.nilExp(), ty=(#ty evrty)}
               | SOME (E.FunEntry _)     => (error pos (Symbol.name id ^ " is function, not a variable.");
                                             {exp=R.nilExp(), ty=T.NIL})
               | _                       => (error pos ("Undefined variable: " ^ Symbol.name id);
                                             {exp=R.nilExp(), ty=T.NIL}))
          | trvar (A.FieldVar (var, id, pos)) =
              (case (trvar var) of
                 {exp, ty=T.RECORD (fieldlist, uniqv)} =>
                   (case List.find (fn (s, ty) => s = id) fieldlist of
                      SOME (s, ty) => {exp=R.nilExp(), ty=ty}
                    | _            => (error pos ("Field \"" ^ Symbol.name id ^ "\" does not belong to record.");
                                       {exp=R.nilExp(), ty=T.UNIT}))
               | _                                  => (error pos ("Var " ^ Symbol.name id ^ " is not a record.");
                                                        {exp=R.nilExp(), ty=T.UNIT}))
          | trvar (A.SubscriptVar (var, exp, pos)) =
              (checkInt(transExp (venv, tenv, exp), pos);
               case (trvar var) of
                 {exp, ty=T.ARRAY (ty, uniqv)} => {exp=R.nilExp(), ty=ty}
               | _                             => (error pos ("Var is not an array.");
                                                   {exp=R.nilExp(), ty=T.UNIT}))
      in
        trvar var
      end
    and transDec (venv, tenv, dec) =
      let
        fun trdec (A.VarDec {name, escape, typ, init, pos}) =
              let
                val trValue = transExp(venv, tenv, init)
              in
                (case typ of
                   SOME t => checkTypeMatch(trValue, {exp=R.nilExp(), ty=t}, pos);
                 {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=ty})})
              end
          | trdec (A.TypeDec [{name, ty, pos}]) =
                {venv=venv, tenv=S.enter(tenv,name,transTy(tenv,ty))}
          | trdec (A.FunctionDec (A.fundeclist [{name, params, body, pos, result=(rt, pos2)}])) =
              let
                val result_ty = case S.look(tenv, rt) of
                                  NONE     => (error pos2 ("Type " ^ Symbol.name rt ^ " does not exist."); NONE)
                                | SOME (t) => t

                fun transparam{name, typ, pos} =
                  case S.look(tenv, typ) of
                    SOME t => {name=name, ty=t}
                  | _      => (error pos ("Type " ^ Symbol.name typ ^ " does not exist.");
                               {name="", ty=T.NIL})
                val params' = map transparam params

                val venv' = S.enter(venv, name, E.FunEntry{formals=map #ty params', result=result_ty})

                fun enterparam ({name, ty}, venv) = S.enter(venv, name, E.VarEntry{access=(),ty=ty})
                val venv'' = foldl enterparam params' venv'

                val retTypeFound = (#ty (transExp(venv'',tenv) body))
              in
                case result_ty of
                  SOME t => if retTypeFound = t
                                     then ()
                                     else error pos ("Return of " ^ Symbol.name name ^ " doesn't conform.")
                | NONE   => if retTypeFound = T.UNIT
                                     then ()
                                     else error pos ("Return of " ^ Symbol.name name ^ " doesn't conform.");
                {venv=venv', tenv=tenv}
              end
      in
        trdec dec
      end
    and transDecs (venv, tenv, decs) = nil
    and transProg (absyn) = (transExp(E.base_venv, E.base_tenv) absyn; ())

    and transTy (tenv, typ) =
      case typ of
        A.NameTy (s, pos) => case S.look(tenv, typ) of
                               SOME (ty) => ty
                             | NONE      => (error pos "Non existent type.")

end