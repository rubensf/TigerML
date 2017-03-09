(* Dummy Translate*)
structure Translate =
struct
  type exp = unit
  fun nilExp() = ()
end

signature SEMANT =
sig
  type venv
  type tenv

  type expty

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
        T.NAME (s, tyopref) => (case (!tyopref) of
                                  SOME (ty') => actual_ty(pos, tenv, ty')
                                | NONE       => T.UNIT)
      | _                   => ty

  fun checkInt({exp, ty}, pos, refstring) =
      case ty of
        T.INT => ()
      | _     => error pos (refstring ^ ": Integer required.")

  fun checkString({exp, ty}, pos, refstring) =
      case ty of
        T.STRING => ()
      | _        => error pos (refstring ^ ": String required.")

  fun checkUnit({exp, ty}, pos, refstring) =
      case ty of
        T.UNIT => ()
      | _      => error pos (refstring ^ ": Unit required.")

  fun checkTypeMatch(ty1, ty2, pos, refstring) =
    if ((ty1 = ty2 andalso ty1 <> T.NIL) orelse
        (ty1 = T.NIL andalso (case ty2 of T.RECORD r => true | _ => false)) orelse
        (ty2 = T.NIL andalso (case ty1 of T.RECORD r => true | _ => false)))
      then true
      else (error pos (refstring ^ ": Type mismatch. Expected: " ^ T.toString ty1 ^
                       ". Given: " ^ T.toString ty2 ^ ".");
            false)

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
        | trexp (A.OpExp {left, oper, right, pos}) =
            (let
              val real_left = actual_ty(pos, tenv, #ty (trexp left))
              val real_right = actual_ty(pos, tenv, #ty (trexp right))
             in
               case oper of
                 (A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.nilExp(), ty=T.INT}
                     | _                    => (error pos "Can only operate ints.";
                                                {exp=R.nilExp(), ty=T.INT}))
               | (A.LtOp | A.LeOp | A.GtOp | A.GeOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.nilExp(), ty=T.INT}
                     | (T.STRING, T.STRING) => {exp=R.nilExp(), ty=T.INT}
                     | _                    => (error pos "Can only compare ints and strings.";
                                                {exp=R.nilExp(), ty=T.INT}))
               | (A.EqOp | A.NeqOp) => (checkTypeMatch(real_left, real_right,
                                                       pos, "Equal/NEqual Comp");
                                        {exp=R.nilExp(), ty=T.INT})
             end)
        | trexp (A.CallExp {func, args, pos}) =
            (case S.look(venv, func) of
               SOME (E.FunEntry {formals, result}) =>
                 if (length formals) = (length args)
                   then ((foldl (fn (x, ans) =>
                                   let
                                     val defty   = (hd ans)
                                     val giventy = (#ty (trexp x))
                                   in
                                     checkTypeMatch(defty, giventy, pos, "Function Call");
                                     tl ans
                                   end)
                              formals args);
                            {exp=R.nilExp(), ty=result})
                   else (error pos "Function args length differ from defined.";
                         {exp=R.nilExp(), ty=result})
               | SOME (E.VarEntry {ty})            => (error pos ("Function expected, but variable found.");
                                                       {exp=R.nilExp(), ty=ty})
               | NONE                              => (error pos ("Function " ^ S.name func ^ " does not exist.");
                                                       {exp=R.nilExp(), ty=T.UNIT}))
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
                                       val defty = (#2 x)
                                       val lhead = (hd ans)
                                       val givenname = (#1 lhead)
                                       val giventy   = (#ty (trexp (#2 lhead)))
                                       val givenpos  = (#3 lhead)
                                     in
                                       if defname = givenname
                                         then ()
                                         else (error givenpos ("Incorrect record type. Expected field " ^ Symbol.name defname ^
                                                               ". Given field " ^ Symbol.name givenname ^ "."); ());
                                       checkTypeMatch(defty, giventy, givenpos, "Record Expression");
                                       tl ans
                                     end)
                              fields stlist);
                            {exp=R.nilExp(), ty=typty})
                      else (error pos "Record fields length differ from defined.";
                            {exp=R.nilExp(), ty=T.UNIT})
               | _ => (error pos (S.name typ ^ " isn't a type.");
                       {exp=R.nilExp(), ty=T.UNIT})
             end)
        | trexp (A.SeqExp exps) =
            if List.null exps
              then {exp=R.nilExp(), ty=T.UNIT}
              else let
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
              checkTypeMatch(#ty var', #ty exp', pos, "Assignment");
              {exp=R.nilExp(), ty=(#ty var')}
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
                  checkInt(test', pos, "If statement");
                  checkTypeMatch(#ty then'', #ty else'', pos, "If statement");
                  {exp=R.nilExp(), ty=if_ty}
                end
              | NONE =>
                (checkInt(trexp(test), pos, "If statement");
                 checkUnit(trexp(then'), pos, "If statement");
                 {exp=R.nilExp(), ty=T.UNIT})
            )
        | trexp (A.WhileExp{test, body, pos}) =
            (checkInt(trexp(test), pos, "While loop");
             nest := !nest + 1;
             checkUnit(trexp(body), pos, "While loop");
             nest := !nest - 1;
             {exp = R.nilExp(), ty = T.UNIT})
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
            let
              val venv' = S.enter(venv, var, E.VarEntry{ty=T.INT})
            in
              checkInt(trexp(lo), pos, "For loop");
              checkInt(trexp(hi), pos, "For loop");
              nest := !nest + 1;
              checkUnit(transExp(venv', tenv, body), pos, "For loop");
              nest := !nest - 1;
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
              val depth = !nest
              val _ = (nest := 0)
              val {venv=venv', tenv=tenv'} = foldl (fn (x, ans) =>
                                                      transDec (#venv ans, #tenv ans, x))
                                               {venv=venv, tenv=tenv} decs
              val _ = (nest := depth)
            in
              transExp(venv', tenv', body)
            end
        | trexp (A.ArrayExp{typ, size, init, pos}) =
            let
              val init_ty = (#ty (trexp init))
              val array_ty = case S.look(tenv, typ) of
                               SOME typ' => actual_ty(pos, tenv, typ')
                             | NONE      => (error pos "This type doesn't exist.";
                                             T.ARRAY (T.UNIT, ref ()))
            in
              checkInt(trexp(size), pos, "Array Expr");
              case array_ty of
                T.ARRAY (ty, u) =>
                  (checkTypeMatch(ty, #ty (trexp init), pos, "Array Expr");
                   {exp=R.nilExp(), ty=array_ty})
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
              (checkInt(transExp (venv, tenv, exp), pos, "Field of var");
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
                val typ' = case typ of
                             SOME (s, p) => (case S.look(tenv, s) of
                                               SOME t => SOME t
                                             | NONE   => (error pos "Type doesn't exist."; NONE))
                           | NONE        => NONE
              in
                ((case trValue of
                    {ty=T.UNIT, ...} => (error pos "Cannot assign to unit."; ())
                  | {ty=T.NIL, ...}  => (case typ' of
                                          SOME (T.RECORD r) => ()
                                        | _                 => (error pos "Can only assign nil to records."; ()))
                  | {ty=_, ...}     => ());
                 (case typ' of
                    SOME t => (checkTypeMatch(#ty trValue, t, pos, "Var Declaration"); ())
                  | NONE   => ());
                  {tenv=tenv, venv=S.enter(venv, name,
                                           (case typ' of
                                              SOME t => E.VarEntry {ty=t}
                                            | NONE   => E.VarEntry {ty=(#ty trValue)}))})
              end
          | trdec (A.TypeDec tydecs) =
              let
                fun addHeader({name, ty, pos}, env) = S.enter(env, name, T.NAME(name, ref NONE))
                fun processDec({name, ty, pos}, env) = 
                      case S.look(env, name) of 
                        SOME (T.NAME(symb, tyopref)) => (tyopref := SOME(transTy(env,ty)); env)
                      | NONE                         => (error pos "Symbol not found during type declaration"; env)
                fun cycle(visited, tyop, pos) = case tyop of 
                    NONE    => (error pos ("type not found when performing cycle detection:"); false)
                  | SOME ty => (case ty of
                        T.NAME(symb, tyopref) => case (List.all (fn x => x <> symb) visited) of
                            true  => cycle(symb::visited, !tyopref, pos)
                          | false => false)
                fun valid(env, nil)      = ()
                  | valid(env, {name, ty, pos}::rest) = case S.look(env, name) of 
                      SOME(T.NAME(_,tyopref)) => (case (not(cycle([name], !tyopref, pos))) of
                          true  => (error (pos) ("cycle detected in type definition:" ^ S.name(name)))
                        | false => valid(env, rest))
                    | _                       => (error pos "Did not find type in cycle detection") 
                val tenv'  = foldr addHeader tenv tydecs;
                val tenv'' = foldr processDec tenv' tydecs;
              in
                valid (tenv'', tydecs);
                {venv=venv, tenv=tenv''}
              end
          | trdec (A.FunctionDec [{name, params, result, body, pos}]) =
              let
                val result_ty = case result of
                                  SOME (rt, pos2) => (case S.look(tenv, rt) of
                                                        SOME t => t
                                                      | NONE   => (error pos2 ("Type " ^ Symbol.name rt ^ " does not exist."); T.UNIT))
                                | NONE => T.UNIT

                fun transparam ({name, escape, typ, pos} : A.field) =
                  case S.look(tenv, typ) of
                    SOME t => {name=name, ty=t}
                  | NONE   => (error pos ("Type " ^ Symbol.name typ ^ " does not exist.");
                               {name=S.symbol "", ty=T.NIL})
                val params' = map transparam params

                val venv' = S.enter(venv, name, E.FunEntry{formals=map #ty params', result=result_ty})

                fun enterparam ({name, ty}, venv : venv) = S.enter(venv, name, E.VarEntry {ty=ty})
                val venv'' = foldl enterparam venv' params'

                val retTypeFound = (#ty (transExp(venv'', tenv, body)))
              in
                checkTypeMatch(result_ty, retTypeFound, pos, "Function Declaration");
                {venv=venv', tenv=tenv}
              end
      in
        trdec dec
      end
    and transProg (absyn) = (transExp(E.base_venv, E.base_tenv, absyn); ())
    and transTy (tenv, typ) =
      case typ of
        A.NameTy (s, pos) => (case S.look(tenv, s) of
                                SOME ty => ty
                              | NONE    => ((error pos "Non existent type."); T.UNIT))
      | A.RecordTy flist  => T.RECORD (foldl (fn (x, ans) => case S.look(tenv, (#typ x)) of
                                                              SOME ty => ans @ [((#name x), ty)]
                                                            | NONE    => (error (#pos x) "Non existent type.";
                                                                          ans @ [((#name x), T.UNIT)]))
                                        [] flist, ref ())
      | A.ArrayTy (s, pos) => (case S.look(tenv, s) of
                                 SOME ty => T.ARRAY (ty, ref ())
                               | NONE    => ((error pos "Non existent type."); T.UNIT))

end