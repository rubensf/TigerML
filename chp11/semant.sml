signature SEMANT = sig
  type frag
  val transProg : Absyn.exp -> frag list
end

functor Semant(R: TRANSLATE) :> SEMANT where type frag = R.frag =
struct
  structure A = Absyn
  structure E = Env (R)
  structure P = PrintAbsyn
  structure S = Symbol
  structure T = Types

  type frag = R.frag
  type venv = E.enventry S.table
  type tenv = T.ty S.table

  type expty = {exp: R.exp, ty: T.ty}

  val error = ErrorMsg.error
  val nest = ref 0

  fun actual_ty (tenv, ty) =
    case ty of
        T.NAME (s, tyopref) => (case (!tyopref) of
                                  SOME (ty') => actual_ty(tenv, ty')
                                | NONE       => T.UNIT)
      | _                   => ty

  fun checkInt({exp, ty}, pos, refstring) =
      case ty of
        T.INT => true
      | _     => (error pos (refstring ^ ": Integer required."); false)

  fun checkString({exp, ty}, pos, refstring) =
      case ty of
        T.STRING => true
      | _        => (error pos (refstring ^ ": String required."); false)

  fun checkUnit({exp, ty}, pos, refstring) =
      case ty of
        T.UNIT => true
      | _      => (error pos (refstring ^ ": Unit required."); false)

  fun checkTypeMatch(t1, t2, tenv, pos, refstring) =
    let
      val ty1 = actual_ty (tenv, t1)
      val ty2 = actual_ty (tenv, t2)
    in
      if ((ty1 = ty2 andalso ty1 <> T.NIL) orelse
          (ty1 = T.NIL andalso (case ty2 of T.RECORD r => true | _ => false)) orelse
          (ty2 = T.NIL andalso (case ty1 of T.RECORD r => true | _ => false)))
        then true
        else (error pos (refstring ^ ": Type mismatch. Expected: " ^ T.toString ty1 ^
                         ". Given: " ^ T.toString ty2 ^ ".");
              false)
    end

  fun transExp(venv, tenv, level, exp, break) =
    let
      fun trexp (A.VarExp var) =
            transVar(venv, tenv, level, var, break)
        | trexp (A.NilExp) =
            {exp=R.nilExp(), ty=T.NIL}
        | trexp (A.IntExp i) =
            {exp=R.intExp(i), ty=T.INT}
        | trexp (A.StringExp (str,pos)) =
            {exp=R.strExp(str), ty=T.STRING}
        | trexp (A.OpExp {left, oper, right, pos}) =
            (let
              val left = (trexp left)
              val right = (trexp right)
              val real_left = actual_ty(tenv, #ty left)
              val real_right = actual_ty(tenv, #ty right)
             in
               case oper of
                 (A.PlusOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | _                    => (error pos "Can only operate ints.";
                                                {exp=R.errExp(), ty=T.INT}))
               | (A.MinusOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | _                    => (error pos "Can only operate ints.";
                                                {exp=R.errExp(), ty=T.INT}))
               | (A.TimesOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | _                    => (error pos "Can only operate ints.";
                                                {exp=R.errExp(), ty=T.INT}))
               | (A.DivideOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | _                    => (error pos "Can only operate ints.";
                                                {exp=R.errExp(), ty=T.INT}))
               | (A.LtOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | (T.STRING, T.STRING) => {exp=R.strOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | _                    => (error pos "Can only compare ints and strings.";
                                                {exp=R.errExp(), ty=T.INT}))
               | (A.LeOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | (T.STRING, T.STRING) => {exp=R.strOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | _                    => (error pos "Can only compare ints and strings.";
                                                {exp=R.errExp(), ty=T.INT}))
               | (A.GtOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | (T.STRING, T.STRING) => {exp=R.strOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | _                    => (error pos "Can only compare ints and strings.";
                                                {exp=R.errExp(), ty=T.INT}))
               | (A.GeOp) =>
                    (case (real_left, real_right) of
                       (T.INT, T.INT)       => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | (T.STRING, T.STRING) => {exp=R.strOpExp(oper, #exp left, #exp right), ty=T.INT}
                     | _                    => (error pos "Can only compare ints and strings.";
                                                {exp=R.errExp(), ty=T.INT}))
               | (A.EqOp) =>
                    (if checkTypeMatch(real_left, real_right, tenv, pos, "Equal/NEqual Comp")
                     then case (real_left, real_right) of
                            (T.STRING, T.STRING) => {exp=R.strOpExp(oper, #exp left, #exp right), ty=T.INT}
                          |  _                   => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     else (error pos "Can only compare same types."; {exp=R.errExp(), ty=T.INT}))
               | (A.NeqOp) =>
                    (if checkTypeMatch(real_left, real_right, tenv, pos, "Equal/NEqual Comp")
                     then case (real_left, real_right) of
                            (T.STRING, T.STRING) => {exp=R.strOpExp(oper, #exp left, #exp right), ty=T.INT}
                          |  _                   => {exp=R.intOpExp(oper, #exp left, #exp right), ty=T.INT}
                     else (error pos "Can only compare same types."; {exp=R.errExp(), ty=T.INT}))
             end)
        | trexp (A.CallExp {func, args, pos}) =
            (case S.look(venv, func) of
               SOME (E.FunEntry {level=declevel, label, formals, result}) =>
                 if (length formals) = (length args)
                   then ((foldl (fn (x, ans) =>
                                   let
                                     val defty   = (hd ans)
                                     val giventy = (#ty (trexp x))
                                   in
                                     checkTypeMatch(defty, giventy, tenv, pos, "Function Call");
                                     tl ans
                                   end)
                              formals args);
                            {exp=R.callExp(declevel, level, label, map #exp (map trexp args)), ty=result})
                   else (error pos "Function args length differ from defined.";
                         {exp=R.errExp(), ty=result})
               | SOME (E.VarEntry {access, ty}) => (error pos ("Function expected, but variable found.");
                                                         {exp=R.errExp(), ty=ty})
               | NONE                                => (error pos ("Function " ^ S.name func ^ " does not exist.");
                                                         {exp=R.errExp(), ty=T.UNIT}))
        | trexp (A.RecordExp{fields, typ, pos}) =
            (let
               val typty =
                 case S.look(tenv, typ) of
                   SOME t => actual_ty (tenv, t)
                 | NONE   => (error pos "Type doesn't exist."; T.UNIT)
             in
               case typty of
                 T.RECORD (stlist, uniqv) =>
                   if (length stlist) = (length fields)
                     then let
                            val exps = ref []
                          in
                            ((foldl (fn (x, ans) =>
                                       let
                                         val defname = (#1 x)
                                         val defty = (#2 x)
                                         val lhead = (hd ans)
                                         val resolve   = (trexp (#2 lhead))
                                         val givenname = (#1 lhead)
                                         val giventy   = (#ty resolve)
                                         val givenpos  = (#3 lhead)
                                       in
                                         exps := (#exp resolve)::(!exps);
                                         if defname = givenname
                                           then ()
                                           else (error givenpos ("Incorrect record type. Expected field " ^ Symbol.name defname ^
                                                                 ". Given field " ^ Symbol.name givenname ^ "."); ());
                                         checkTypeMatch(defty, giventy, tenv, givenpos, "Record Expression");
                                         tl ans
                                       end)
                                fields stlist);
                              {exp=R.recCreation(!exps), ty=typty})
                          end
                      else (error pos "Record fields length differ from defined.";
                            {exp=R.errExp(), ty=T.UNIT})
               | _ => (error pos (S.name typ ^ " isn't a type.");
                       {exp=R.errExp(), ty=T.UNIT})
             end)
        | trexp (A.SeqExp exps) =
            if List.null exps
              then {exp=R.seqExp([]), ty=T.UNIT}
              else let
                     val exps' = map trexp (map #1 exps)
                     val list_ty = List.last exps'
                   in
                     {exp=R.seqExp(map #exp exps'), ty=(#ty list_ty)}
                   end
        | trexp (A.AssignExp{var, exp, pos}) =
            let
              val var' = transVar(venv, tenv, level, var, break)
              val exp' = trexp exp
            in
              checkTypeMatch(#ty var', #ty exp', tenv, pos, "Assignment");
              {exp=R.assignExp(#exp var', #exp exp'), ty=T.UNIT}
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
                  checkTypeMatch(#ty then'', #ty else'', tenv, pos, "If statement");
                  {exp=R.ifThenElseExp(#exp test', #exp then'', #exp else''), ty=if_ty}
                end
              | NONE =>
                (checkInt(trexp(test), pos, "If statement");
                 checkUnit(trexp(then'), pos, "If statement");
                 {exp=R.ifThenExp(#exp (trexp test), #exp (trexp then')), ty=T.UNIT})
            )
        | trexp (A.WhileExp{test, body, pos}) =
            let
              val _ = (nest := !nest + 1);
              val breaklabel = Temp.newlabel ()
              val body' = transExp(venv, tenv, level, body, breaklabel)
              val test' = trexp(test)
              val _ = (nest := !nest - 1);
            in
              checkInt(trexp(test), pos, "While loop");
              checkUnit(trexp(body), pos, "While loop");
              {exp=R.whileExp(#exp test', #exp body', breaklabel), ty=T.UNIT}
            end
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
            let
              val access = R.allocLocal level (!escape)
              val venv' = S.enter(venv, var, E.VarEntry{access=access, ty=T.INT})
              val _ = (nest := !nest + 1);
              val lo' = trexp(lo)
              val hi' = trexp(hi)
              val breaklabel = Temp.newlabel ()
              val body' = transExp(venv', tenv, level, body, breaklabel)
              val _ = (nest := !nest - 1);
            in
              checkInt(lo', pos, "For loop");
              checkInt(hi', pos, "For loop");
              checkUnit(body', pos, "For loop");
              {exp=R.forExp(R.simpleVarAccess (access, level), breaklabel, #exp lo', #exp hi', #exp body'), ty = T.UNIT}
            end
        | trexp (A.BreakExp pos) =
            let in
            case !nest > 0 of
              true  => {exp = R.breakExp(break), ty = T.UNIT}
            | false => (error pos "Break must be inside a loop";
                        {exp = R.errExp(), ty = T.UNIT})
            end
        | trexp (A.LetExp {decs, body, pos}) =
            let
              val depth = !nest
              val _ = (nest := 0)
              val {venv=venv', tenv=tenv', exps=exps'} =
                    foldl (fn (x, ans) =>
                             let
                               val result = transDec (#venv ans, #tenv ans, level, x, break)
                             in
                               {venv=(#venv result), tenv=(#tenv result), exps=((#exps ans) @ (#exps result))}
                             end)
                          {venv=venv, tenv=tenv, exps=[]} decs
              val _ = (nest := depth)
              val letRet = transExp(venv', tenv', level, body, break)
            in
              {exp=R.packExps(exps', (#exp letRet)), ty=(#ty letRet)}
            end
        | trexp (A.ArrayExp{typ, size, init, pos}) =
            let
              val size = trexp size
              val init = trexp init
              val init_ty = (#ty init)
              val array_ty = case S.look(tenv, typ) of
                               SOME typ' => actual_ty(tenv, typ')
                             | NONE      => (error pos "This type doesn't exist.";
                                             T.ARRAY (T.UNIT, ref ()))
            in
              checkInt(size, pos, "Array Expr");
              case array_ty of
                T.ARRAY (ty, u) =>
                  (checkTypeMatch(ty, #ty init, tenv, pos, "Array Expr");
                   {exp=R.arrCreation(#exp size, #exp init), ty=array_ty})
              | _ => (error pos "Array expected"; {exp = R.errExp(), ty = T.UNIT})
            end
    in
        trexp exp
    end
    and transVar(venv, tenv, level, var, break) =
      let
        fun trvar (A.SimpleVar (id, pos)) =
              (case Symbol.look (venv, id) of
                 SOME (E.VarEntry evrty) => {exp=R.simpleVarAccess(#access evrty, level), ty=(#ty evrty)}
               | SOME (E.FunEntry _)     => (error pos (Symbol.name id ^ " is function, not a variable.");
                                             {exp=R.errExp(), ty=T.NIL})
               | _                       => (error pos ("Undefined variable: " ^ Symbol.name id);
                                             {exp=R.errExp(), ty=T.NIL}))
          | trvar (A.FieldVar (var, id, pos)) =
              let
                val var = trvar var
                val varty = actual_ty(tenv, (#ty var))
              in
                (case varty of
                   T.RECORD (fieldlist, uniqv) =>
                     let
                       val posList = foldl (fn ((s, t), ans) => ans @ [(s, t, length ans)]) [] fieldlist
                       val findPos = foldl (fn ((s, t, p), ans) => if s = id then (p, t) else ans) ((0-1), T.INT) posList
                     in
                       if (#1 findPos) <> (0-1)
                       then {exp=R.fieldVarAccess((#exp var), R.intExp (#1 findPos)), ty=(#2 findPos)}
                       else (error pos ("Field \"" ^ Symbol.name id ^ "\" does not belong to record.");
                             {exp=R.errExp(), ty=T.UNIT})
                    end
                 | _                                  => (error pos ("Var " ^ Symbol.name id ^ " is not a record.");
                                                          {exp=R.errExp(), ty=T.UNIT}))
              end
          | trvar (A.SubscriptVar (var, exp, pos)) =
              let
                val subscrExp = transExp (venv, tenv, level, exp, break)
                val subscrExp' =
                  if checkInt(subscrExp, pos, "Pos of array var")
                  then (#exp subscrExp)
                  else R.errExp()
                val varty = (trvar var)
                val varty' = actual_ty (tenv, (#ty varty))
              in
                 case varty' of
                   T.ARRAY (ty, uniqv) => {exp=R.arrayVarAccess((#exp varty), subscrExp'), ty=ty}
                 | _                   => (error pos ("Var is not an array.");
                                           {exp=R.errExp(), ty=T.UNIT})
              end
      in
        trvar var
      end
    and transDec (venv, tenv, level, dec, break) =
      let
        fun trdec (A.VarDec {name, escape, typ, init, pos}, exps) =
              let
                val trValue = transExp(venv, tenv, level, init, break)
                val typ' = case typ of
                             SOME (s, p) => (case S.look(tenv, s) of
                                               SOME t => SOME (actual_ty (tenv, t))
                                             | NONE   => (error pos "Type doesn't exist."; NONE))
                           | NONE        => NONE
                val access = R.allocLocal level (!escape)
                val varAc = R.simpleVarAccess (access, level)
                val assign = R.assignExp (varAc, (#exp trValue))
              in
                ((case trValue of
                    {ty=T.UNIT, ...} => (error pos "Cannot assign to unit."; ())
                  | {ty=T.NIL, ...}  => (case typ' of
                                          SOME (T.RECORD r) => ()
                                        | _                 => (error pos "Can only assign nil to records."; ()))
                  | {ty=_, ...}     => ());
                 (case typ' of
                    SOME t => (checkTypeMatch(#ty trValue, t, tenv, pos, "Var Declaration"); ())
                  | NONE   => ());
                  {tenv=tenv, venv=S.enter(venv, name,
                                           (case typ' of
                                              SOME t => E.VarEntry {access=access, ty=t}
                                            | NONE   => E.VarEntry {access=access, ty=(#ty trValue)})),
                   exps=assign::exps})
              end
          | trdec (A.TypeDec tydecs, exps) =
              let
                val tnames = map #name tydecs

                fun checkNames (x, []) = []
                  | checkNames (x, ans : A.symbol list) =
                  (case List.find (fn y => y = x) ans of
                     SOME _ => (error (#pos (hd tydecs)) "Multiple types with same name."; tl ans) (*This may print the error multiple times...*)
                   | NONE   => tl ans)

                val _ = foldl checkNames (tl tnames) tnames

                fun addHeader({name, ty, pos}, tenv) = S.enter(tenv, name, T.NAME(name, ref NONE))
                val tenv'  = foldr addHeader tenv tydecs;

                fun processDec({name, ty, pos}, env) =
                      case S.look(env, name) of
                        SOME (T.NAME(symb, tyopref)) => (tyopref := SOME(transTy(env,ty)); env)
                      | _                            => (error pos "Error during type declaration."; env)
                val tenv'' = foldr processDec tenv' tydecs;

                fun cycle(path, current) =
                  case (List.find (fn x => x = current)) path of
                    SOME _ => true
                  | _      => case !(#2 current) of
                                SOME (T.NAME n) => cycle(path @ [current], n)
                              | _               => false

                fun valid(env, nil)      = ()
                  | valid(env, {name, ty, pos}::rest) =
                      case S.look(env, name) of
                        SOME (T.NAME t) =>
                          (case !(#2 t) of
                             SOME (T.NAME n) => if not(cycle([t], n))
                                                  then valid(env, rest)
                                                  else (error pos "Cycle found on type declarations."; valid(env, rest))
                           | _               => valid(env, rest))
                      | _                       => (error pos "Did not find type in cycle detection")
              in
                valid (tenv'', tydecs);
                {venv=venv, tenv=tenv'', exps=exps}
              end
          | trdec (A.FunctionDec fundecs, exps) =
              let
                val fnames = map #name fundecs

                fun checkNames (x, []) = []
                  | checkNames (x, ans : A.symbol list) =
                  (case List.find (fn y => y = x) ans of
                     SOME _ => (error (#pos (hd fundecs)) "Multiple functions with same name."; tl ans) (*This may print the error multiple times...*)
                   | NONE   => tl ans)

                val _ = foldl checkNames (tl fnames) fnames

                fun processDec({name, params, result, body, pos}, venv) =
                  let
                    val result_ty = case result of
                                      SOME (rt, pos2) => (case S.look(tenv, rt) of
                                                            SOME t => actual_ty (tenv, t)
                                                          | NONE   => (error pos2 ("Type " ^ Symbol.name rt ^ " does not exist."); T.UNIT))
                                    | NONE => T.UNIT

                    fun transparam ({name, escape, typ, pos} : A.field) =
                      case S.look(tenv, typ) of
                        SOME t => actual_ty (tenv, t)
                      | NONE   => (error pos ("Type " ^ Symbol.name typ ^ " does not exist.");
                                   T.UNIT)
                    val params' = map transparam params

                    val escList = foldl (fn (x, ans) => !(#escape x)::ans) [] params
                    val trLevelName = Temp.newlabel ()
                    val trNewLevel = R.newLevel {parent=level, name=trLevelName, formals=escList}
                  in
                    S.enter(venv, name, E.FunEntry{level=trNewLevel, label=trLevelName, formals=params', result=result_ty})
                  end

                  val venv' = foldl processDec venv fundecs

                  fun checkFunc {name, params, result, body, pos} =
                    let
                      (* Don't print errors here since was already printed on first pass. *)
                      val result_ty = case result of
                                        SOME (rt, pos2) => (case S.look(tenv, rt) of
                                                              SOME t => actual_ty (tenv, t)
                                                            | NONE   => T.UNIT)
                                      | NONE => T.UNIT

                      fun transparam ({name, escape, typ, pos} : A.field) =
                        case S.look(tenv, typ) of
                          SOME t => (name, escape, actual_ty (tenv, t))
                        | NONE   => (name, escape, T.UNIT)
                      (* Arguments for new functions go in the current function frame. *)
                      fun enterparam ((name, escape, ty), venv) =
                         S.enter(venv, name, E.VarEntry {access=R.allocLocal level (!escape),
                                                         ty=ty})
                      val venv'' = foldl enterparam venv' (map transparam params)

                      val funcTrLevel =
                        case S.look(venv', name) of
                          SOME (E.FunEntry t) => (#level t)
                        | SOME (E.VarEntry _) => ((error pos "Internal error processing functions var."); level)
                        | NONE                => ((error pos "Internal error processing functions none."); level)

                      val funcExp = transExp(venv'', tenv, funcTrLevel, body, break)
                      val retTypeFound = (#ty funcExp)
                    in
                      R.procEntryExit {level=funcTrLevel, body=(#exp funcExp)};
                      checkTypeMatch(result_ty, retTypeFound, tenv, pos, "Function Declaration")
                    end
              in
                List.all checkFunc fundecs;
                {venv=venv', tenv=tenv, exps=exps}
              end
      in
        trdec (dec, [])
      end
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
    and transProg (absyn) =
      let
        val ir = #exp (transExp(E.base_venv,
                                E.base_tenv,
                                R.outermost,
                                absyn,
                                Temp.newlabel ()))
      in
        R.procEntryExit {level=R.outermost, body=ir};
        R.getResult ()
      end
end
