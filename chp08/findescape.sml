structure FindEscape: sig
  val findEscape: Absyn.exp -> unit
end =
struct
  structure A = Absyn
  structure S = Symbol
  type depth = int
  type escEnv = (depth * bool ref) S.table

  fun traverseVar(env:escEnv, d:depth, s:A.var): unit =
    (let
      fun travvar (A.SimpleVar(sym, pos)) = (case S.look(env, sym) of
        SOME (dep, b) => (if d > dep then b := true else ())
      | _ => ()) (* error checking here? *)
        | travvar (A.FieldVar (var, sym, pos)) = traverseVar(env, d, var)
        | travvar (A.SubscriptVar (var, exp, pos)) = traverseVar(env, d, var)
    in
      travvar s
    end)
  and traverseExp(env:escEnv, d:depth, s:A.exp): unit =
    (let
      fun travexp (A.VarExp var) = traverseVar(env, d, var)
        | travexp (A.NilExp) = ()
        | travexp (A.IntExp (_)) = ()
        | travexp (A.StringExp (_,_)) = ()
        | travexp (A.CallExp {func, args, pos}) =
          (let
            fun f (nil) = ()
            | f (arg::rest) = (traverseExp(env, d, arg);f(rest))
          in
            f args
          end)
        | travexp (A.OpExp {left, oper, right, pos}) = (traverseExp(env, d, left);traverseExp(env, d, right))
        | travexp (A.RecordExp {fields, typ, pos}) =
          (let
            fun f (nil) = ()
            | f ((_,exp,_)::rest) = (traverseExp(env,d,exp);f(rest))
          in
            f fields
          end)
        | travexp (A.SeqExp exps) =
          (let
            fun f (nil) = ()
            | f ((exp,pos)::rest) = (traverseExp(env,d,exp);f(rest))
          in
            f exps
          end)
        | travexp (A.AssignExp {var, exp, pos}) = (traverseVar(env,d,var);traverseExp(env,d,exp))
        | travexp (A.IfExp {test, then', else', pos}) = (traverseExp(env,d,test);traverseExp(env,d,then');(
            case else' of
              SOME exp => traverseExp(env,d,exp)
            | NONE => ()
          ))
        | travexp (A.WhileExp {test, body, pos}) = (traverseExp(env,d,test);traverseExp(env,d,body))
        | travexp (A.ForExp {var, escape, lo, hi, body, pos}) =
          (let
            val env' = S.enter(env,var,(d,escape))
          in
            (* d+1 ? *)
            (escape:=false;traverseExp(env,d,lo);traverseExp(env,d,hi);traverseExp(env',d,body))
          end)
        | travexp (A.BreakExp pos) = ()
        | travexp (A.LetExp {decs, body, pos}) =
          (let
            val env' = traverseDecs(env,d,decs)
          in
            traverseExp(env',d,body)
          end)
        | travexp (A.ArrayExp {typ,size,init,pos}) = (traverseExp(env,d,size);traverseExp(env,d,init))
      in
        travexp s
      end)
  and traverseDecs(env, d, s: A.dec list): escEnv =
    (let
      fun f (A.FunctionDec fundecs, env) =
        (let
          fun h ({name,escape,typ,pos} : A.field, env) = (escape:=false;S.enter(env,name,(d+1,escape)))
          fun g ({name,params,result,body,pos} : A.fundec,env) =
            (let
              val env' = foldl h env params
            in
              (traverseExp(env',d+1,body);env')
            end)
        in
          foldl g env fundecs
        end)
      | f (A.VarDec {name,escape,typ,init,pos},env) = (
        escape:=false;
        traverseExp(env,d,init);
        S.enter(env,name,(d,escape)))
      | f(A.TypeDec (_), env) = env
    in
      foldl f env s
    end)

  fun findEscape(prog: A.exp): unit =
    (let
      val table = S.empty
    in
      traverseExp(table,0,prog)
    end)
end