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
	val transVar : venv * tenv * Absyn.var -> expty
	val transExp : venv * tenv * Absyn.exp -> expty
	val transDec : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
	val transTy  :        tenv * Absyn.ty  -> Types.ty
end

structure Semant = 
struct
	structure A = Absyn
	structure E = Env
	structure P = PrintAbsyn
	structure S = Symbol
	structure T = Types
	val error = ErrorMsg.error

	fun checkInt({exp, ty}, pos) =
	    case ty of T.INT => ()
	      	         | _ => error pos "Integer required"

	fun checkUnit({exp, ty}, pos) =
	    case ty of T.UNIT => ()
	      			  | _ => error pos "Unit required"

	VarExp of var
        | NilExp
        | IntExp of int
        | StringExp of string * pos
        | CallExp of {func: symbol, args: exp list, pos: pos}
        | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
        | RecordExp of {fields: (symbol * exp * pos) list,
			typ: symbol, pos: pos}
        | SeqExp of (exp * pos) list
        | AssignExp of {var: var, exp: exp, pos: pos}
        | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
        | WhileExp of {test: exp, body: exp, pos: pos}
	    | ForExp of {var: symbol, escape: bool ref,
		     lo: exp, hi: exp, body: exp, pos: pos}
        | BreakExp of pos
        | LetExp of {decs: dec list, body: exp, pos: pos}
        | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}


	fun transExp(venv, tenv) = 
		let 
			fun trexp (A.VarExp) = 
			  | trexp (A.NilExp var) =
			  | trexp (A.IntExp i) =
			  | trexp (A.StringExp (str,pos)) =
			  | trexp (A.CallExp{func,args,pos}) =
			  | trexp (A.OpExp{left,oper,right,pos})) =
			  | trexp (A.RecordExp{fields,typ,pos}) = 
			  | trexp (A.SeqExp exps) = 
			  | trexp (A.AssignExp{var,exp,pos}) =
			  | trexp (A.IfExp{test,then',else',pos}) =
			  | trexp (A.WhileExp{test,body,pos}) =
			  | trexp (A.ForExp{var,escape,lo,hi,body,pos}) =
			  | trexp (A.BreakExp pos) =
			  | trexp (A.LetExp {decs,body,pos}) =
			  | trexp (A.ArrayExp{typ,size,init,pos}) =
			and trvar (A.SimpleVar(id,pos)) = 
          	  | trvar (A.FieldVar(var,id,pos)) =
              | trvar (A.SubscriptVar(var, exp,pos)) =
    	in
      		trexp
    	end

    and transDec (venv, tenv) = 
    and transDecs (venv, tenv, decs) =

    fun transProg(absyn) = (transExp(E.base_venv, E.base_tenv) absyn; ())

end