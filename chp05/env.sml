signature ENV =
sig
  type access
  type ty

  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

(* Make it transparent coz too much of a hassle not knowing type. *)
structure Env : ENV =
struct
  structure S = Symbol
  structure T = Types

  (* Dunno what's this yet *)
  type access = unit

  type ty = T.ty

  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  val predef_types = [("int", T.INT),
                      ("string", T.STRING)]

  fun fillTypeFn ((name, ty), ans) = S.enter(ans, S.symbol name, ty)
  val base_tenv = foldl fillTypeFn S.empty predef_types

  val predef_funcs = [("print",     FunEntry {formals=[T.STRING], result=T.UNIT}),
                      ("flush",     FunEntry {formals=[], result=T.UNIT}),
                      ("getchar",   FunEntry {formals=[], result=T.STRING}),
                      ("ord",       FunEntry {formals=[T.STRING], result=T.INT}),
                      ("chr",       FunEntry {formals=[T.INT], result=T.STRING}),
                      ("size",      FunEntry {formals=[T.STRING], result=T.INT}),
                      ("substring", FunEntry {formals=[T.STRING,T.INT,T.INT], result=T.STRING}),
                      ("concat",    FunEntry {formals=[T.STRING,T.STRING], result=T.STRING}),
                      ("not",       FunEntry {formals=[T.INT], result=T.INT}),
                      ("exit",      FunEntry {formals=[T.INT], result=T.UNIT})]

  fun fillFuncFn ((name, entry), ans) = S.enter(ans, S.symbol name, entry)
  val base_venv = foldl fillFuncFn S.empty predef_funcs
end