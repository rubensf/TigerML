signature ENV =
sig
  type access
  type ty

  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label,
                                   formals: ty list, result: ty}

  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

(* Make it transparent coz too much of a hassle not knowing type. *)
structure Env : ENV =
struct
  structure R = Translate
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

  val base_label = Temp.namedlabel "BaseFunctions"
  val predef_funcs = [("print",     FunEntry {level=R.outermost, label=base_label, formals=[T.STRING], result=T.UNIT}),
                      ("flush",     FunEntry {level=R.outermost, label=base_label, formals=[], result=T.UNIT}),
                      ("getchar",   FunEntry {level=R.outermost, label=base_label, formals=[], result=T.STRING}),
                      ("ord",       FunEntry {level=R.outermost, label=base_label, formals=[T.STRING], result=T.INT}),
                      ("chr",       FunEntry {level=R.outermost, label=base_label, formals=[T.INT], result=T.STRING}),
                      ("size",      FunEntry {level=R.outermost, label=base_label, formals=[T.STRING], result=T.INT}),
                      ("substring", FunEntry {level=R.outermost, label=base_label, formals=[T.STRING,T.INT,T.INT], result=T.STRING}),
                      ("concat",    FunEntry {level=R.outermost, label=base_label, formals=[T.STRING,T.STRING], result=T.STRING}),
                      ("not",       FunEntry {level=R.outermost, label=base_label, formals=[T.INT], result=T.INT}),
                      ("exit",      FunEntry {level=R.outermost, label=base_label, formals=[T.INT], result=T.UNIT})]

  fun fillFuncFn ((name, entry), ans) = S.enter(ans, S.symbol name, entry)
  val base_venv = foldl fillFuncFn S.empty predef_funcs
end