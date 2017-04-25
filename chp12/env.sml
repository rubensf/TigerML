signature ENV =
sig
  type access
  type level
  type ty = Types.ty

  datatype enventry = VarEntry of {access: access, ty: ty}
                    | FunEntry of {level: level,
                                   label: Temp.label,
                                   parameters: ty list, result: ty}

  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

functor Env(R: TRANSLATE) :> ENV where type access = R.access
                                   and type level = R.level =
struct
  structure S = Symbol
  structure T = Types

  type access = R.access
  type level = R.level
  type ty = T.ty

  datatype enventry = VarEntry of {access: access, ty: ty}
                    | FunEntry of {level: level,
                                   label: Temp.label,
                                   parameters: ty list, result: ty}

  val predef_types = [("int", T.INT),
                      ("string", T.STRING)]

  fun fillTypeFn ((name, ty), ans) = S.enter(ans, S.symbol name, ty)
  val base_tenv = foldl fillTypeFn S.empty predef_types

  val base_label = Temp.newlabel ()
  val predef_funcs = [("print",     FunEntry {level=R.outermost, label=base_label, parameters=[T.STRING], result=T.UNIT}),
                      ("flush",     FunEntry {level=R.outermost, label=base_label, parameters=[], result=T.UNIT}),
                      ("getchr",    FunEntry {level=R.outermost, label=base_label, parameters=[], result=T.STRING}),
                      ("ord",       FunEntry {level=R.outermost, label=base_label, parameters=[T.STRING], result=T.INT}),
                      ("chr",       FunEntry {level=R.outermost, label=base_label, parameters=[T.INT], result=T.STRING}),
                      ("size",      FunEntry {level=R.outermost, label=base_label, parameters=[T.STRING], result=T.INT}),
                      ("substring", FunEntry {level=R.outermost, label=base_label, parameters=[T.STRING,T.INT,T.INT], result=T.STRING}),
                      ("concat",    FunEntry {level=R.outermost, label=base_label, parameters=[T.STRING,T.STRING], result=T.STRING}),
                      ("not",       FunEntry {level=R.outermost, label=base_label, parameters=[T.INT], result=T.INT}),
                      ("exit",      FunEntry {level=R.outermost, label=base_label, parameters=[T.INT], result=T.UNIT})]

  fun fillFuncFn ((name, entry), ans) = S.enter(ans, S.symbol name, entry)
  val base_venv = foldl fillFuncFn S.empty predef_funcs
end
