structure Temp :> TEMP =
struct
  type temp = int
  type label = Symbol.symbol

  val temps = ref 100

  structure Table = IntMapTable(type key = int
        fun getInt n = n)

  fun newtemp() = let val t = !temps in temps := t+1; t end
  fun makestring t = "t" ^ Int.toString t
  fun namedlabel = Symbol.symbol

  local
    structure F = Format
    fun postinc x = let val i = !x in x := i+1; i end
    val labs = ref 0
  in
    fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
  end
end
