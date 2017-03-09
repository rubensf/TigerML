structure Types =
struct
  type unique = unit ref

  datatype ty =
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	  			| NAME of Symbol.symbol * ty option ref
					| UNIT

	fun toString t = case t of
		                 RECORD r    => "Record"
		               | NIL         => "Nil"
		               | INT         => "Int"
		               | STRING      => "String"
		               | ARRAY (p,_) => "Array of " ^ toString(p)
		               | NAME   n    => "Name"
		               | UNIT        => "Unit"
end
