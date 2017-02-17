Shift Reduce Conflicts:

1) state 20: shift/reduce conflict (shift LBRACK, reduce by rule 66)

funcall : ID . LPAREN expcomma RPAREN 
lvalue : ID .  (reduce by rule 66)
lvalue : ID . LBRACK exp RBRACK 
reccreation : ID . LBRACE idassigns RBRACE 
arrcreation : ID . LBRACK exp RBRACK OF exp 

We note that shift/reduce conflicts are resolved by ML-YACC through a shift by default. In state 20, the shift reduce conflict occurs because an identifier can be reduce to an lvalue, or we can shift an LBRACK, which would indicate an the code trying to access some index in an array. Square brackets (LBRACK and RBRACK) do not appear in places other than array expressions (in other words, these brackets will always have an identifier occur right before). Therefore, we can safely shift in this case because we know that an identifier followed by an LBRACK will definitely be some sort of array operation.

2) state 36: shift/reduce conflict (shift FUNCTION, reduce by rule 57)

fundeclist : fundec .  (reduce by rule 57)
fundeclist : fundec . fundeclist 

In this case, the shift/reduce conflict occurs because there is a chance that there are multiple function declarations in a row. One example of this would be mutually recursive function declarations. In this case, we can safely shift because multiple function declarations in a row can safely be grouped. If the function declarations are independent, then it makes no difference that they are grouped. If the function declarations are mutually recursive, then reducing would be the wrong choice. We should shift instead because we want to group function declarations that are interdependent.

3) state 104: shift/reduce conflict (shift TYPE, reduce by rule 53)

tydeclist : TYPE ID EQ ty .  (reduce by rule 53)
tydeclist : TYPE ID EQ ty . tydeclist 

The reasoning behind why this shift/reduce conflict is not harmful is the same as the previous conflict. If the type declarations are independent of each other, there is no harm in grouping them. If they are dependent on each other, reducing would be problematic because then they would not be grouped together, and therefore we should always shift in this case.



Explanation For Precedence Directives:

%right    OF

This was inserted because Tiger can have expressions for things like 3D arrays. For example, in the case of "array of array of array", this should refer to the type array of (array of array). If %left was issued, it would reduce first, and instead be recognized as (array of array) of array, which doesn't make sense.

%nonassoc DO
%nonassoc THEN
%nonassoc ELSE
%nonassoc ASSIGN

The parser should never have to determine associativity for these expressions because they should not be able to appear consecutively.

%left     OR
%left     AND
%nonassoc EQ NEQ LT LE GT GE
%left     PLUS MINUS
%left     TIMES DIVIDE
%left     UMINUS

All of these terminals are left-associative. Furthermore, they are listed top-to-bottom from least priority to most priority.



Other Important Notes:

1) We have many pairs of non-terminals of the form THING and THING_some. This is to differentiate between empty and non-empty cases (THING can be empty, THING_some is forced to be non-empty).

2) All other non-terminals should be fairly self-explanatory.

