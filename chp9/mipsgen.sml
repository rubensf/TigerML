structure MipsGen :> CODEGEN =
struct
  structure A = Assem
  structure F = MipsFrame
  structure T = Tree

  fun emit a = ()

  fun munchExp x = Temp.newtemp()

  fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
    | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
        emit(A.OPER{assem="sw      $s0, " ^ (Int.toString i) ^ "($s1)",
                    src=[munchExp e2, munchExp e1],
                    dst=[], jump=NONE})
    | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
        emit(A.OPER{assem="sw      $s0, " ^ (Int.toString i) ^ "($s1)",
                    src=[munchExp e2, munchExp e1],
                    dst=[], jump=NONE})
    | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i)), e2)) =
        emit(A.OPER{assem="sw      $s0, -" ^ (Int.toString i) ^ "($s1)",
                    src=[munchExp e2, munchExp e1],
                    dst=[], jump=NONE}) (* Minus isn't associativy, so no inverse. *)
    | munchStm (T.MOVE(T.MEM(e1), T.MEM(e2))) =
        emit(A.OPER{assem="sw      $s0, 0($s1)",
                    src=[munchExp e2, munchExp e1],
                    dst=[], jump=NONE}) (* Minus isn't associativy, so no inverse. *)



  (*fun codegen f t =*)
end

