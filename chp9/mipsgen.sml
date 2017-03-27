structure MipsGen :> CODEGEN =
struct
  structure A = Assem
  structure F = MipsFrame
  structure T = Tree

  fun codegen frame stm =
    let
      val instrlist = ref (nil: A.instr list)
      fun emit x = instrlist := x :: !instrlist
      fun result(gen) =
        let
          val t = Temp.newtemp()
        in
           gen t; t
        end

      fun munchExp(T.CONST i) =
            result(fn r => emit(A.OPER {assem="addi    $d0, $r0, " ^ Int.toString i ^ "\n",
                                        src=[],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="addi    $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, T.CONST i, e)) =
            result(fn r => emit(A.OPER {assem="addi    $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, e1, e2)) =
            result(fn r => emit(A.OPER {assem="add     $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MINUS, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="addi    $d0, $s0, " ^ (Int.toString (~i)) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MINUS, e1, e2)) =
            result(fn r => emit(A.OPER {assem="sub     $d0, $s0, $s1\n",
                                        src=[munchExp e1,  munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MUL, e1, e2)) =
            result(fn r => emit(A.OPER {assem="mul     $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.DIV, e1, e2)) =
            result(fn r => emit(A.OPER {assem="div     $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.AND, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="andi    $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.AND, T.CONST i, e)) =
            result(fn r => emit(A.OPER {assem="andi    $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.AND, e1, e2)) =
            result(fn r => emit(A.OPER {assem="and     $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.OR, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="ori     $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.OR, T.CONST i, e)) =
            result(fn r => emit(A.OPER {assem="ori     $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.OR, e1, e2)) =
            result(fn r => emit(A.OPER {assem="or      $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.LSHIFT, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="sll     $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.LSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER {assem="sllv    $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.RSHIFT, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="srl     $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.RSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER {assem="srlv    $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.ARSHIFT, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="sra     $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.ARSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER {assem="srav    $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.XOR, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="xori    $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.XOR, T.CONST i, e)) =
            result(fn r => emit(A.OPER {assem="xori    $d0, $s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.XOR, e1, e2)) =
            result(fn r => emit(A.OPER {assem="xor     $d0, $s0, $s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.NAME l) =
            result(fn r => emit(A.OPER {assem="la      $d0, " ^ (Temp.labelToString l) ^ "\n",
                                        src=[],
                                        dst=[r], jump=NONE}))
        | munchExp (T.MEM (T.BINOP(T.PLUS, e, T.CONST i))) =
            result(fn r => emit(A.OPER {assem="lw      $d0, " ^ (Int.toString i) ^ "($s0)",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.MEM (T.BINOP(T.PLUS, T.CONST i, e))) =
            result(fn r => emit(A.OPER {assem="lw      $d0, " ^ (Int.toString i) ^ "($s0)",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.MEM (T.BINOP(T.MINUS, e, T.CONST i))) =
            result(fn r => emit(A.OPER {assem="lw      $d0, " ^ (Int.toString (~i)) ^ "($s0)",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.MEM e) =
            result(fn r => emit(A.OPER {assem="lw      $d0, 0($s0)",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.TEMP t) = t
        | munchExp (T.ESEQ(s, e)) = (munchStm s; munchExp e)

      and munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
            emit(A.OPER {assem="sw      $s0, " ^ (Int.toString i) ^ "($s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
            emit(A.OPER {assem="sw      $s0, " ^ (Int.toString i) ^ "($s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i)), e2)) =
            emit(A.OPER {assem="sw      $s0, " ^ (Int.toString (~i)) ^ "($s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE}) (* Minus isn't associativy, so no inverse. *)
        | munchStm (T.MOVE(T.MEM(e1), T.MEM(e2))) =
            emit(A.OPER {assem="sw      $s0, 0($s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(e1), e2)) =
            emit(A.OPER {assem="sw      $s0, 0($s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE})
        | munchStm (T.LABEL l) =
            emit(A.LABEL {assem=(Temp.labelToString l) ^ ":\n",
                          lab=l})
        | munchStm (T.ERROR e) =
            emit(A.OPER {assem="addi $v0, $r0, 10\n syscall",
                         src=[],
                         dst=[], jump=NONE}) (* TODO Print error message*)
    in
      munchStm stm;
      List.rev (!instrlist)
    end

  (*fun codegen f t =*)
end

