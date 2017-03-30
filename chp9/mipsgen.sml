structure MipsGen : CODEGEN =
struct
  structure A = Assem
  structure F = MipsFrame
  structure T = Tree

  type frame = F.frame

  fun codegen frame stm =
    let
      val instrlist = ref (nil: A.instr list)

      val err = ErrorMsg.error

      fun emit x = instrlist := x :: !instrlist

      fun relToString T.EQ = "beqz"
        | relToString T.NE = "bnez"
        | relToString T.LT = "bltz"
        | relToString T.GT = "bgtz"
        | relToString T.LE = "blez"
        | relToString T.GE = "bgez"
        | relToString _    = (ErrorMsg.error 0 "Internal error - unnecessary relop to string."; "")
        (* There are no unsigned branch instructions in MIPS. *)

      fun result(gen) =
        let
          val t = Temp.newtemp()
        in
           gen t; t
        end

      fun munchError() =
            emit(A.OPER {assem="addi    `d0, `s0, 10\n syscall\n",
                         src=[F.r0],
                         dst=[F.rv], jump=NONE}) (* TODO Print error message*)
      and munchExp(T.CONST i) =
            result(fn r => emit(A.OPER {assem="addi    `d0, `s0, " ^ Int.toString i ^ "\n",
                                        src=[F.r0],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="addi    `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, T.CONST i, e)) =
            result(fn r => emit(A.OPER {assem="addi    `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, e1, e2)) =
            result(fn r => emit(A.OPER {assem="add     `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MINUS, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="addi    `d0, `s0, " ^ (Int.toString (~i)) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MINUS, e1, e2)) =
            result(fn r => emit(A.OPER {assem="sub     `d0, `s0, `s1\n",
                                        src=[munchExp e1,  munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MUL, e1, e2)) =
            result(fn r => emit(A.OPER {assem="mul     `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.DIV, e1, e2)) =
            result(fn r => emit(A.OPER {assem="div     `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.AND, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="andi    `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.AND, T.CONST i, e)) =
            result(fn r => emit(A.OPER {assem="andi    `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.AND, e1, e2)) =
            result(fn r => emit(A.OPER {assem="and     `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.OR, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="ori     `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.OR, T.CONST i, e)) =
            result(fn r => emit(A.OPER {assem="ori     `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.OR, e1, e2)) =
            result(fn r => emit(A.OPER {assem="or      `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.LSHIFT, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="sll     `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.LSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER {assem="sllv    `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.RSHIFT, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="srl     `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.RSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER {assem="srlv    `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.ARSHIFT, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="sra     `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.ARSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER {assem="srav    `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.XOR, e, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="xori    `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.XOR, T.CONST i, e)) =
            result(fn r => emit(A.OPER {assem="xori    `d0, `s0, " ^ (Int.toString i) ^ "\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.XOR, e1, e2)) =
            result(fn r => emit(A.OPER {assem="xor     `d0, `s0, `s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r], jump=NONE}))
        | munchExp (T.NAME l) =
            result(fn r => emit(A.OPER {assem="la      `d0, " ^ (Temp.labelToString l) ^ "\n",
                                        src=[],
                                        dst=[r], jump=NONE}))
        | munchExp (T.MEM (T.BINOP(T.PLUS, e, T.CONST i))) =
            result(fn r => emit(A.OPER {assem="lw      `d0, " ^ (Int.toString i) ^ "(`s0)\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.MEM (T.BINOP(T.PLUS, T.CONST i, e))) =
            result(fn r => emit(A.OPER {assem="lw      `d0, " ^ (Int.toString i) ^ "(`s0)\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.MEM (T.BINOP(T.MINUS, e, T.CONST i))) =
            result(fn r => emit(A.OPER {assem="lw      `d0, " ^ (Int.toString (~i)) ^ "(`s0)\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.MEM e) =
            result(fn r => emit(A.OPER {assem="lw      `d0, 0(`s0)\n",
                                        src=[munchExp e],
                                        dst=[r], jump=NONE}))
        | munchExp (T.TEMP t) = t
        | munchExp (T.ESEQ(s, e)) = (munchStm s; munchExp e)
        | munchExp (T.CALL(T.NAME n, args)) =
            (emit(A.OPER {
                  assem = "jal     " ^ (Temp.labelToString n) ^ "\n",
                  src = munchArgs(0, args, 16),
                  dst = F.ra::F.rv::(F.getRegTemps F.calleeRegs),
                  jump = NONE});
             F.rv)
        | munchExp (T.CALL(_, _)) = (err 0 "Please supply NAME to T.CALL."; Temp.newtemp())
      and munchStm (T.SEQ(e1, e2)) = (munchStm e1; munchStm e2)
        | munchStm (T.EXP(e)) = (munchExp e; ())
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
            emit(A.OPER {assem="sw      `s0, " ^ (Int.toString i) ^ "(`s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
            emit(A.OPER {assem="sw      `s0, " ^ (Int.toString i) ^ "(`s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i)), e2)) =
            emit(A.OPER {assem="sw      `s0, " ^ (Int.toString (~i)) ^ "(`s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE}) (* Minus isn't associativy, so no inverse. *)
        | munchStm (T.MOVE(T.MEM(e1), T.MEM(e2))) =
            emit(A.OPER {assem="sw      `s0, 0(`s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(e1), e2)) =
            emit(A.OPER {assem="sw      `s0, 0(`s1)\n",
                         src=[munchExp e2, munchExp e1],
                         dst=[], jump=NONE})
        | munchStm (T.MOVE(T.TEMP t, e)) =
            emit(A.OPER {assem="move    `d0, `s0\n",
                         src=[munchExp e],
                         dst=[t], jump=NONE})
        | munchStm (T.MOVE(e1, e2)) =
            munchError ()
        | munchStm (T.LABEL l) =
            emit(A.LABEL {assem=(Temp.labelToString l) ^ ":\n",
                          lab=l})
        | munchStm (T.JUMP(T.NAME(l), ll)) =
            emit(A.OPER {assem="j       " ^ (Temp.labelToString l) ^ "\n",
                         src=[],
                         dst=[], jump=SOME(ll)})
        | munchStm (T.JUMP(e, ll)) =
            emit(A.OPER {assem="jr      `s0\n",
                         src=[munchExp e],
                         dst=[], jump=SOME(ll)})
        | munchStm (T.CJUMP(T.ULT, e, T.CONST i, l1, l2)) =
            let
              val t = Temp.newtemp ()
            in
              emit (A.OPER {assem="sltiu   `d0, `s0, " ^ (Int.toString i),
                             src=[munchExp e],
                             dst=[t], jump=NONE});
              munchStm(T.CJUMP(T.NE, T.TEMP t, T.CONST 0, l1, l2))
            end
        | munchStm (T.CJUMP(T.ULT, e1, e2, l1, l2)) =
            let
              val t = Temp.newtemp ()
            in
              emit (A.OPER {assem="sltu    `d0, `s0, `s1",
                            src=[munchExp e1, munchExp e2],
                            dst=[t], jump=NONE});
              munchStm(T.CJUMP(T.NE, T.TEMP t, T.CONST 0, l1, l2))
            end
        | munchStm (T.CJUMP(T.ULE, e, T.CONST i, l1, l2)) =
            let
              val l3 = Temp.newlabel ()
            in
              munchStm(T.CJUMP (T.ULT, e, T.CONST i, l1, l3));
              munchStm(T.LABEL l3);
              munchStm(T.CJUMP (T.EQ, e, T.CONST i, l1, l2))
            end
        | munchStm (T.CJUMP(T.ULE, e1, e2, l1, l2)) =
            let
              val l3 = Temp.newlabel ()
            in
              munchStm(T.CJUMP (T.ULT, e1, e2, l1, l3));
              munchStm(T.LABEL l3);
              munchStm(T.CJUMP (T.EQ, e1, e2, l1, l2))
            end
        | munchStm (T.CJUMP(T.UGT, e1, e2, l1, l2)) =
            let
              val l3 = Temp.newlabel ()
            in
              munchStm(T.CJUMP (T.ULT, e1, e2, l2, l3));
              munchStm(T.LABEL l3);
              munchStm(T.CJUMP (T.EQ, e1, e2, l2, l1))
            end
        | munchStm (T.CJUMP(T.UGE, e1, e2, l1, l2)) =
            munchStm(T.CJUMP (T.ULT, e1, e2, l2, l1))
        | munchStm (T.CJUMP(rel, e1, e2, l1, l2)) =
            let
              val t = Temp.newtemp()
            in
              emit(A.OPER {assem="sub     `d0, `s0, `s1\n"
                          ^ (relToString rel) ^ "    `s2, `s3, `j0\nb `j1\n" ,
                         src=[munchExp e1, munchExp e2, t, F.r0],
                         dst=[t], jump=SOME([l1, l2])})
            end
            
        | munchStm (T.ERROR e) =
            munchError ()
      and munchArgs(i, [], offset) = []
        | munchArgs(i, arg::l, offset) =
            if i < 4
            then
              let
                val temp = List.nth(F.getRegTemps F.argsRegs, i)
              in
                munchStm(T.MOVE(T.TEMP temp, arg));
                temp::munchArgs(i + 1, l, offset)
              end
            else
                (munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP F.sp, T.CONST offset)), arg));
                 munchArgs(i + 1, l, offset + 4))
    in
      munchStm stm;
      List.rev (!instrlist)
    end

  (*fun codegen f t =*)
end

