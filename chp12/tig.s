	.globl malloc
	.text
sys_tig_malloc:
	# round up the requested amount to a multiple of 4
	add $a0, $a0, 3
	srl $a0, $a0, 2
	sll $a0, $a0, 2

	# allocate the memory with sbrk()
	li $v0, 9
	syscall

	j $ra

	.data
	.align 4
sys_tig_getchar_buf: .byte 0x0

	.text
sys_tig_getchar:
	# read the character
	la $a0, sys_tig_getchar_buf
	li $a1, 2
	li $v0, 8
	syscall

	# return it
	lb $v0, ($a0)
	j $ra

	.data
	.align 4
sys_tig_putchar_buf: .byte 0x0

	.text
sys_tig_putchar:
	# save the character so that it is NUL-terminated
	la $t0, sys_tig_putchar_buf
	sb $a0, ($t0)

	# print it out
	la $a0, sys_tig_putchar_buf
	li $v0, 4
	syscall

	j $ra

	.text
# just prints the format string, not the arguments
sys_tig_printf:
	li $v0, 4
	syscall
	j $ra

	.text
sys_tig_exit:
	li $v0, 10
	syscall

	.text
	.align	4
	.globl	tig_stringEqual
tig_stringEqual:
	beq	$4,$5,.L2
	lw	$8,0($4)
	lw	$2,0($5)
	beq	$8,$2,.L9
.L3:
	move	$2,$0
	j	$31
.L9:
	blez	$8,.L2
	lbu	$3,4($4)
	lbu	$2,4($5)
	bne	$3,$2,.L3
	b	.L4
	move	$2,$0
.L5:
	lbu	$7,4($7)
	lbu	$3,4($6)
	bne	$7,$3,.L3
.L4:
	addiu	$2,$2,1
	slt	$3,$2,$8
	addu	$7,$4,$2
	bne	$3,$0,.L5
	addu	$6,$5,$2
.L2:
	li	$2,1			# 0x1
	j	$31
	.align	4
	.globl	tig_strcmp
tig_strcmp:
	sltu	$2,$6,6
	beq	$2,$0,.L11
	sltu	$2,$6,2
	bne	$2,$0,.L24
	lw	$3,0($4)
	lw	$2,0($5)
	slt	$7,$2,$3
	beq	$7,$0,.L17
	move	$2,$3
.L17:
	blez	$2,.L11
	lbu	$2,4($4)
	lbu	$3,4($5)
	sltu	$2,$3,$2
	beq	$2,$0,.L25
	xori	$6,$6,0x4
	sltu	$2,$6,1
	j	$31
.L24:
	lw	$9,0($4)
	lw	$2,0($5)
	beq	$9,$2,.L26
.L11:
	move	$2,$0
	j	$31
.L25:
	xori	$6,$6,0x2
	sltu	$2,$6,1
	j	$31
.L26:
	blez	$9,.L13
	bne	$6,$0,.L27
	move	$2,$0
	addu	$7,$4,$2
.L28:
	addu	$3,$5,$2
	lbu	$7,4($7)
	lbu	$3,4($3)
	addiu	$2,$2,1
	bne	$7,$3,.L11
	slt	$8,$2,$9
	bne	$8,$0,.L28
	addu	$7,$4,$2
.L13:
	xori	$2,$6,0x1
	j	$31
.L27:
	lbu	$6,4($4)
	lbu	$2,4($5)
	xor	$2,$6,$2
	sltu	$2,$0,$2
	j	$31
	.align	4
	.globl	tig_ord
tig_ord:
	lw	$2,0($4)
	beq	$2,$0,.L33
	lbu	$2,4($4)
	j	$31
.L33:
	li	$2,-1			# 0xffffffffffffffff
	j	$31
	.align	4
	.globl	tig_size
tig_size:
	lw	$2,0($4)
	j	$31
	.align	4
	.globl	tig_not
tig_not:
	sltu	$2,$4,1
	j	$31
	.align	4
	.globl	tig_getchar
tig_getchar:
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	jal	sys_tig_getchar
	beq	$2,$0,.L42
	sll	$2,$2,3
	la	$3,consts
	lw	$31,28($sp)
	addu	$2,$3,$2
	addiu	$sp,$sp,32
	j	$31
.L42:
	lw	$31,28($sp)
	la	$2,empty
	addiu	$sp,$sp,32
	j	$31
	.align	4
	.globl	tig_concat
tig_concat:
	addiu	$sp,$sp,-48
	sw	$31,44($sp)
	sw	$17,40($sp)
	sw	$16,36($sp)
	lw	$2,0($4)
	beq	$2,$0,.L44
	move	$16,$4
	lw	$17,0($5)
	bne	$17,$0,.L52
	addu	$17,$17,$2
	move	$5,$4
.L44:
	lw	$31,44($sp)
	move	$2,$5
	lw	$17,40($sp)
	lw	$16,36($sp)
	addiu	$sp,$sp,48
	j	$31
.L52:
	addiu	$4,$17,4
	jal	sys_tig_malloc
	sw	$5,24($sp)
	sw	$17,0($2)
	lw	$4,0($16)
	lw	$5,24($sp)
	blez	$4,.L46
	move	$3,$0
.L47:
	addu	$6,$16,$3
	lbu	$8,4($6)
	addu	$7,$2,$3
	addiu	$3,$3,1
	slt	$6,$3,$4
	bne	$6,$0,.L47
	sb	$8,4($7)
.L46:
	lw	$9,0($5)
	blez	$9,.L48
	move	$3,$0
.L49:
	addu	$6,$5,$3
	lbu	$8,4($6)
	addu	$7,$2,$3
	addiu	$3,$3,1
	addu	$7,$7,$4
	slt	$6,$3,$9
	bne	$6,$0,.L49
	sb	$8,4($7)
.L48:
	lw	$31,44($sp)
	move	$5,$2
	move	$2,$5
	lw	$17,40($sp)
	lw	$16,36($sp)
	addiu	$sp,$sp,48
	j	$31
	.align	4
	.globl	tig_allocRecord
tig_allocRecord:
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$16,24($sp)
	jal	sys_tig_malloc
	move	$16,$4
	blez	$16,.L54
	move	$4,$2
	move	$3,$0
.L55:
	addiu	$3,$3,4
	slt	$5,$3,$16
	sw	$0,0($4)
	bne	$5,$0,.L55
	addiu	$4,$4,4
.L54:
	lw	$31,28($sp)
	lw	$16,24($sp)
	addiu	$sp,$sp,32
	j	$31
	.align	4
	.globl	tig_initArray
tig_initArray:
	addiu	$sp,$sp,-40
	sw	$31,36($sp)
	sw	$16,32($sp)
	sw	$5,24($sp)
	move	$16,$4
	jal	sys_tig_malloc
	sll	$4,$4,2
	lw	$5,24($sp)
	blez	$16,.L59
	move	$4,$2
	move	$3,$0
.L60:
	addiu	$3,$3,1
	sw	$5,0($4)
	bne	$3,$16,.L60
	addiu	$4,$4,4
.L59:
	lw	$31,36($sp)
	lw	$16,32($sp)
	addiu	$sp,$sp,40
	j	$31
.LC0:
	.data
	.align	4
	.ascii	"substring([%d],%d,%d) out of range\012\000"
	.text
	.align	4
	.globl	tig_substring
tig_substring:
	addiu	$sp,$sp,-40
	sw	$31,36($sp)
	sw	$18,32($sp)
	sw	$17,28($sp)
	sw	$16,24($sp)
	move	$17,$5
	move	$18,$4
	bltz	$5,.L81
	move	$16,$6
	lw	$5,0($4)
	addu	$2,$6,$17
	slt	$2,$5,$2
	bne	$2,$0,.L65
	li	$2,1			# 0x1
	beq	$16,$2,.L82
.L67:
	jal	sys_tig_malloc
	addiu	$4,$16,4
	move	$4,$2
	blez	$16,.L68
	sw	$16,0($2)
	addiu	$7,$17,4
	sltu	$3,$16,4
	addu	$7,$18,$7
	bne	$3,$0,.L69
	addiu	$5,$4,4
	andi	$3,$7,0x3
	bne	$3,$0,.L69
	addiu	$3,$7,4
	sltu	$5,$3,$5
	beq	$5,$0,.L83
	addiu	$3,$4,8
	srl	$10,$16,2
.L84:
	sll	$3,$10,2
	beq	$3,$0,.L72
	move	$6,$4
	move	$5,$0
.L73:
	lw	$9,0($7)
	addiu	$5,$5,1
	sltu	$8,$5,$10
	sw	$9,4($6)
	addiu	$7,$7,4
	bne	$8,$0,.L73
	addiu	$6,$6,4
	beq	$16,$3,.L68
.L72:
	addu	$17,$3,$17
	addu	$18,$18,$17
	addu	$4,$4,$3
	addiu	$18,$18,4
	addiu	$4,$4,4
.L74:
	lbu	$6,0($18)
	addiu	$3,$3,1
	slt	$5,$3,$16
	sb	$6,0($4)
	addiu	$18,$18,1
	bne	$5,$0,.L74
	addiu	$4,$4,1
.L68:
	lw	$31,36($sp)
	lw	$18,32($sp)
	lw	$17,28($sp)
	lw	$16,24($sp)
	addiu	$sp,$sp,40
	j	$31
.L81:
	lw	$5,0($4)
.L65:
	la	$4,.LC0
	move	$6,$17
	jal	sys_tig_printf
	move	$7,$16
	jal	sys_tig_exit
	li	$4,1			# 0x1
	li	$2,1			# 0x1
	bne	$16,$2,.L67
.L82:
	addu	$17,$18,$17
	lbu	$2,4($17)
	la	$3,consts
	lw	$31,36($sp)
	sll	$2,$2,3
	addu	$2,$3,$2
	lw	$18,32($sp)
	lw	$17,28($sp)
	lw	$16,24($sp)
	addiu	$sp,$sp,40
	j	$31
.L83:
	sltu	$3,$3,$7
	bne	$3,$0,.L84
	srl	$10,$16,2
.L69:
	addu	$17,$18,$17
	addiu	$17,$17,4
	move	$3,$0
.L75:
	lbu	$6,0($17)
	addu	$5,$4,$3
	addiu	$3,$3,1
	sb	$6,4($5)
	bne	$3,$16,.L75
	addiu	$17,$17,1
	lw	$31,36($sp)
	lw	$18,32($sp)
	lw	$17,28($sp)
	lw	$16,24($sp)
	addiu	$sp,$sp,40
	j	$31
.LC1:
	.data
	.align	4
	.ascii	"chr(%d) out of range\012\000"
	.text
	.align	4
	.globl	tig_chr
tig_chr:
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$16,24($sp)
	sltu	$2,$4,256
	bne	$2,$0,.L86
	move	$16,$4
	la	$4,.LC1
	jal	sys_tig_printf
	move	$5,$16
	li	$4,1			# 0x1
	jal	sys_tig_exit
.L86:
	la	$2,consts
	lw	$31,28($sp)
	sll	$16,$16,3
	addu	$2,$2,$16
	lw	$16,24($sp)
	addiu	$sp,$sp,32
	j	$31
	.align	4
	.globl	main
main:
	la	$3,consts
	move	$2,$0
	li	$5,1			# 0x1
	li	$4,256			# 0x100
.L89:
	sb	$2,4($3)
	addiu	$2,$2,1
	sw	$5,0($3)
	addiu	$3,$3,8
	bne	$2,$4,.L89
	jal	tig_main
	move	$4,$0
	.align	4
	.globl	tig_flush
tig_flush:
	jr	$31
	.align	4
	.globl	tig_print
tig_print:
	addiu	$sp,$sp,-40
	sw	$31,36($sp)
	sw	$18,32($sp)
	sw	$17,28($sp)
	sw	$16,24($sp)
	lw	$2,0($4)
	blez	$2,.L97
	move	$18,$4
	addiu	$17,$4,4
	move	$16,$0
.L96:
	lbu	$4,0($17)
	jal	sys_tig_putchar
	addiu	$16,$16,1
	lw	$2,0($18)
	slt	$2,$16,$2
	bne	$2,$0,.L96
	addiu	$17,$17,1
.L97:
	lw	$31,36($sp)
	lw	$18,32($sp)
	lw	$17,28($sp)
	lw	$16,24($sp)
	addiu	$sp,$sp,40
	j	$31
	.globl	consts
	.data
	.align 4
consts:
	.word	0
	.byte	0x0
	.space	3
	.space	2040
	.globl	empty
	.align	4
empty:
	.word	0
	.byte	0x0
	.space	3
.text
tig_main:
addiu   $sp, $sp, -32
sw      $fp, 28($sp)
sw      $ra, 20($sp)
addiu   $fp, $sp, 32
L81:
sw      $s0, -12($sp)
sw      $s1, -16($sp)
addi    $s0, $fp, -8
jal     tig_getchar
sw      $v0, 0($s0)
move    $a0, $fp
jal     L25
move    $s1, $v0
addi    $s0, $fp, -8
jal     tig_getchar
sw      $v0, 0($s0)
move    $a0, $fp
jal     L25
move    $a0, $fp
move    $a1, $s1
move    $a2, $v0
jal     L26
move    $a0, $fp
move    $a1, $v0
jal     L28
lw      $s0, -12($sp)
lw      $s1, -16($sp)
j       L80
L80:
lw      $ra, 20($sp)
lw      $fp, 28($sp)
addiu   $sp, $sp, 32
jr      $ra
L28:
addiu   $sp, $sp, -28
sw      $fp, 24($sp)
sw      $ra, 16($sp)
addiu   $fp, $sp, 28
L83:
sw      $a0, -8($fp)
sw      $a1, -16($sp)
lw      $a1, -16($sp)
addi    $a0, $a1, 0
beqz    $a0, L77
j       L78
L78:
lw      $a0, -8($fp)
lw      $a1, -16($sp)
sub     $a1, $0, $a1
beqz    $a1, L71
j       L72
L72:
lw      $a1, -16($sp)
add     $a1, $a1, $0
lw      $a1, 0($a1)
jal     L27
la      $a0, L10
jal     tig_print
lw      $a0, -8($fp)
lw      $a1, -16($sp)
sub     $a1, $0, $a1
beqz    $a1, L75
j       L76
L76:
li      $a2, 4
lw      $a1, -16($sp)
add     $a1, $a1, $a2
lw      $a1, 0($a1)
jal     L28
L79:
j       L82
L77:
la      $a0, L11
jal     tig_print
j       L79
L71:
li      $v0, 10
syscall
j       L72
L75:
li      $v0, 10
syscall
j       L76
L82:
lw      $ra, 16($sp)
lw      $fp, 24($sp)
addiu   $sp, $sp, 28
jr      $ra
L27:
addiu   $sp, $sp, -28
sw      $fp, 24($sp)
sw      $ra, 16($sp)
addiu   $fp, $sp, 28
L85:
sw      $a0, -8($fp)
sw      $a1, -16($sp)
lw      $a1, -16($sp)
addi    $a0, $a1, 0
bltz    $a0, L66
j       L67
L67:
lw      $a1, -16($sp)
addi    $a0, $a1, 0
bgtz    $a0, L63
j       L64
L64:
la      $a0, L4
jal     tig_print
L65:
L68:
j       L84
L66:
la      $a0, L62
jal     tig_print
move    $a0, $fp
lw      $a1, -16($sp)
sub     $a1, $0, $a1
jal     L59
j       L68
L63:
move    $a0, $fp
lw      $a1, -16($sp)
jal     L59
j       L65
L84:
lw      $ra, 16($sp)
lw      $fp, 24($sp)
addiu   $sp, $sp, 28
jr      $ra
L59:
addiu   $sp, $sp, -32
sw      $fp, 28($sp)
sw      $ra, 20($sp)
addiu   $fp, $sp, 32
L87:
sw      $a0, -8($fp)
sw      $a1, -20($sp)
sw      $s0, -16($sp)
lw      $a1, -20($sp)
addi    $a0, $a1, 0
bgtz    $a0, L61
j       L60
L60:
lw      $s0, -16($sp)
j       L86
L61:
lw      $a0, -8($fp)
li      $a2, 10
lw      $a1, -20($sp)
div     $a1, $a1, $a2
jal     L59
li      $a0, 10
lw      $a1, -20($sp)
div     $a1, $a1, $a0
li      $a0, 10
mul     $a0, $a1, $a0
lw      $a1, -20($sp)
sub     $s0, $a1, $a0
la      $a0, L4
jal     tig_ord
add     $a0, $s0, $v0
jal     tig_chr
move    $a0, $v0
jal     tig_print
j       L60
L86:
lw      $ra, 20($sp)
lw      $fp, 28($sp)
addiu   $sp, $sp, 32
jr      $ra
L26:
addiu   $sp, $sp, -44
sw      $fp, 40($sp)
sw      $ra, 32($sp)
addiu   $fp, $sp, 44
L89:
sw      $a0, -8($fp)
sw      $a1, -28($sp)
sw      $a2, -24($sp)
sw      $s0, -16($sp)
sw      $s1, -20($sp)
lw      $a1, -28($sp)
addi    $a0, $a1, 0
beqz    $a0, L56
j       L57
L57:
lw      $a2, -24($sp)
addi    $a0, $a2, 0
beqz    $a0, L53
j       L54
L54:
lw      $a1, -28($sp)
sub     $a0, $0, $a1
beqz    $a0, L34
j       L35
L35:
lw      $a1, -28($sp)
add     $a0, $a1, $0
lw      $a1, 0($a0)
lw      $a2, -24($sp)
sub     $a0, $0, $a2
beqz    $a0, L36
j       L37
L37:
lw      $a2, -24($sp)
add     $a0, $a2, $0
lw      $a0, 0($a0)
sub     $a0, $a1, $a0
bltz    $a0, L50
j       L51
L51:
li      $a0, 8
jal     tig_allocRecord
move    $s1, $v0
addi    $a1, $s1, 0
lw      $a2, -24($sp)
sub     $a0, $0, $a2
beqz    $a0, L44
j       L45
L45:
lw      $a2, -24($sp)
add     $a0, $a2, $0
sw      $a0, 0($a1)
addi    $s0, $s1, 4
lw      $a0, -8($fp)
lw      $a1, -28($sp)
lw      $a2, -24($sp)
sub     $a2, $0, $a2
beqz    $a2, L48
j       L49
L49:
li      $a3, 4
lw      $a2, -24($sp)
add     $a2, $a2, $a3
lw      $a2, 0($a2)
jal     L26
sw      $v0, 0($s0)
L52:
move    $a1, $s1
L55:
move    $v0, $a1
L58:
lw      $s0, -16($sp)
lw      $s1, -20($sp)
j       L88
L56:
lw      $a2, -24($sp)
move    $v0, $a2
j       L58
L53:
lw      $a1, -28($sp)
j       L55
L34:
li      $v0, 10
syscall
j       L35
L36:
li      $v0, 10
syscall
j       L37
L50:
li      $a0, 8
jal     tig_allocRecord
move    $s1, $v0
addi    $a2, $s1, 0
lw      $a1, -28($sp)
sub     $a0, $0, $a1
beqz    $a0, L38
j       L39
L39:
lw      $a1, -28($sp)
add     $a0, $a1, $0
sw      $a0, 0($a2)
addi    $s0, $s1, 4
lw      $a0, -8($fp)
lw      $a1, -28($sp)
sub     $a1, $0, $a1
beqz    $a1, L42
j       L43
L43:
li      $a2, 4
lw      $a1, -28($sp)
add     $a1, $a1, $a2
lw      $a1, 0($a1)
lw      $a2, -24($sp)
jal     L26
sw      $v0, 0($s0)
j       L52
L38:
li      $v0, 10
syscall
j       L39
L42:
li      $v0, 10
syscall
j       L43
L44:
li      $v0, 10
syscall
j       L45
L48:
li      $v0, 10
syscall
j       L49
L88:
lw      $ra, 32($sp)
lw      $fp, 40($sp)
addiu   $sp, $sp, 44
jr      $ra
L25:
addiu   $sp, $sp, -36
sw      $fp, 32($sp)
sw      $ra, 24($sp)
addiu   $fp, $sp, 36
L91:
sw      $a0, -8($fp)
sw      $s0, -16($sp)
sw      $s1, -20($sp)
sw      $s2, -24($sp)
li      $a0, 4
jal     tig_allocRecord
sw      $0, 0($v0)
move    $s0, $v0
lw      $a0, -8($fp)
move    $a1, $s0
jal     L1
move    $s2, $v0
sub     $a0, $0, $s0
beqz    $a0, L29
j       L30
L30:
add     $a0, $s0, $0
lw      $a0, 0($a0)
sub     $a0, $0, $a0
bnez    $a0, L31
j       L32
L32:
li      $v0, 0
L33:
lw      $s0, -16($sp)
lw      $s1, -20($sp)
lw      $s2, -24($sp)
j       L90
L29:
li      $v0, 10
syscall
j       L30
L31:
li      $a0, 8
jal     tig_allocRecord
move    $s1, $v0
sw      $s2, 0($s1)
addi    $s0, $s1, 4
lw      $a0, -8($fp)
jal     L25
sw      $v0, 0($s0)
move    $v0, $s1
j       L33
L90:
lw      $ra, 24($sp)
lw      $fp, 32($sp)
addiu   $sp, $sp, 36
jr      $ra
L1:
addiu   $sp, $sp, -36
sw      $fp, 32($sp)
sw      $ra, 24($sp)
addiu   $fp, $sp, 36
L93:
sw      $a0, -8($fp)
sw      $a1, -24($sp)
sw      $s0, -16($sp)
sw      $s1, -20($sp)
li      $s1, 0
move    $a0, $fp
jal     L3
lw      $a1, -24($sp)
sub     $a0, $0, $a1
beqz    $a0, L20
j       L21
L21:
lw      $a1, -24($sp)
add     $s0, $a1, $0
move    $a0, $fp
lw      $a1, -8($fp)
lw      $a1, -8($a1)
jal     L2
sw      $v0, 0($s0)
L23:
move    $a0, $fp
lw      $a1, -8($fp)
lw      $a1, -8($a1)
jal     L2
sub     $a0, $0, $v0
bnez    $a0, L24
j       L22
L22:
move    $v0, $s1
lw      $s0, -16($sp)
lw      $s1, -20($sp)
j       L92
L20:
li      $v0, 10
syscall
j       L21
L24:
li      $a0, 10
mul     $s0, $s1, $a0
lw      $a0, -8($fp)
lw      $a0, -8($a0)
jal     tig_ord
add     $s0, $s0, $v0
la      $a0, L4
jal     tig_ord
sub     $s1, $s0, $v0
lw      $a0, -8($fp)
addi    $s0, $a0, -8
jal     tig_getchar
sw      $v0, 0($s0)
j       L23
L92:
lw      $ra, 24($sp)
lw      $fp, 32($sp)
addiu   $sp, $sp, 36
jr      $ra
L3:
addiu   $sp, $sp, -32
sw      $fp, 28($sp)
sw      $ra, 20($sp)
addiu   $fp, $sp, 32
L95:
sw      $a0, -8($fp)
sw      $s0, -16($sp)
L18:
lw      $a0, -8($fp)
lw      $a0, -8($a0)
lw      $a0, -8($a0)
la      $a1, L10
li      $a2, 0
jal     tig_strcmp
sub     $a0, $0, $v0
bnez    $a0, L12
j       L13
L13:
lw      $a0, -8($fp)
lw      $a0, -8($a0)
lw      $a0, -8($a0)
la      $a1, L11
li      $a2, 0
jal     tig_strcmp
L14:
sub     $a0, $0, $v0
bnez    $a0, L19
j       L9
L9:
li      $v0, 0
lw      $s0, -16($sp)
j       L94
L12:
li      $v0, 1
j       L14
L19:
lw      $a0, -8($fp)
lw      $a0, -8($a0)
addi    $s0, $a0, -8
jal     tig_getchar
sw      $v0, 0($s0)
j       L18
L94:
lw      $ra, 20($sp)
lw      $fp, 28($sp)
addiu   $sp, $sp, 32
jr      $ra
L2:
addiu   $sp, $sp, -12
sw      $fp, 8($sp)
addiu   $fp, $sp, 12
L97:
sw      $a0, -8($fp)
li      $v0, 0
j       L96
L96:
lw      $fp, 8($sp)
addiu   $sp, $sp, 12
jr      $ra
.data
L62:
 .align 4
 .word  1
 .align 4
 .ascii "-"
L11:
 .align 4
 .word  1
 .align 4
 .ascii "
"
L10:
 .align 4
 .word  1
 .align 4
 .ascii " "
L5:
 .align 4
 .word  1
 .align 4
 .ascii "9"
L4:
 .align 4
 .word  1
 .align 4
 .ascii "0"
