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
