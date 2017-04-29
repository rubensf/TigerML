	.text
	.align	4
	.globl	tig_stringEqual
tig_stringEqual:
	beq	$4,$5,.L2
	nop
	lw	$8,0($4)
	lw	$2,0($5)
	nop
	beq	$8,$2,.L9
	nop
.L3:
	move	$2,$0
	j	$31
.L9:
	blez	$8,.L2
	nop
	lbu	$3,4($4)
	lbu	$2,4($5)
	nop
	bne	$3,$2,.L3
	nop
	b	.L4
	move	$2,$0
.L5:
	lbu	$7,4($7)
	lbu	$3,4($6)
	nop
	bne	$7,$3,.L3
	nop
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
	nop
	sltu	$2,$6,2
	bne	$2,$0,.L24
	nop
	lw	$3,0($4)
	lw	$2,0($5)
	nop
	slt	$7,$2,$3
	beq	$7,$0,.L17
	nop
	move	$2,$3
.L17:
	blez	$2,.L11
	nop
	lbu	$2,4($4)
	lbu	$3,4($5)
	nop
	sltu	$2,$3,$2
	beq	$2,$0,.L25
	nop
	xori	$6,$6,0x4
	sltu	$2,$6,1
	j	$31
.L24:
	lw	$9,0($4)
	lw	$2,0($5)
	nop
	beq	$9,$2,.L26
	nop
.L11:
	move	$2,$0
	j	$31
.L25:
	xori	$6,$6,0x2
	sltu	$2,$6,1
	j	$31
.L26:
	blez	$9,.L13
	nop
	bne	$6,$0,.L27
	nop
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
	nop
	xor	$2,$6,$2
	sltu	$2,$0,$2
	j	$31
	.align	4
	.globl	main
main:
	la	$3,consts
	move	$2,$0
	li	$5,1			# 0x1
	li	$4,256			# 0x100
.L30:
	sb	$2,4($3)
	addiu	$2,$2,1
	sw	$5,0($3)
	bne	$2,$4,.L30
	j	tig_main
	.align	4
	.globl	tig_ord
tig_ord:
	lw	$2,0($4)
	nop
	beq	$2,$0,.L37
	nop
	lbu	$2,4($4)
	nop
	j	$31
.L37:
	li	$2,-1			# 0xffffffffffffffff
	j	$31
	.align	4
	.globl	tig_chr
tig_chr:
	la	$2,consts
	sll	$4,$4,3
	addu	$2,$2,$4
	j	$31
	.align	4
	.globl	tig_size
tig_size:
	lw	$2,0($4)
	nop
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
	la	$25,getchar
	nop
	jalr	$25
	nop
	beq	$2,$0,.L48
	sll	$2,$2,3
	la	$3,consts
	lw	$31,28($sp)
	addu	$2,$3,$2
	addiu	$sp,$sp,32
	j	$31
.L48:
	lw	$31,28($sp)
	la $2, empty
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
	nop
	beq	$2,$0,.L50
	move	$16,$4
	lw	$17,0($5)
	nop
	bne	$17,$0,.L58
	addu	$17,$17,$2
	move	$5,$4
.L50:
	lw	$31,44($sp)
	move	$2,$5
	lw	$17,40($sp)
	lw	$16,36($sp)
	addiu	$sp,$sp,48
	j	$31
.L58:
	la	$25,tig_malloc
	addiu	$4,$17,4
	jalr	$25
	sw	$5,24($sp)
	sw	$17,0($2)
	lw	$4,0($16)
	lw	$5,24($sp)
	blez	$4,.L52
	nop
	move	$3,$0
.L53:
	addu	$6,$16,$3
	lbu	$8,4($6)
	addu	$7,$2,$3
	addiu	$3,$3,1
	slt	$6,$3,$4
	bne	$6,$0,.L53
	sb	$8,4($7)
.L52:
	lw	$9,0($5)
	nop
	blez	$9,.L54
	nop
	move	$3,$0
.L55:
	addu	$6,$5,$3
	lbu	$8,4($6)
	addu	$7,$2,$3
	addiu	$3,$3,1
	addu	$7,$7,$4
	slt	$6,$3,$9
	bne	$6,$0,.L55
	sb	$8,4($7)
.L54:
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
	la	$25,tig_malloc
	nop
	jalr	$25
	move	$16,$4
	blez	$16,.L60
	nop
	move	$4,$2
	move	$3,$0
.L61:
	addiu	$3,$3,4
	slt	$5,$3,$16
	sw	$0,0($4)
	bne	$5,$0,.L61
	addiu	$4,$4,4
.L60:
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
	la	$25,tig_malloc
	sw	$5,24($sp)
	move	$16,$4
	jalr	$25
	sll	$4,$4,2
	lw	$5,24($sp)
	blez	$16,.L65
	nop
	move	$4,$2
	move	$3,$0
.L66:
	addiu	$3,$3,1
	sw	$5,0($4)
	bne	$3,$16,.L66
	addiu	$4,$4,4
.L65:
	lw	$31,36($sp)
	lw	$16,32($sp)
	addiu	$sp,$sp,40
	j	$31
	.align	4
	.globl	tig_substring
tig_substring:
	addiu	$sp,$sp,-40
	sw	$31,36($sp)
	sw	$16,32($sp)
	bltz	$5,.L70
	move	$16,$4
	lw	$2,0($4)
	addu	$3,$6,$5
	slt	$2,$2,$3
	bne	$2,$0,.L70
	li	$2,1			# 0x1
	beq	$6,$2,.L86
	nop
	la	$25,tig_malloc
	addiu	$4,$6,4
	sw	$5,24($sp)
	jalr	$25
	sw	$6,28($sp)
	lw	$6,28($sp)
	nop
	sw	$6,0($2)
	lw	$5,24($sp)
	blez	$6,.L73
	move	$4,$2
	addiu	$9,$5,4
	sltu	$3,$6,4
	addu	$9,$16,$9
	bne	$3,$0,.L74
	addiu	$7,$4,4
	andi	$3,$9,0x3
	bne	$3,$0,.L74
	addiu	$3,$9,4
	sltu	$7,$3,$7
	beq	$7,$0,.L87
	addiu	$3,$4,8
	srl	$12,$6,2
.L88:
	sll	$3,$12,2
	beq	$3,$0,.L77
	move	$7,$0
	move	$8,$4
.L78:
	lw	$11,0($9)
	addiu	$7,$7,1
	sltu	$10,$7,$12
	sw	$11,4($8)
	addiu	$9,$9,4
	bne	$10,$0,.L78
	addiu	$8,$8,4
	beq	$6,$3,.L73
	nop
.L77:
	addu	$5,$3,$5
	addu	$16,$16,$5
	addu	$4,$4,$3
	addiu	$16,$16,4
	addiu	$4,$4,4
.L79:
	lbu	$7,0($16)
	addiu	$3,$3,1
	slt	$5,$3,$6
	sb	$7,0($4)
	addiu	$16,$16,1
	bne	$5,$0,.L79
	addiu	$4,$4,1
.L73:
	lw	$31,36($sp)
	lw	$16,32($sp)
	addiu	$sp,$sp,40
	j	$31
.L86:
	addu	$5,$4,$5
	lbu	$2,4($5)
	la	$3,consts
	lw	$31,36($sp)
	sll	$2,$2,3
	addu	$2,$3,$2
	lw	$16,32($sp)
	addiu	$sp,$sp,40
	j	$31
.L87:
	sltu	$3,$3,$9
	bne	$3,$0,.L88
	srl	$12,$6,2
.L74:
	addu	$5,$16,$5
	addiu	$5,$5,4
	move	$3,$0
.L80:
	lbu	$8,0($5)
	addu	$7,$4,$3
	addiu	$3,$3,1
	sb	$8,4($7)
	bne	$3,$6,.L80
	addiu	$5,$5,1
	lw	$31,36($sp)
	lw	$16,32($sp)
	addiu	$sp,$sp,40
	j	$31
.L70:
	la	$25,exit
	nop
	jalr	$25
	li	$4,1			# 0x1
	.align	4
	.globl	tig_flush
tig_flush:
	la	$25,fflush
	nop
	jr	$25
	nop
	.align	4
	.globl	tig_print
tig_print:
	addiu	$sp,$sp,-40
	sw	$31,36($sp)
	sw	$18,32($sp)
	sw	$17,28($sp)
	sw	$16,24($sp)
	lw	$2,0($4)
	nop
	blez	$2,.L94
	move	$18,$4
	addiu	$17,$4,4
	move	$16,$0
.L93:
	la	$25,putchar
	lbu	$4,0($17)
	jalr	$25
	addiu	$16,$16,1
	lw	$2,0($18)
	slt	$2,$16,$2
	bne	$2,$0,.L93
	addiu	$17,$17,1
.L94:
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
