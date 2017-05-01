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
.align	2
.globl	tig_stringEqual
tig_stringEqual:
beq     $a0,$a1,.L2
lw      $t0,0($a0)
lw      $v0,0($a1)
beq     $t0,$v0,.L9
.L3:
move    $v0,$0
jr      $ra
.L9:
blez    $t0,.L2
lbu     $v1,4($a0)
lbu     $v0,4($a1)
bne     $v1,$v0,.L3
b       .L4
move    $v0,$0
.L5:
lbu     $a3,4($a3)
lbu     $v1,4($a2)
bne     $a3,$v1,.L3
.L4:
addiu   $v0,$v0,1
slt     $v1,$v0,$t0
addu    $a3,$a0,$v0
bne     $v1,$0,.L5
addu    $a2,$a1,$v0
.L2:
li      $v0,1		# 0x1
jr      $ra
.align	2
.globl	tig_strcmp
tig_strcmp:
sltu    $v0,$a2,6
beq     $v0,$0,.L11
sltu    $v0,$a2,2
bne     $v0,$0,.L24
lw      $v1,0($a0)
lw      $v0,0($a1)
slt     $a3,$v0,$v1
beq     $a3,$0,.L17
move    $v0,$v1
.L17:
blez    $v0,.L11
lbu     $v0,4($a0)
lbu     $v1,4($a1)
sltu    $v0,$v1,$v0
beq     $v0,$0,.L25
xori    $a2,$a2,0x4
sltu    $v0,$a2,1
jr      $ra
.L24:
lw      $t1,0($a0)
lw      $v0,0($a1)
beq     $t1,$v0,.L26
.L11:
move    $v0,$0
jr      $ra
.L25:
xori    $a2,$a2,0x2
sltu    $v0,$a2,1
jr      $ra
.L26:
blez    $t1,.L13
bne     $a2,$0,.L27
move    $v0,$0
addu    $a3,$a0,$v0
.L28:
addu    $v1,$a1,$v0
lbu     $a3,4($a3)
lbu     $v1,4($v1)
addiu   $v0,$v0,1
bne     $a3,$v1,.L11
slt     $t0,$v0,$t1
bne     $t0,$0,.L28
addu    $a3,$a0,$v0
.L13:
xori    $v0,$a2,0x1
jr      $ra
.L27:
lbu     $a2,4($a0)
lbu     $v0,4($a1)
xor     $v0,$a2,$v0
sltu    $v0,$0,$v0
jr      $ra
.align	2
.globl	tig_ord
tig_ord:
lw      $v0,0($a0)
beq     $v0,$0,.L33
lbu     $v0,4($a0)
jr      $ra
.L33:
li      $v0,-1		# 0xffffffffffffffff
jr      $ra
.align	2
.globl	tig_size
tig_size:
lw      $v0,0($a0)
jr      $ra
.align	2
.globl	tig_not
tig_not:
sltu    $v0,$a0,1
jr      $ra
.align	2
.globl	tig_getchar
tig_getchar:
addiu   $sp,$sp,-32
sw      $ra,28($sp)
jal     sys_tig_getchar
beq     $v0,$0,.L42
sll     $v0,$v0,3
la      $v1,consts
lw      $ra,28($sp)
addu    $v0,$v1,$v0
addiu   $sp,$sp,32
jr      $ra
.L42:
lw      $ra,28($sp)
la      $v0,empty
addiu   $sp,$sp,32
jr      $ra
.align	2
.globl	tig_concat
tig_concat:
addiu   $sp,$sp,-48
sw      $ra,44($sp)
sw      $s1,40($sp)
sw      $s0,36($sp)
lw      $v0,0($a0)
beq     $v0,$0,.L44
move    $s0,$a0
lw      $s1,0($a1)
addu    $s1,$s1,$v0
bne     $s1,$0,.L52
move    $a1,$a0
.L44:
lw      $ra,44($sp)
move    $v0,$a1
lw      $s1,40($sp)
lw      $s0,36($sp)
addiu   $sp,$sp,48
jr      $ra
.L52:
addiu   $a0,$s1,4
jal     sys_tig_malloc
sw      $a1,24($sp)
sw      $s1,0($v0)
lw      $a0,0($s0)
lw      $a1,24($sp)
blez    $a0,.L46
move    $v1,$0
.L47:
addu    $a2,$s0,$v1
lbu     $t0,4($a2)
addu    $a3,$v0,$v1
addiu   $v1,$v1,1
slt     $a2,$v1,$a0
sb      $t0,4($a3)
bne     $a2,$0,.L47
.L46:
lw      $t1,0($a1)
blez    $t1,.L48
move    $v1,$0
.L49:
addu    $a2,$a1,$v1
lbu     $t0,4($a2)
addu    $a3,$v0,$v1
addiu   $v1,$v1,1
addu    $a3,$a3,$a0
slt     $a2,$v1,$t1
sb      $t0,4($a3)
bne     $a2,$0,.L49
.L48:
lw      $ra,44($sp)
move    $a1,$v0
move    $v0,$a1
lw      $s1,40($sp)
lw      $s0,36($sp)
addiu   $sp,$sp,48
jr      $ra
.align	2
.globl	tig_allocRecord
tig_allocRecord:
addiu   $sp,$sp,-32
sw      $ra,28($sp)
sw      $s0,24($sp)
move    $s0,$a0
jal     sys_tig_malloc
blez    $s0,.L54
move    $a0,$v0
move    $v1,$0
.L55:
addiu   $v1,$v1,4
slt     $a1,$v1,$s0
sw      $0,0($a0)
bne     $a1,$0,.L55
addiu   $a0,$a0,4
.L54:
lw      $ra,28($sp)
lw      $s0,24($sp)
addiu   $sp,$sp,32
jr      $ra
.align	2
.globl	tig_initArray
tig_initArray:
addiu   $sp,$sp,-40
sw      $ra,36($sp)
sw      $s0,32($sp)
sw      $a1,24($sp)
move    $s0,$a0
sll     $a0,$a0,2
jal     sys_tig_malloc
lw      $a1,24($sp)
blez    $s0,.L59
move    $a0,$v0
move    $v1,$0
.L60:
addiu   $v1,$v1,1
sw      $a1,0($a0)
addiu   $a0,$a0,4
bne     $v1,$s0,.L60
.L59:
lw      $ra,36($sp)
lw      $s0,32($sp)
addiu   $sp,$sp,40
jr      $ra
.LC0:
.data
.align	2
.ascii"substring([%d],%d,%d) out of range\012\000"
.text
.align	2
.globl	tig_substring
tig_substring:
addiu   $sp,$sp,-40
sw      $ra,36($sp)
sw      $s2,32($sp)
sw      $s1,28($sp)
sw      $s0,24($sp)
move    $s1,$a1
move    $s2,$a0
bltz    $a1,.L81
move    $s0,$a2
lw      $a1,0($a0)
addu    $v0,$a2,$s1
slt     $v0,$a1,$v0
bne     $v0,$0,.L65
li      $v0,1		# 0x1
beq     $s0,$v0,.L82
.L67:
addiu   $a0,$s0,4
jal     sys_tig_malloc
move    $a0,$v0
blez    $s0,.L68
sw      $s0,0($v0)
addiu   $a3,$s1,4
sltu    $v1,$s0,4
addu    $a3,$s2,$a3
bne     $v1,$0,.L69
addiu   $a1,$a0,4
andi    $v1,$a3,0x3
bne     $v1,$0,.L69
addiu   $v1,$a3,4
sltu    $a1,$v1,$a1
beq     $a1,$0,.L83
addiu   $v1,$a0,8
srl     $t2,$s0,2
.L84:
sll     $v1,$t2,2
beq     $v1,$0,.L72
move    $a2,$a0
move    $a1,$0
.L73:
lw      $t1,0($a3)
addiu   $a1,$a1,1
sltu    $t0,$a1,$t2
sw      $t1,4($a2)
addiu   $a3,$a3,4
bne     $t0,$0,.L73
addiu   $a2,$a2,4
beq     $s0,$v1,.L68
.L72:
addu    $s1,$v1,$s1
addu    $s2,$s2,$s1
addu    $a0,$a0,$v1
addiu   $s2,$s2,4
addiu   $a0,$a0,4
.L74:
lbu     $a2,0($s2)
addiu   $v1,$v1,1
slt     $a1,$v1,$s0
sb      $a2,0($a0)
addiu   $s2,$s2,1
bne     $a1,$0,.L74
addiu   $a0,$a0,1
.L68:
lw      $ra,36($sp)
lw      $s2,32($sp)
lw      $s1,28($sp)
lw      $s0,24($sp)
addiu   $sp,$sp,40
jr      $ra
.L81:
lw      $a1,0($a0)
.L65:
la      $a0,.LC0
move    $a2,$s1
jal     sys_tig_printf
move    $a3,$s0
li      $a0,1		# 0x1
jal     sys_tig_exit
li      $v0,1		# 0x1
bne     $s0,$v0,.L67
.L82:
addu    $s1,$s2,$s1
lbu     $v0,4($s1)
la      $v1,consts
lw      $ra,36($sp)
sll     $v0,$v0,3
addu    $v0,$v1,$v0
lw      $s2,32($sp)
lw      $s1,28($sp)
lw      $s0,24($sp)
addiu   $sp,$sp,40
jr      $ra
.L83:
sltu    $v1,$v1,$a3
bne     $v1,$0,.L84
srl     $t2,$s0,2
.L69:
addu    $s1,$s2,$s1
addiu   $s1,$s1,4
move    $v1,$0
.L75:
lbu     $a2,0($s1)
addu    $a1,$a0,$v1
addiu   $v1,$v1,1
sb      $a2,4($a1)
bne     $v1,$s0,.L75
addiu   $s1,$s1,1
lw      $ra,36($sp)
lw      $s2,32($sp)
lw      $s1,28($sp)
lw      $s0,24($sp)
addiu   $sp,$sp,40
jr      $ra
.LC1:
.data
.align	2
.ascii	"chr(%d) out of range\012\000"
.text
.align	2
.globl	tig_chr
tig_chr:
addiu   $sp,$sp,-32
sw      $ra,28($sp)
sw      $s0,24($sp)
sltu    $v0,$a0,256
move    $s0,$a0
bne     $v0,$0,.L86
la      $a0,.LC1
jal     sys_tig_printf
move    $a1,$s0
li      $a0,1		# 0x1
jal     sys_tig_exit
.L86:
la      $v0,consts
sll     $s0,$s0,3
addu    $v0,$v0,$s0
lw      $ra,28($sp)
lw      $s0,24($sp)
addiu   $sp,$sp,32
jr      $ra
.align	2
.globl	main
main:
la      $v1,consts
move    $v0,$0
li      $a1,1		# 0x1
li      $a0,256		# 0x100
.L89:
sb      $v0,4($v1)
addiu   $v0,$v0,1
sw      $a1,0($v1)
addiu   $v1,$v1,8
bne     $v0,$a0,.L89
move    $a0,$0
j       tig_main
.align	2
.globl	tig_flush
tig_flush:
jr      $ra
.align	2
.globl	tig_print
tig_print:
addiu   $sp,$sp,-40
sw      $ra,36($sp)
sw      $s2,32($sp)
sw      $s1,28($sp)
sw      $s0,24($sp)
lw      $v0,0($a0)
blez    $v0,.L97
move    $s2,$a0
addiu   $s1,$a0,4
move    $s0,$0
.L96:
lbu     $a0,0($s1)
jal     sys_tig_putchar
addiu   $s0,$s0,1
lw      $v0,0($s2)
slt     $v0,$s0,$v0
addiu   $s1,$s1,1
bne     $v0,$0,.L96
.L97:
lw      $ra,36($sp)
lw      $s2,32($sp)
lw      $s1,28($sp)
lw      $s0,24($sp)
addiu   $sp,$sp,40
jr      $ra
.globl	consts
.data
.align 4
consts:
.word	0
.byte	0x0
.space	3
.space	2040
.globl	empty
.align	2
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
L83:
sw      $s0, -28($fp)
sw      $s1, -32($fp)
addi    $s0, $fp, -8
jal     tig_getchar
sw      $v0, 0($s0)
move    $a0, $fp
jal     L27
move    $s1, $v0
addi    $s0, $fp, -8
jal     tig_getchar
sw      $v0, 0($s0)
move    $a0, $fp
jal     L27
move    $a0, $fp
move    $a1, $s1
move    $a2, $v0
jal     L28
move    $a0, $fp
move    $a1, $v0
jal     L30
lw      $s0, -28($fp)
lw      $s1, -32($fp)
j       L82
L82:
lw      $ra, 20($sp)
lw      $fp, 28($sp)
addiu   $sp, $sp, 32
jr      $ra
L30:
addiu   $sp, $sp, -28
sw      $fp, 24($sp)
sw      $ra, 16($sp)
addiu   $fp, $sp, 28
L85:
sw      $a0, -8($fp)
sw      $a1, -28($fp)
lw      $a1, -28($fp)
addi    $a0, $a1, 0
beqz    $a0, L79
j       L80
L80:
lw      $a0, -8($fp)
lw      $a1, -28($fp)
sub     $a1, $0, $a1
beqz    $a1, L73
j       L74
L74:
lw      $a1, -28($fp)
add     $a1, $a1, $0
lw      $a1, 0($a1)
jal     L29
la      $a0, L12
jal     tig_print
lw      $a0, -8($fp)
lw      $a1, -28($fp)
sub     $a1, $0, $a1
beqz    $a1, L77
j       L78
L78:
li      $a2, 4
lw      $a1, -28($fp)
add     $a1, $a1, $a2
lw      $a1, 0($a1)
jal     L30
L81:
j       L84
L79:
la      $a0, L13
jal     tig_print
j       L81
L73:
li      $v0, 10
syscall
j       L74
L77:
li      $v0, 10
syscall
j       L78
L84:
lw      $ra, 16($sp)
lw      $fp, 24($sp)
addiu   $sp, $sp, 28
jr      $ra
L29:
addiu   $sp, $sp, -28
sw      $fp, 24($sp)
sw      $ra, 16($sp)
addiu   $fp, $sp, 28
L87:
sw      $a0, -8($fp)
sw      $a1, -28($fp)
lw      $a1, -28($fp)
addi    $a0, $a1, 0
bltz    $a0, L68
j       L69
L69:
lw      $a1, -28($fp)
addi    $a0, $a1, 0
bgtz    $a0, L65
j       L66
L66:
la      $a0, L4
jal     tig_print
L67:
L70:
j       L86
L68:
la      $a0, L64
jal     tig_print
move    $a0, $fp
lw      $a1, -28($fp)
sub     $a1, $0, $a1
jal     L61
j       L70
L65:
move    $a0, $fp
lw      $a1, -28($fp)
jal     L61
j       L67
L86:
lw      $ra, 16($sp)
lw      $fp, 24($sp)
addiu   $sp, $sp, 28
jr      $ra
L61:
addiu   $sp, $sp, -32
sw      $fp, 28($sp)
sw      $ra, 20($sp)
addiu   $fp, $sp, 32
L89:
sw      $a0, -8($fp)
sw      $a1, -32($fp)
sw      $s0, -28($fp)
lw      $a1, -32($fp)
addi    $a0, $a1, 0
bgtz    $a0, L63
j       L62
L62:
lw      $s0, -28($fp)
j       L88
L63:
lw      $a0, -8($fp)
li      $a2, 10
lw      $a1, -32($fp)
div     $a1, $a1, $a2
jal     L61
li      $a0, 10
lw      $a1, -32($fp)
div     $a1, $a1, $a0
li      $a0, 10
mul     $a0, $a1, $a0
lw      $a1, -32($fp)
sub     $s0, $a1, $a0
la      $a0, L4
jal     tig_ord
add     $a0, $s0, $v0
jal     tig_chr
move    $a0, $v0
jal     tig_print
j       L62
L88:
lw      $ra, 20($sp)
lw      $fp, 28($sp)
addiu   $sp, $sp, 32
jr      $ra
L28:
addiu   $sp, $sp, -44
sw      $fp, 40($sp)
sw      $ra, 32($sp)
addiu   $fp, $sp, 44
L91:
sw      $a0, -8($fp)
sw      $a1, -44($fp)
sw      $a2, -40($fp)
sw      $s0, -32($fp)
sw      $s1, -36($fp)
lw      $a1, -44($fp)
addi    $a0, $a1, 0
beqz    $a0, L58
j       L59
L59:
lw      $a2, -40($fp)
addi    $a0, $a2, 0
beqz    $a0, L55
j       L56
L56:
lw      $a1, -44($fp)
sub     $a0, $0, $a1
beqz    $a0, L36
j       L37
L37:
lw      $a1, -44($fp)
add     $a0, $a1, $0
lw      $a1, 0($a0)
lw      $a2, -40($fp)
sub     $a0, $0, $a2
beqz    $a0, L38
j       L39
L39:
lw      $a2, -40($fp)
add     $a0, $a2, $0
lw      $a0, 0($a0)
sub     $a0, $a1, $a0
bltz    $a0, L52
j       L53
L53:
li      $a0, 8
jal     tig_allocRecord
move    $s1, $v0
addi    $a1, $s1, 0
lw      $a2, -40($fp)
sub     $a0, $0, $a2
beqz    $a0, L46
j       L47
L47:
lw      $a2, -40($fp)
add     $a0, $a2, $0
lw      $a0, 0($a0)
sw      $a0, 0($a1)
addi    $s0, $s1, 4
lw      $a0, -8($fp)
lw      $a1, -44($fp)
lw      $a2, -40($fp)
sub     $a2, $0, $a2
beqz    $a2, L50
j       L51
L51:
li      $a3, 4
lw      $a2, -40($fp)
add     $a2, $a2, $a3
lw      $a2, 0($a2)
jal     L28
sw      $v0, 0($s0)
L54:
move    $a1, $s1
L57:
move    $v0, $a1
L60:
lw      $s0, -32($fp)
lw      $s1, -36($fp)
j       L90
L58:
lw      $a2, -40($fp)
move    $v0, $a2
j       L60
L55:
lw      $a1, -44($fp)
j       L57
L36:
li      $v0, 10
syscall
j       L37
L38:
li      $v0, 10
syscall
j       L39
L52:
li      $a0, 8
jal     tig_allocRecord
move    $s1, $v0
addi    $a2, $s1, 0
lw      $a1, -44($fp)
sub     $a0, $0, $a1
beqz    $a0, L40
j       L41
L41:
lw      $a1, -44($fp)
add     $a0, $a1, $0
lw      $a0, 0($a0)
sw      $a0, 0($a2)
addi    $s0, $s1, 4
lw      $a0, -8($fp)
lw      $a1, -44($fp)
sub     $a1, $0, $a1
beqz    $a1, L44
j       L45
L45:
li      $a2, 4
lw      $a1, -44($fp)
add     $a1, $a1, $a2
lw      $a1, 0($a1)
lw      $a2, -40($fp)
jal     L28
sw      $v0, 0($s0)
j       L54
L40:
li      $v0, 10
syscall
j       L41
L44:
li      $v0, 10
syscall
j       L45
L46:
li      $v0, 10
syscall
j       L47
L50:
li      $v0, 10
syscall
j       L51
L90:
lw      $ra, 32($sp)
lw      $fp, 40($sp)
addiu   $sp, $sp, 44
jr      $ra
L27:
addiu   $sp, $sp, -36
sw      $fp, 32($sp)
sw      $ra, 24($sp)
addiu   $fp, $sp, 36
L93:
sw      $a0, -8($fp)
sw      $s0, -28($fp)
sw      $s1, -32($fp)
sw      $s2, -36($fp)
li      $a0, 4
jal     tig_allocRecord
sw      $0, 0($v0)
move    $s0, $v0
lw      $a0, -8($fp)
move    $a1, $s0
jal     L1
move    $s2, $v0
sub     $a0, $0, $s0
beqz    $a0, L31
j       L32
L32:
add     $a0, $s0, $0
lw      $a0, 0($a0)
sub     $a0, $0, $a0
bnez    $a0, L33
j       L34
L34:
li      $v0, 0
L35:
lw      $s0, -28($fp)
lw      $s1, -32($fp)
lw      $s2, -36($fp)
j       L92
L31:
li      $v0, 10
syscall
j       L32
L33:
li      $a0, 8
jal     tig_allocRecord
move    $s1, $v0
sw      $s2, 0($s1)
addi    $s0, $s1, 4
lw      $a0, -8($fp)
jal     L27
sw      $v0, 0($s0)
move    $v0, $s1
j       L35
L92:
lw      $ra, 24($sp)
lw      $fp, 32($sp)
addiu   $sp, $sp, 36
jr      $ra
L1:
addiu   $sp, $sp, -36
sw      $fp, 32($sp)
sw      $ra, 24($sp)
addiu   $fp, $sp, 36
L95:
sw      $a0, -8($fp)
sw      $a1, -36($fp)
sw      $s0, -28($fp)
sw      $s1, -32($fp)
li      $s1, 0
move    $a0, $fp
jal     L3
lw      $a1, -36($fp)
sub     $a0, $0, $a1
beqz    $a0, L22
j       L23
L23:
lw      $a1, -36($fp)
add     $s0, $a1, $0
move    $a0, $fp
lw      $a1, -8($fp)
lw      $a1, -8($a1)
jal     L2
sw      $v0, 0($s0)
L25:
move    $a0, $fp
lw      $a1, -8($fp)
lw      $a1, -8($a1)
jal     L2
sub     $a0, $0, $v0
bnez    $a0, L26
j       L24
L24:
move    $v0, $s1
lw      $s0, -28($fp)
lw      $s1, -32($fp)
j       L94
L22:
li      $v0, 10
syscall
j       L23
L26:
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
j       L25
L94:
lw      $ra, 24($sp)
lw      $fp, 32($sp)
addiu   $sp, $sp, 36
jr      $ra
L3:
addiu   $sp, $sp, -32
sw      $fp, 28($sp)
sw      $ra, 20($sp)
addiu   $fp, $sp, 32
L97:
sw      $a0, -8($fp)
sw      $s0, -32($fp)
L20:
lw      $a0, -8($fp)
lw      $a0, -8($a0)
lw      $a0, -8($a0)
la      $a1, L12
li      $a2, 0
jal     tig_strcmp
sub     $a0, $0, $v0
bnez    $a0, L14
j       L15
L15:
lw      $a0, -8($fp)
lw      $a0, -8($a0)
lw      $a0, -8($a0)
la      $a1, L13
li      $a2, 0
jal     tig_strcmp
L16:
sub     $a0, $0, $v0
bnez    $a0, L21
j       L11
L11:
li      $v0, 0
lw      $s0, -32($fp)
j       L96
L14:
li      $v0, 1
j       L16
L21:
lw      $a0, -8($fp)
lw      $a0, -8($a0)
addi    $s0, $a0, -8
jal     tig_getchar
sw      $v0, 0($s0)
j       L20
L96:
lw      $ra, 20($sp)
lw      $fp, 28($sp)
addiu   $sp, $sp, 32
jr      $ra
L2:
addiu   $sp, $sp, -28
sw      $fp, 24($sp)
sw      $ra, 16($sp)
addiu   $fp, $sp, 28
L99:
sw      $a0, -8($fp)
sw      $s0, -24($fp)
sw      $s1, -28($fp)
lw      $a0, -8($fp)
lw      $a0, -8($a0)
lw      $a0, -8($a0)
jal     tig_ord
move    $s0, $v0
la      $a0, L4
jal     tig_ord
sub     $a0, $s0, $v0
bgez    $a0, L6
j       L7
L7:
li      $v0, 0
L8:
lw      $s0, -24($fp)
lw      $s1, -28($fp)
j       L98
L6:
li      $s1, 1
lw      $a0, -8($fp)
lw      $a0, -8($a0)
lw      $a0, -8($a0)
jal     tig_ord
move    $s0, $v0
la      $a0, L5
jal     tig_ord
sub     $a0, $s0, $v0
blez    $a0, L9
j       L10
L10:
li      $s1, 0
L9:
move    $v0, $s1
j       L8
L98:
lw      $ra, 16($sp)
lw      $fp, 24($sp)
addiu   $sp, $sp, 28
jr      $ra
.data
L64:
 .align 2
 .word  1
 .align 2
 .ascii "-"
L13:
 .align 2
 .word  1
 .align 2
 .ascii "
"
L12:
 .align 2
 .word  1
 .align 2
 .ascii " "
L5:
 .align 2
 .word  1
 .align 2
 .ascii "9"
L4:
 .align 2
 .word  1
 .align 2
 .ascii "0"
