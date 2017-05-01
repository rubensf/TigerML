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
j       $ra
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
j       $ra
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
j       $ra
.L24:
lw      $t1,0($a0)
lw      $v0,0($a1)
beq     $t1,$v0,.L26
.L11:
move    $v0,$0
j       $ra
.L25:
xori    $a2,$a2,0x2
sltu    $v0,$a2,1
j       $ra
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
j       $ra
.L27:
lbu     $a2,4($a0)
lbu     $v0,4($a1)
xor     $v0,$a2,$v0
sltu    $v0,$0,$v0
j       $ra
.align	2
.globl	tig_ord
tig_ord:
lw      $v0,0($a0)
beq     $v0,$0,.L33
lbu     $v0,4($a0)
j       $ra
.L33:
li      $v0,-1		# 0xffffffffffffffff
j       $ra
.align	2
.globl	tig_size
tig_size:
lw      $v0,0($a0)
j       $ra
.align	2
.globl	tig_not
tig_not:
sltu    $v0,$a0,1
j       $ra
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
j       $ra
.L42:
lw      $ra,28($sp)
la      $v0,empty
addiu   $sp,$sp,32
j       $ra
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
bne     $s1,$0,.L52
addu    $s1,$s1,$v0
move    $a1,$a0
.L44:
lw      $ra,44($sp)
move    $v0,$a1
lw      $s1,40($sp)
lw      $s0,36($sp)
addiu   $sp,$sp,48
j       $ra
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
bne     $a2,$0,.L47
sb      $t0,4($a3)
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
bne     $a2,$0,.L49
sb      $t0,4($a3)
.L48:
lw      $ra,44($sp)
move    $a1,$v0
move    $v0,$a1
lw      $s1,40($sp)
lw      $s0,36($sp)
addiu   $sp,$sp,48
j       $ra
.align	2
.globl	tig_allocRecord
tig_allocRecord:
addiu   $sp,$sp,-32
sw      $ra,28($sp)
sw      $s0,24($sp)
jal     sys_tig_malloc
move    $s0,$a0
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
j       $ra
.align	2
.globl	tig_initArray
tig_initArray:
addiu   $sp,$sp,-40
sw      $ra,36($sp)
sw      $s0,32($sp)
sw      $a1,24($sp)
move    $s0,$a0
jal     sys_tig_malloc
sll     $a0,$a0,2
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
j       $ra
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
jal     sys_tig_malloc
addiu   $a0,$s0,4
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
j       $ra
.L81:
lw      $a1,0($a0)
.L65:
la      $a0,.LC0
move    $a2,$s1
jal     sys_tig_printf
move    $a3,$s0
jal     sys_tig_exit
li      $a0,1		# 0x1
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
j       $ra
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
j       $ra
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
bne     $v0,$0,.L86
move    $s0,$a0
la      $a0,.LC1
jal     sys_tig_printf
move    $a1,$s0
li      $a0,1		# 0x1
jal     sys_tig_exit
.L86:
la      $v0,consts
lw      $ra,28($sp)
sll     $s0,$s0,3
addu    $v0,$v0,$s0
lw      $s0,24($sp)
addiu   $sp,$sp,32
j       $ra
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
slt $v0,$s0,$v0
bne $v0,$0,.L96
addiu $s1,$s1,1
.L97:
lw      $ra,36($sp)
lw      $s2,32($sp)
lw      $s1,28($sp)
lw      $s0,24($sp)
addiu   $sp,$sp,40
j       $ra
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
