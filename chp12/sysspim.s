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

