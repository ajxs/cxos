.section .text

#####################################################################
#  Protected_Mode_Init
#
#  Performs the jump to protected mode.
#  Reloads the segment registers after the jump.
#####################################################################
.global __pmode_init
.type __pmode_init, @function
__pmode_init:
	# Clear interrupt flag.
	cli

	# Enable protected mode flag in processor control register 0.
	movl %cr0, %eax
	orl $0x1, %eax
	movl %eax, %cr0

	# Use a far jump to reload the segment registers.
	# Refer to '__gdt_load' function and associated documentation for 
	# a better explanation of this method.
	ljmp $0x0008, $.Lpmode_reload_segments

.Lpmode_reload_segments:
	mov $0x10, %eax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs
	mov %ax, %ss

	# Set interrupt flag.
	sti

	ret
