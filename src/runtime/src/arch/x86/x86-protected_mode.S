#####################################################################
#  Copyright (c) 2019, CXOS.
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  Authors:
#     Anthony <ajxs [at] panoptic.online>
#####################################################################

.section .text

#####################################################################
#  Protected_Mode_Init
#
#  Performs the jump to protected mode.
#  Reloads the segment registers after the jump.
#####################################################################
.global __protected_mode_init
.type __protected_mode_init, @function
__protected_mode_init:
	# Clear interrupt flag.
	cli

	# Enable protected mode flag in processor control register 0.
	movl %cr0, %eax
	orl $0x1, %eax
	movl %eax, %cr0

	# This far-jump instruction loads the value of '1' into the CS register. JMP is used 
	# here since we can't modify CS with the MOV instruction. The JMP instruction used with 
	# the format 'CS:OFFSET' will reload the CS register to the target of the jump.
	# Refer to: https://c9x.me/x86/html/file_module_x86_id_147.html
	# This refers to GDT entry 1 being the code segment.
	# This is because bit 1 << 3 is the index bit of the segment structure, the 0x8 here 
	# will set that bit to '1'.
	# Refer to 3.4.2 of the Intel SDM on page 95 for a description of the segment selector
	# structure. 
	# For a simple explanation, refer to: https://stackoverflow.com/a/23979175/5931673
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
