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
#  Paging Load
#
#  Instructs the processor on the location of the Page Table Directory.
#####################################################################
.global __paging_load
.type __paging_load, @function
__paging_load:
	movl %esp, %ebp

	# Store address of page directory in eax, move to CR3.
	leal page_directory_start, %eax
	movl %eax, %cr3

	# Enable paging bit in CR0.
	movl %cr0, %eax
	orl $0x80000000, %eax
	movl %eax, %cr0

	# Restore stack pointer
	movl %ebp, %esp

	ret
