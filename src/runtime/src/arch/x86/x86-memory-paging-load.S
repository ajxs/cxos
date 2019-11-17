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
#  Load Page Directory
#
#  Loads a pointer to a Page Table Directory structure.
#####################################################################
.global __load_page_directory
.type __load_page_directory, @function
__load_page_directory:
	pushl %ebp
	movl %esp, %ebp

	movl 0x8(%ebp), %eax
	movl %eax, %cr3

	leave
	ret
