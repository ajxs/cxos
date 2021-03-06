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
#  Flush_Tlb
#
#  Reloads the Translation Lookaside Buffer.
#####################################################################
.global cxos_memory_paging_flush_tlb
.type cxos_memory_paging_flush_tlb, @function
cxos_memory_paging_flush_tlb:
	movl %cr3, %eax
	movl %eax, %cr3

	ret
