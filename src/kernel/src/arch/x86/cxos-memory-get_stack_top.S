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
# Get stack top
#
# Returns a pointer to the currently loaded process' stack top.
#####################################################################
.global cxos_memory_get_stack_top
.type cxos_memory_get_stack_top, @function
cxos_memory_get_stack_top:
   movl %esp, %eax                     # Move ESP into EAX.
   ret
