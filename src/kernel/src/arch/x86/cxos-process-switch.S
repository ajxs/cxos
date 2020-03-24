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
#  Switch to process
#
#  Switches to the provided process.
#####################################################################
.type __process_switch_to_process, @function
.global __process_switch_to_process
__process_switch_to_process:
   pushl %ebp
   movl %esp, %ebp

   pushl %ebx
   pushl %esi
   pushl %edi
   pushl %ebp

   movl 8(%ebp), %esi                       # The address of the process block.

   movl 4(%esi), %eax                       # Move next task's ESP to EAX.
   movl 8(%esi), %ebx                       # Move next task's CR3 to EBX.

   movl %eax, %esp
   movl %ebx, %cr3

.end_process_switch:
   pop %ebp
   pop %edi
   pop %esi
   pop %ebx

   ret
