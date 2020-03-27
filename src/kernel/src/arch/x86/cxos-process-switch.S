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

.type cxos_process_save_process_state, @function
.global cxos_process_save_process_state
cxos_process_save_process_state:
   pushl %ebp
   movl %esp, %ebp

   pushl %esi                               # Preserve ESI.

   movl 8(%ebp), %esi                       # The address of the process block.
   movl %esp, 4(%esi)

   pushl %eax                               # Preserve EAX.
   movl %cr3, %eax
   movl %eax, 8(%esi)

   popl %eax                                # Restore EAX.
   movl %ebx, 16(%esi)
   movl %ecx, 20(%esi)
   movl %edx, 24(%esi)
   movl %edi, 28(%esi)

   movl %esi, %edi
   popl %esi                                # Restore ESI.
   movl %esi, 32(%edi)

   movl %ebp, 36(%edi)

   mov %ebp, %esp
   pop %ebp
   ret


#####################################################################
#  Load process
#
#  Switches to the provided process.
#####################################################################
.type cxos_process_load_process, @function
.global cxos_process_load_process
cxos_process_load_process:
   pushl %ebp
   movl %esp, %ebp

   movl 8(%ebp), %esi                       # The address of the process block.
   movl 4(%esi), %eax                       # Move next task's ESP to EAX.
   movl 8(%esi), %ebx                       # Move next task's CR3 to EBX.

   movl %ebx, %cr3                          # Load new address space.
   movl %eax, %esp

   ret
