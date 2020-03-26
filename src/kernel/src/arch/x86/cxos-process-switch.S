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
#  Calculate the number of page frames in the kernel stack.
#  Stores the result in EAX.
#####################################################################
.type calculate_kernel_stack_page_size, @function
calculate_kernel_stack_page_size:
   movl $0, %edx                            # Zero upper word of dividend.
   movl $KERNEL_STACK_SIZE, %eax            # Store KERNEL_STACK_SIZE in EAX.
   movl $0x1000, %ecx                       # Store size of page in ECX.
   divl %ecx

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

.preserve_registers:
   pushl %ebx
   pushl %esi
   pushl %edi
   pushl %ebp

.restore_memory_space:
   movl 8(%ebp), %esi                       # The address of the process block.
   movl 4(%esi), %eax                       # Move next task's ESP to EAX.
   movl 8(%esi), %ebx                       # Move next task's CR3 to EBX.

   movl %ebx, %cr3                          # Load new address space.
   movl %eax, %esp

.end_process_switch:
   pop %ebp
   pop %edi
   pop %esi
   pop %ebx

   ret
