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
.type cxos_process_switch_to_process, @function
.global cxos_process_switch_to_process
cxos_process_switch_to_process:
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

.setup_kernel_stack:
   movl $0xFFFFC000, %edi                   # Move address of page table entry into EDI.
   movl $4, %ecx                            # Kernel stack size is 4 page frames.

.setup_stack_frame:
   movl (%edi), %edx                        # Move page table entry value into EDX.
   andl $0xFF, %edx                         # Preserve flags in page dir entry.
   orl %eax, %edx                           # Move EAX into address field of PTE.
   movl %edx, (%edi)                        # Move value back into PTE.

   addl $0x1000, %eax                       # Increment stack frame address.
   addl $0x4, %edi                          # Increment PTE address.

   loop .setup_stack_frame

# Flush the TLB to load the new mapping.
.flush_tlb:
   movl %ebx, %cr3

.load_kernel_stack:
	 movl $0xFF003FE8, %esp                   # Set ESP.

.end_process_switch:
   pop %ebp
   pop %edi
   pop %esi
   pop %ebx

   ret
