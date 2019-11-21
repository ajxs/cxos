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

.set ALIGN,      1<<0
.set MEMINFO,    1<<1
.set FLAGS,      ALIGN | MEMINFO
.set MAGIC,      0x1BADB002
.set CHECKSUM,   -(MAGIC + FLAGS)
.set STACK_SIZE, 0x4000


.section .multiboot
#####################################################################
#  Multiboot header
#####################################################################
	.long MAGIC
	.long FLAGS
	.long CHECKSUM


.section .bss, "aw", @nobits

# Store multiboot variables.
.global multiboot_magic
multiboot_magic:
	.skip 4
.global multiboot_struct
multiboot_struct:
	.skip 4


.section .text

#####################################################################
#  Initialise Boot Page Directory
#
#  Initialises the paging structures necessary to map the kernel
#  to the required virtual memory addresses.
#  These structures are used only during the boot process to jump to
#  the correct virtual memory address.
#####################################################################
.type init_boot_page_directory, @function
init_boot_page_directory:
# Identity map lowest 1MB.
.map_low_memory:
	# The page table entry address.
	movl $(boot_page_table - 0xC0000000), %edi
	# The page frame address.
	movl $0, %esi
	# The number of entries to map.
	movl $256, %ecx

.map_low_memory_frame:
	# Move page address into EDX.
	movl %esi, %edx
	# Apply flags: Supervisor x Present x Writeable.
	orl $0x3, %edx
	# Write page structure to entry address.
	movl %edx, (%edi)

.increment_low_frame_address:
	# Size of page frame is 4096 bytes.
	addl $0x1000, %esi
	# Size of entries in boot_page_table is 4 bytes.
	addl $4, %edi

	loop .map_low_memory_frame

# Map kernel memory.
# Creates both the identity and vma offset mapping.
.map_kernel_memory:
	# The page table entry address.
	movl $(boot_page_table + 1024) - 0xC0000000, %edi
	# The page frame address.
	movl $kernel_start - 0xC0000000, %esi

.map_kernel_frame:
	# Check that we're only mapping the kernel.
	cmpl $kernel_end - 0xC0000000, %esi
	jge .recursively_map_boot_directory

	# Move physical address of page frame into EDX.
	movl %esi, %edx
	# Apply flags: Supervisor x Preset x Writeable.
	orl $3, %edx
	movl %edx, (%edi)

.increment_frame_address:
	# Size of page is 4096 bytes.
	addl $0x1000, %esi
	# Size of entries in boot_page_table is 4 bytes.
	addl $4, %edi

	jmp .map_kernel_frame

# Map the final entry in the page directory to itself.
.recursively_map_boot_directory:
	# Load page directory address into EDX.
	movl $boot_page_directory - 0xC0000000, %edx
	# Apply flags: Supervisor x Present x Writeable.
	orl $3, %edx
	# Move to final entry in page directory.
	movl %edx, boot_page_directory - 0xC0000000 + (1023 * 4)

.load_mapped_page_tables:
	# Move address of boot page table into EDX.
	movl $boot_page_table - 0xC0000000, %edx
	# Apply flags: Supervisor x Present x Writeable.
	orl $3, %edx

	# Map the boot page table to virtual addresses 0x00000000 and 0xC0000000.
	# This allows for execution to continue sequentially immediately after enabling paging.
	movl %edx, boot_page_directory - 0xC0000000
	movl %edx, boot_page_directory - 0xC0000000 + (768 * 4)

	ret


#####################################################################
#  Boot Hang
#
#  Fixes the machine in an endless loop.
#  Used to halt the processor in the case of a boot error.
#####################################################################
.boot_hang:
	jmp .boot_hang


#####################################################################
#  Boot entry point
#
#  Main boot entry point.
#  Execution begins here.
#####################################################################
.global _start
.type _start, @function
_start:
	# Disable interrupts prior to system initialisation.
	cli

	# Save multiboot variables, since we want to pass these to the system
	# initialisation function after having initialised the runtime.
	movl %eax, multiboot_magic - 0xC0000000
	movl %ebx, multiboot_struct - 0xC0000000

	# Initialise the boot page structures.
	call init_boot_page_directory

	# Load address of the page directory into CR3.
	movl $boot_page_directory - 0xC0000000, %ecx
	movl %ecx, %cr3

	# Set write-protect and paging-enabled flags.
	movl %cr0, %ecx
	orl $0x80010000, %ecx
	movl %ecx, %cr0

	# Perform an absolute jump to higher half.
	lea .higher_half_jump, %ecx
	jmp *%ecx

.higher_half_jump:
	# Setup stack.
	movl $stack_top, %esp

	# Initialise Ada runtime.
	call adainit

	# Retrieve the magic number and boot info struct address then push onto the
	# stack as per the Multiboot spec.
	# See: https://www.gnu.org/software/grub/manual/multiboot/multiboot.html#Machine-state
	movl multiboot_magic, %eax
	movl multiboot_struct, %ebx

	pushl %ebx
	pushl %eax

	# Initialise the system.
	call __system_init

	# Jump to the kernel entry.
	call _ada_kernel_entry

	cli
	hlt

	# Halt the machine indefinitely if execution reaches this point.
	jmp .boot_hang
