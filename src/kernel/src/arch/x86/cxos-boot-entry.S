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

.set MULTIBOOT_BOOTLOADER_MAGIC, 0x1BADB002
.set MULTIBOOT_HEADER_FLAGS,     0x3
.set MULTIBOOT_HEADER_CHECKSUM,  -(MULTIBOOT_BOOTLOADER_MAGIC + MULTIBOOT_HEADER_FLAGS)

.section .multiboot
#####################################################################
#  Multiboot header
#####################################################################
.align 4
	.long MULTIBOOT_BOOTLOADER_MAGIC
	.long MULTIBOOT_HEADER_FLAGS
	.long MULTIBOOT_HEADER_CHECKSUM

.section .bss, "aw", @nobits

#####################################################################
# Multiboot Variables
# These are stored globally so they are acessible to the kernel
# during kernel initialisation.
# Since the runtime and hardware is initialised first, these sre
# stored here to not rely on the stack or registers to store these.
#####################################################################
.global multiboot_magic
multiboot_magic:
	.skip 4
.global multiboot_struct_ptr
multiboot_struct_ptr:
	.skip 4


.section .text

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

	# Save multiboot variables in global variables.
	# These are used in the kernel start procedure to authenticate
	# the bootloader.
	# See: https://www.gnu.org/software/grub/manual/multiboot/multiboot.html#Machine-state

	movl $KERNEL_VMA_OFFSET, %edx            # Move kernel VMA offset into EDX.

	movl $multiboot_magic, %ecx              # Move multiboot_magic address into ECX.
	subl %edx, %ecx                          # Subtract VMA offset.
	movl %eax, (%ecx)                        # Move magic number into the reserved address.

	movl $multiboot_struct_ptr, %ecx         # Move multiboot_struct address into ECX.
	subl %edx, %ecx                          # Subtract VMA offset.
	movl %ebx, (%ecx)                        # Move struct to the reserved address.

	# Initialise the boot page structures.
	call init_boot_page_directory

	# Move VMA offset into EAX.
	movl $KERNEL_VMA_OFFSET, %eax

	# Load page directory.
	movl $boot_page_directory, %ecx          # Load address of the page dir into CR3.
	subl %eax, %ecx                          # Subtract VMA offset.
	movl %ecx, %cr3                          # Move the address into CR3.

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

	# Initialise the system.
	# This procedure is defined within the runtime. It performs the initial
	# initialisation of the x86 platform required by the kernel, such as loading
	# the GDT/IDT.
	call __system_init

	# Copy Multiboot data to a reserved location where it can be accessed by the
	# kernel after low memory mapping is removed.
	call copy_multiboot_data

	# Remove the boot paging identity mapping.
	call remove_identity_mapping

	# Initialise the kernel.
	call __kernel_init

	# Jump to the kernel main loop.
	call __main

	cli
	hlt

	# Halt the machine indefinitely if execution reaches this point.
	jmp .boot_hang
