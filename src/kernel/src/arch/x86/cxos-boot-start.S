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


.section .multiboot
#####################################################################
#  Multiboot header
#####################################################################
	.long MAGIC
	.long FLAGS
	.long CHECKSUM


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
.global multiboot_struct
multiboot_struct:
	.skip 4


.section .text

#####################################################################
# Calculate page count
#
# Calculates the number of page frames that exist between two
# addresses and returns that value to the caller in EAX.
#####################################################################
.type calculate_page_count, @function
calculate_page_count:
	pushl %ebp
	movl %esp, %ebp

	pushl %ebx

	# The start of the section.
	movl 8(%ebp), %ebx
	# The end of the section.
	movl 12(%ebp), %eax

	# Compute length of text section.
	subl %ebx, %eax
	# Length of text section is now in EAX.
	# Divide length by 4kb to get number of page frames in section.
	movl $0x1000, %ebx
	movl $0, %edx
	divl %ebx
	# Number of page frames in text section is now in EAX.
	# Add 1 for the correct amount.
	incl %eax

	popl %ebx

	mov %ebp, %esp
	pop %ebp
	ret


#####################################################################
# Map Section
#
# void map_section(uint32_t source, uint32_t dest,
#   uint32_t frame_count, uint32_t flags);
# This function maps an individual section.
# It maps the provided number of page frames from the source address
# onwards into the page tables pointed to by dest.
#####################################################################
.type map_section, @function
map_section:
	pushl %ebp
	movl %esp, %ebp

	pushl %edi
	pushl %esi
	pushl %ebx

	# The source address for the section.
	movl 8(%ebp), %esi
	# The page table to map the provided address into.
	movl 12(%ebp), %edi
	# The number of page frames to map.
	movl 16(%ebp), %ecx
	# The flags for the pages.
	movl 20(%ebp), %ebx

.map_section_frame:
	# Move page address into EDX.
	movl %esi, %edx
	# Apply the permission flags.
	orl %ebx, %edx
	# Write page structure to entry address.
	movl %edx, (%edi)

.increment_section_frame_address:
	# Add 4kb to the source address to point to the next frame.
	addl $0x1000, %esi
	# Size of entries in boot_page_tables is 4 bytes.
	addl $4, %edi

	# Loop until frame counter in ECX is 0.
	loop .map_section_frame

	popl %ebx
	popl %esi
	popl %edi

	mov %ebp, %esp
	pop %ebp
	ret


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
	# Store the kernel's VMA offset in EAX.
	movl $KERNEL_VMA_OFFSET, %eax
	# Store the page table entry address in the destination register.
	movl $boot_page_tables, %edi
	# Subtract VMA offset.
	subl %eax, %edi

	# The permission bitmask for the page.
	# Supervisor x Present x Writeable.
	pushl $3
	# The number of entries to map.
	pushl $256
	pushl %edi
	# The page frame address to map.
	pushl $0
	call map_section
	addl $16, %esp


.map_text_section:
	# Map the text section.
	# Calculate the number of pages contained in this section.
	# Page count is stored in EAX.
	pushl $text_end
	pushl $text_start
	call calculate_page_count
	addl $8, %esp
	movl %eax, %ecx

	# Store the kernel's VMA offset in EAX.
	movl $KERNEL_VMA_OFFSET, %eax

	# Store the page frame entry in the destination register.
	# The offset of 1024 corresponds to the physical start of the kernel at 1MB.
	movl $boot_page_tables + (256*4), %edi
	subl %eax, %edi             # Subtract VMA offset.
	# Store the page frame address in the source register.
	movl $text_start, %esi
	subl %eax, %esi             # Subtract VMA offset.

	# Supervisor x Present.
	pushl $0x1
	pushl %ecx
	pushl %edi
	pushl %esi

	# Increment destination page table address by (page_count * 4).
	# Calculate offset for the page table.
	movl $4, %ebx
	movl %ecx, %eax
	mull %ebx
	# Add offset to current destination value.
	addl %eax, %edi

	call map_section
	addl $16, %esp

.map_rodata_section:
	# Page count is stored in ECX.
	pushl $rodata_end
	pushl $rodata_start
	call calculate_page_count
	addl $8, %esp
	movl %eax, %ecx

	# Store the kernel's VMA offset in EAX.
	movl $KERNEL_VMA_OFFSET, %eax

	# Store the page frame address in the source register.
	movl $rodata_start, %esi
	subl %eax, %esi             # Subtract VMA offset.

	# Supervisor x Present.
	pushl $0x1
	pushl %ecx
	pushl %edi
	pushl %esi

	# Increment destination page_table address.
	movl $4, %ebx
	movl %ecx, %eax
	mull %ebx
	addl %eax, %edi

	call map_section
	addl $16, %esp

.map_data_section:
	# Page count is stored in ECX.
	pushl $data_end
	pushl $data_start
	call calculate_page_count
	addl $8, %esp
	movl %eax, %ecx

	# Store the kernel's VMA offset in EAX.
	movl $KERNEL_VMA_OFFSET, %eax

	# Store the page frame address in the source register.
	movl $data_start, %esi
	subl %eax, %esi             # Subtract VMA offset.

	# The permission bitmask for the page.
	# Supervisor x Present x Writeable.
	pushl $0x3
	pushl %ecx
	pushl %edi
	pushl %esi

	# Increment destination page_table address.
	movl $4, %ebx
	movl %ecx, %eax
	mull %ebx
	addl %eax, %edi

	call map_section
	addl $16, %esp

.map_bss_section:
	# Page count is stored in ECX.
	pushl $bss_end
	pushl $bss_start
	call calculate_page_count
	addl $8, %esp
	movl %eax, %ecx

	# Store the kernel's VMA offset in EAX.
	movl $KERNEL_VMA_OFFSET, %eax

	# Store the page frame address in the source register.
	movl $bss_start, %esi
	subl %eax, %esi             # Subtract VMA offset.

	# The permission bitmask for the page.
	# Supervisor x Present x Writeable.
	pushl $0x3
	pushl %ecx
	pushl %edi
	pushl %esi
	call map_section
	addl $16, %esp

# Map the final entry in the page directory to itself.
.recursively_map_boot_directory:
	# Store target address in EBX.
	movl $boot_page_directory, %ebx
	# Subtract VMA offset.
	subl %eax, %ebx
	# Store offset into page dir in ECX.
	movl $(1023 * 4), %ecx
	# Add page dir offset to target address.
	addl %ecx, %ebx

	# Load page directory address into EDX.
	movl $boot_page_directory, %edx
	# Subtract VMA offset.
	subl %eax, %edx
	# Apply flags: Supervisor x Present x Writeable.
	orl $3, %edx
	# Move to final entry in the page directory.
	movl %edx, (%ebx)

# Map the kernel mapping table to index 1022 in the page directory.
.map_mapping_table:
	# Store target address in EBX.
	movl $boot_page_directory, %ebx
	# Subtract VMA offset.
	subl %eax, %ebx
	# Store offset into page dir in ECX.
	movl $(1022 * 4), %ecx
	# Add page dir offset to target address.
	addl %ecx, %ebx

# Load the mapping table address into EDX.
	movl $boot_mapping_table, %edx
	# Subtract VMA offset.
	subl %eax, %edx
	# Apply flags: Supervisor x Present x Writeable.
	orl $3, %edx
	# Move to the correct entry in the page directory.
	movl %edx, (%ebx)

.map_kernel_pages_in_directory:
	# Map the boot page table to virtual addresses 0x00000000 and 0xC0000000.
	# This allows for execution to continue sequentially immediately
	# after enabling paging.

.calculate_kernel_page_count:
	# Calculate number of pages required to map the kernel.
	# Store the length of the kernel in EAX.
	movl $kernel_end, %eax
	movl $kernel_start, %ebx
	subl %ebx, %eax

	# Divide by the size of a page frame.
	movl $0, %edx
	movl $0x400000, %ebx
	divl %ebx
	# Increment by 1, since we are calculating the page count.
	incl %eax
	# Store the page count in ECX.
	movl %eax, %ecx

.calculate_higher_half_directory_offset:
	# Store the offset into the page directory in ECX.
	movl $KERNEL_VMA_OFFSET, %eax
	movl $0, %edx
	movl $0x100000, %ebx
	# Divide the kernel VMA offset by the size of a page frame divided 4.
	# This will give us the correct byte offset into the page directory
	# to map the kernel pages.
	# e.g 0xC0000000 / (0x400000 /4) = (768*4 bytes).
	divl %ebx
	# Move result back to EBX.
	movl %eax, %ebx

.insert_kernel_pages:
	movl $KERNEL_VMA_OFFSET, %eax

	# Move address of boot page table into EDX.
	movl $boot_page_tables, %esi
	subl %eax, %esi                    # Subtract VMA offset.
	# Apply flags: Supervisor x Present x Writeable.
	orl $3, %esi

	# Store boot page directory offset 0 address in EDI.
	movl $boot_page_directory, %edi
	subl %eax, %edi                    # Subtract VMA offset.

	# Store boot page directory higher-half address in EDX.
	movl $boot_page_directory, %edx
	subl %eax, %edx                    # Subtract VMA offset.
	# Add higher-half offset into the page directory to map to the
	# appropriate higher-half kernel entry into the page directory.
	addl %ebx, %edx

.map_table:
	# Identity map low memory.
	movl %esi, (%edi)
	# Map higher-half kernel address.
	movl %esi, (%edx)

	addl $0x1000, %esi
	addl $4, %edi
	addl $4, %edx

	loop .map_table

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

	# Save multiboot variables in global variables.
	# These are used in the kernel start procedure to authenticate
	# the bootloader.
	# See: https://www.gnu.org/software/grub/manual/multiboot/multiboot.html#Machine-state

	# Move kernel VMA offset into EDX.
	movl $KERNEL_VMA_OFFSET, %edx

	# Move multiboot_magic address into ECX.
	movl $multiboot_magic, %ecx
	# Subtract VMA offset.
	subl %edx, %ecx
	# Move magic number value into the reserved variable address.
	movl %eax, (%ecx)

	# Move multiboot_struct address into ECX.
	movl $multiboot_struct, %ecx
	# Subtract VMA offset.
	subl %edx, %ecx
	# Move struct address into the reserved variable address.
	movl %ebx, (%ecx)

	# Initialise the boot page structures.
	call init_boot_page_directory

	# Move VMA offset into EAX.
	movl $KERNEL_VMA_OFFSET, %eax

	# Load address of the page directory into CR3.
	movl $boot_page_directory, %ecx
	# Subtract VMA offset.
	subl %eax, %ecx
	# Move the address into CR3.
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

	# Initialise the system.
	call __system_init

	# Initialise the kernel.
	call __kernel_init

	# Jump to the kernel main loop.
	call __main

	cli
	hlt

	# Halt the machine indefinitely if execution reaches this point.
	jmp .boot_hang
