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

	# Check whether the length is zero, if so, exit.
	cmpl $0, %eax
	je .end_calculate_page_count

	# Length of text section is now in EAX.
	# Divide length by 4kb to get number of page frames in section.
	movl $0x1000, %ebx
	movl $0, %edx
	divl %ebx
	# Number of page frames in text section is now in EAX.

	# Check whether there is a remainder, if not, exit.
	cmpl $0, %edx
	je .end_calculate_page_count

	# Add 1 for the correct amount to take into account that the section may not
	# use a complete frame.
	incl %eax

.end_calculate_page_count:
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

	movl 8(%ebp), %esi                       # The source address for the section.
	movl 12(%ebp), %edi                      # The table to map the provided address into.
	movl 16(%ebp), %ecx                      # The number of page frames to map.
	movl 20(%ebp), %ebx                      # The flags to apply to the pages.

.map_section_frame:
	movl %esi, %edx                          # Move page address into EDX.
	orl %ebx, %edx                           # Apply the permission flags.
	movl %edx, (%edi)                        # Write page structure to entry address.

.increment_section_frame_address:
	addl $0x1000, %esi                       # Add 4kb to ESI to point to the next frame.
	addl $4, %edi                            # Add 4 bytes to point to next entry.

	loop .map_section_frame                  # Loop until frame counter in ECX is 0.

	popl %ebx
	popl %esi
	popl %edi

	mov %ebp, %esp
	pop %ebp
	ret


calculate_higher_half_directory_offset:
	# Store the offset into the page directory in ECX.
	movl $KERNEL_VMA_OFFSET, %eax
	movl $0, %edx
	movl $0x400000, %ebx
	# Divide the kernel VMA offset by the size of a page frame.
	divl %ebx

	cmpl $0, %edx                            # Check if there is a remainder
	je .end_calculate_higher_half_directory_offset

	incl %eax                                # Increment by 1 due to remainder.

.end_calculate_higher_half_directory_offset:
	ret

#####################################################################
#  Initialise Boot Page Directory
#
#  Initialises the paging structures necessary to map the kernel
#  to the required virtual memory addresses.
#  These structures are used only during the boot process to jump to
#  the correct virtual memory address.
#####################################################################
.global init_boot_page_directory
.type init_boot_page_directory, @function
init_boot_page_directory:
# Identity map lowest 1MB.
.map_low_memory:
	movl $KERNEL_VMA_OFFSET, %eax            # Store the kernel's VMA offset in EAX.
	movl $boot_page_tables, %edi             # Store the table address in EDX.
	subl %eax, %edi                          # Subtract VMA offset.

	pushl $3                                 # Flags: Supervisor x Present x Writeable.
	pushl $256                               # The number of entries to map.
	pushl %edi
	pushl $0                                 # The page frame address to map.
	call map_section
	addl $16, %esp

.map_sections:

.calculate_physical_offset:
	# Calculate the destination page table entry corresponding to the physical
	# starting offset of the kernel.
	# This assumes that the physical offset is 4KB aligned.
	# The resulting address of the boot_page_tables to begin copying to will be
	# stored in EDI.
	movl $KERNEL_PHYS_START, %eax            # Store the physical kernel offset in EAX.
	movl $0, %edx
	movl $0x1000, %ebx                       # Divide by the number of page frames.
	divl %ebx

	movl $4, %ebx
	mull %ebx                                # Multiply to get the byte offset.
	movl $boot_page_tables, %edi
	addl %eax, %edi                          # Add the offset

	movl $KERNEL_VMA_OFFSET, %eax            # Store the kernel's VMA offset in EAX.
	subl %eax, %edi                          # Subtract VMA offset.

.map_text_section:
	# Map the text section.
	# Calculate the number of pages contained in this section.
	# Page count is stored in EAX.
	pushl $text_end
	pushl $text_start
	call calculate_page_count
	addl $8, %esp
	movl %eax, %ecx

	# Store the page frame entry in the destination register.
	# The offset of 1024 corresponds to the physical start of the kernel at 1MB.
	# Store the page frame address in the source register.
	movl $text_start, %esi

	movl $KERNEL_VMA_OFFSET, %eax
	subl %eax, %esi                          # Subtract VMA offset.

	pushl $1                                 # Flags: Supervisor x Present.
	pushl %ecx
	pushl %edi
	pushl %esi

	# Increment destination page table address by (page_count * 4).
	# Calculate offset for the page table.
	movl $4, %ebx
	movl %ecx, %eax
	mull %ebx
	addl %eax, %edi                          # Add offset to current destination value.

	call map_section
	addl $16, %esp

.map_rodata_section:
	pushl $rodata_end
	pushl $rodata_start
	call calculate_page_count
	addl $8, %esp
	movl %eax, %ecx                          # Page count is stored in ECX.

	# If there are no pages to map, skip this operation.
	cmpl $0, %ecx
	je .map_data_section

	movl $KERNEL_VMA_OFFSET, %eax            # Store the kernel's VMA offset in EAX.

	# Store the page frame address in the source register.
	movl $rodata_start, %esi
	subl %eax, %esi                          # Subtract VMA offset.

	pushl $1                                 # Flags: Supervisor x Present.
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
	pushl $data_end
	pushl $data_start
	call calculate_page_count
	addl $8, %esp
	movl %eax, %ecx                          # Page count is stored in ECX.

	# If there are no pages to map, skip this operation.
	cmpl $0, %ecx
	je .map_bss_section

	# Store the kernel's VMA offset in EAX.
	movl $KERNEL_VMA_OFFSET, %eax

	# Store the page frame address in the source register.
	movl $data_start, %esi
	subl %eax, %esi                          # Subtract VMA offset.

	pushl $3                                 # Flags: Supervisor x Present x Writeable.
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
	pushl $bss_end
	pushl $bss_start
	call calculate_page_count
	addl $8, %esp
	movl %eax, %ecx                          # Page count is stored in ECX.

	# If there are no pages to map, skip this operation.
	cmpl $0, %ecx
	je .end_map_sections

	movl $KERNEL_VMA_OFFSET, %eax            # Store the kernel's VMA offset in EAX.

	# Store the page frame address in the source register.
	movl $bss_start, %esi
	subl %eax, %esi                          # Subtract VMA offset.

	pushl $3                                 # Flags: Supervisor x Present x Writeable.
	pushl %ecx
	pushl %edi
	pushl %esi
	call map_section
	addl $16, %esp

.end_map_sections:

# Map the table used to hold the graphics buffer.
.map_graphics_table:
	movl $boot_page_directory, %ebx          # Store target address in EBX.
	subl %eax, %ebx                          # Subtract VMA offset.
	movl $(1021 * 4), %ecx                   # Store offset into page dir in ECX.
	addl %ecx, %ebx                          # Add page dir offset to target address.

	movl $boot_graphics_table, %edx          # Load page directory address into EDX.
	subl %eax, %edx                          # Subtract VMA offset.
	orl $3, %edx                             # Flags: Supervisor x Present x Writeable.
	movl %edx, (%ebx)                        # Move to ideal entry in the page directory.

# Map the VGA buffer into the first entry in the graphics table.
# This maps the buffer to 0xFF400000.
.map_vga_buffer:
	movl $boot_graphics_table, %edi
	subl %eax, %edi                          # Subtract VMA offset.
	movl $0xB8003, (%edi)

# Map the second last entry in the page directory to the mapping table.
# This table is reserved for mapping in new paging structures.
.map_mapping_table:
	movl $boot_page_directory, %ebx          # Store target address in EBX.
	subl %eax, %ebx                          # Subtract VMA offset.
	movl $(1022 * 4), %ecx                   # Store offset into page dir in ECX.
	addl %ecx, %ebx                          # Add page dir offset to target address.

	movl $boot_mapping_table, %edx           # Load page table address into EDX.
	subl %eax, %edx                          # Subtract VMA offset.
	orl $3, %edx                             # Flags: Supervisor x Present x Writeable.
	movl %edx, (%ebx)                        # Move to ideal entry in the page directory.

# Map the kernel stack table.
.map_kernel_stack_table:
	movl $boot_page_directory, %ebx          # Store target address in EBX.
	subl %eax, %ebx                          # Subtract VMA offset.
	movl $(1020 * 4), %ecx                   # Store offset into page dir in ECX.
	addl %ecx, %ebx                          # Add page dir offset to target address.

	movl $kernel_stack_table, %edx           # Load page table address into EDX.
	subl %eax, %edx                          # Subtract VMA offset.
	orl $3, %edx                             # Flags: Supervisor x Present x Writeable.
	movl %edx, (%ebx)                        # Move to ideal entry in the page directory.

# Map the kernel stack into the stack table.
.map_kernel_stack:
	movl $kernel_stack_table, %edi           # Move system table address into EDI.
	subl %eax, %edi                          # Subtract VMA offset.

	movl $kernel_stack_bottom, %esi          # Move the stack bottom address into ESI.
	subl %eax, %esi                          # Subtract VMA offset.
	orl $3, %esi                             # Apply flags.

	movl $4, %ecx                            # Map 4 stack frames.
.map_kernel_stack_frame:
	movl %esi, (%edi)
	addl $4, %edi
	addl $0x1000, %esi
	loop .map_kernel_stack_frame

.end_kernel_stack_frame:



# Map the kernel stack into the stack table.
.map_kernel_sec_stack:
	movl $kernel_stack_table, %edi           # Move system table address into EDI.
	addl $512, %edi                          # Start at entry 512 in the stack table.
	subl %eax, %edi                          # Subtract VMA offset.

# Note: The secondary stack grows upwards, towards higher addresses.
	movl $kernel_sec_stack_top, %esi         # Move the stack top address into ESI.
	subl %eax, %esi                          # Subtract VMA offset.
	orl $3, %esi                             # Apply flags.

	movl $4, %ecx                            # Map 4 stack frames.
.map_kernel_sec_stack_frame:
	movl %esi, (%edi)
	addl $4, %edi
	addl $0x1000, %esi
	loop .map_kernel_sec_stack_frame

.end_kernel_sec_stack_frame:




# Map the final entry in the page directory to itself.
.recursively_map_boot_directory:
	movl $boot_page_directory, %ebx          # Store target address in EBX.
	subl %eax, %ebx                          # Subtract VMA offset.
	movl $(1023 * 4), %ecx                   # Store offset into page dir in ECX.
	addl %ecx, %ebx                          # Add page dir offset to target address.

	movl $boot_page_directory, %edx          # Load page directory address into EDX.
	subl %eax, %edx                          # Subtract VMA offset.
	orl $3, %edx                             # Flags: Supervisor x Present x Writeable.
	movl %edx, (%ebx)                        # Move to final entry in the page directory.

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

	movl $0, %edx
	movl $0x400000, %ebx                     # Divide by the size of a page frame.
	divl %ebx

	# If there is any remainder, increment the page count by one.
	cmpl $0, %edx
	je .end_calculate_kernel_page_count

	incl %eax                                # Increment due to the remainder.

.end_calculate_kernel_page_count:
	movl %eax, %ecx                          # Store the page count in ECX.

	# Calculate the higher half page directory offset.
	call calculate_higher_half_directory_offset
	# This will give us the correct byte offset into the page directory
	# to map the kernel pages.
	# e.g 0xC0000000 / (0x400000 /4) = (768*4 bytes).
	movl $4, %ebx
	mull %ebx
	movl %eax, %ebx                          # Move result back to EBX.

.insert_kernel_pages:
	movl $KERNEL_VMA_OFFSET, %eax

	movl $boot_page_tables, %esi             # Move address of boot page table into EDX.
	subl %eax, %esi                          # Subtract VMA offset.
	orl $3, %esi                             # Flags: Supervisor x Present x Writeable.

	movl $boot_page_directory, %edi          # Store page-dir offset 0 address in EDI.
	subl %eax, %edi                          # Subtract VMA offset.

	movl $boot_page_directory, %edx          # Store page-dir higher-half address in EDX.
	subl %eax, %edx                          # Subtract VMA offset.
	# Add higher-half offset into the page directory to map to the
	# appropriate higher-half kernel entry into the page directory.
	addl %ebx, %edx

.map_table:
	movl %esi, (%edi)                        # Identity map low memory.
	movl %esi, (%edx)                        # Map higher-half kernel address.

	addl $0x1000, %esi                       # Increment source address by 4KB
	addl $4, %edi                            # Increment destination addresses by 4 bytes.
	addl $4, %edx

	loop .map_table

.end_init_boot_page_directory:
	ret


#####################################################################
# Remove Identity Mapping.
#
# This function removes the kernel identity mapping after the jump
# to the higher-half.
#####################################################################
.global remove_identity_mapping
remove_identity_mapping:
	# Calculate the higher half page directory offset.
	call calculate_higher_half_directory_offset
	movl %eax, %ecx                          # Move result to ECX.
	movl $boot_page_directory, %edi

.remove_map_loop:
	movl $0, (%edi)
	addl $4, %edi

	loop .remove_map_loop

.end_remove_identity_mapping:
	ret


