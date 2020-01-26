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

.section .bss

#####################################################################
# Multiboot Memory Map Info struct
#####################################################################
.global multiboot_mmap_info
multiboot_mmap_info:
multiboot_mmap_addr:
	.skip 4
multiboot_mmap_length:
	.skip 4
multiboot_mmap_present:
	.skip 1

#####################################################################
# Multiboot drive info struct.
#####################################################################
.global multiboot_drive_info
multiboot_drive_info:
multiboot_drive_addr:
	.skip 4
multiboot_drive_length:
	.skip 4
multiboot_drive_present:
	.skip 1


.section .text

#####################################################################
# Copy Multiboot Data
#####################################################################
.global copy_multiboot_data
.type copy_multiboot_data, @function
copy_multiboot_data:
	# Move the reserved memory addr to EDI.
	# This will stay constant throughout so that we can save the value of EDI
	# as the pointers to individual sections.
	movl $multiboot_reserved_start, %edi

# Copy the mmap to a reserved area of memory in bss.
.copy_multiboot_mmap:
	movl $KERNEL_VMA_OFFSET, %edx            # Store higher-half offset in EDX.
	movl $multiboot_struct_ptr, %esi
	subl %edx, %esi                          # Subtract higher-half offset.

	movl (%esi), %esi                        # Copy pointer address into ESI.

	# Check flags for mmap being present.
	movl (%esi), %eax                        # Copy multiboot flags into EAX.
	andl $0x20, %eax
	cmpl $0, %eax
	# If this flag is not set, skip copying this section data.
	je .end_copy_multiboot_mmap

	addl $44, %esi                           # Add offset to the mmap length.

	movl $multiboot_mmap_length, %ebx        # Store length var address in EBX.
	movl (%esi), %eax                        # Copy length into reserved var.

	movl %eax, (%ebx)                        # Copy length to address in EDI.

	movl %eax, %ecx                          # Copy length into ECX.

	addl $4, %esi                            # Increment ESI to point to mmap addr.
	movl (%esi), %eax                        # Move value contained at ESI to EAX.

	movl %eax, %esi                          # Move value back to ESI.

	movl $multiboot_mmap_addr, %eax          # Copy mmap target to EAX.
	movl %edi, (%eax)                        # Copy memory address to global var.

	movl $multiboot_mmap_present, %eax       # Copy target addr to EAX.
	movl $1, (%eax)                          # Write a true value.

	rep movsb                                # Copy from ESI to EDI
.end_copy_multiboot_mmap:

.copy_multiboot_drive_map:
	movl $KERNEL_VMA_OFFSET, %edx            # Store higher-half offset in EDX.
	movl $multiboot_struct_ptr, %esi
	subl %edx, %esi                          # Subtract higher-half offset.

	movl (%esi), %esi                        # Copy pointer address into ESI.

	# Check flags for drive map being present.
	movl (%esi), %eax                        # Copy multiboot flags into EAX.
	andl $0x40, %eax
	cmpl $0, %eax
	# If this flag is not set, skip copying this section data.
	je .end_copy_multiboot_drive_map

	addl $52, %esi                           # Add offset to the drive map length.

	movl $multiboot_drive_length, %ebx       # Store length var address in EBX.
	movl (%esi), %eax                        # Copy length into reserved var.
	movl %eax, (%ebx)                        # Copy length to address in EDI.

	movl %eax, %ecx                          # Copy length into ECX.

	addl $4, %esi                            # Increment ESI to point to map addr.
	movl (%esi), %eax                        # Move value contained at ESI to EAX.

	movl %eax, %esi                          # Move value back to ESI.

	movl $multiboot_drive_addr, %eax         # Copy drive map target to EAX.
	movl %edi, (%eax)                        # Copy memory address to global var.

	movl $multiboot_drive_present, %eax      # Copy target addr to EAX.
	movl $1, (%eax)                          # Write a true value.

	rep movsb                                # Copy from ESI to EDI
.end_copy_multiboot_drive_map:

.end_copy_multiboot_data:
	ret

