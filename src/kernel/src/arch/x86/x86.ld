/**
 *  Copyright (c) 2019, CXOS.
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Authors:
 *     Anthony <ajxs [at] panoptic.online>
 */

/** The physical starting address of the kernel. */
KERNEL_PHYS_START = 0x100000;
/**
 * The offset between the physical start and the start of the kernel's
 * mapping in virtual memory.
 */
KERNEL_VMA_OFFSET = 0xC0000000;
/** The size of the kernel stack. */
KERNEL_STACK_SIZE = 0x4000;
/** The size of the kernel secondary stack. */
KERNEL_SEC_STACK_SIZE = 0x1000;

ENTRY (_start)
SECTIONS {
	. = KERNEL_PHYS_START;
	. += KERNEL_VMA_OFFSET;
	kernel_start = .;

	.text ALIGN(4K) : AT (ADDR (.text) - KERNEL_VMA_OFFSET)
	{
		*(.multiboot)
		text_start = .;
		*(.text*)
		text_end = .;
	}

	.rodata ALIGN (4K) : AT (ADDR (.rodata) - KERNEL_VMA_OFFSET)
	{
		rodata_start = .;
		*(.rodata*)
		rodata_end = .;
	}

	.data ALIGN (4K) : AT (ADDR (.data) - KERNEL_VMA_OFFSET)
	{
		data_start = .;
		*(.data*)
		data_end = .;
	}

	.bss ALIGN (4K) : AT (ADDR (.bss) - KERNEL_VMA_OFFSET)
	{
		bss_start = .;
		*(COMMON)
		*(.bss*)

		. = ALIGN (4K);
		kernel_stack_bottom = .;
		. += KERNEL_STACK_SIZE;
		kernel_stack_top = .;

		. = ALIGN (4K);
    /** Secondary stack grows upwards, towards higher addresses. **/
		kernel_sec_stack_top = .;
		. += KERNEL_SEC_STACK_SIZE;
		kernel_sec_stack_bottom = .;

		/** Reserve 1MB for saving any needed multiboot data. */
		. = ALIGN (4K);
		multiboot_reserved_start = .;
		. += 1M;
		multiboot_reserved_end = .;

		/** Boot paging structures. */
		. = ALIGN (4K);
		boot_page_directory = .;
		. += 0x1000;
		boot_page_tables = .;
		/**
		 * Calculate the number of page tables necessary to hold the kernel.
		 * Skip the amount of space necessary to hold this amount of pages.
		 */
		. += (1 + ((KERNEL_PHYS_START + kernel_end - kernel_start) / 0x400000)) * 0x1000;

		kernel_stack_table = .;
		. += 0x1000;
		boot_graphics_table = .;
		. += 0x1000;
		boot_mapping_table = .;
		. += 0x1000;

		bss_end = .;
	}

	kernel_end = .;
}
