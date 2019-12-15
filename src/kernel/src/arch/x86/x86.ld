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

KERNEL_VMA_START = 0xC0000000;
STACK_SIZE       = 0x4000;

ENTRY (_start)
SECTIONS {
  . = 0xC0100000;
  kernel_start = .;

  .text ALIGN(4K) : AT (ADDR (.text) - KERNEL_VMA_START)
  {
    *(.multiboot)
    *(.text*)
  }

  .rodata ALIGN (4K) : AT (ADDR (.rodata) - KERNEL_VMA_START)
  {
    *(.rodata*)
  }

  .data ALIGN (4K) : AT (ADDR (.data) - KERNEL_VMA_START)
  {
    *(.data*)
  }

  .bss ALIGN (4K) : AT (ADDR (.bss) - KERNEL_VMA_START)
  {
    bss_start = .;
    *(COMMON)
    *(.bss*)

    . = ALIGN (16);
    stack_bottom = .;
    . += STACK_SIZE;
    stack_top = .;

    . = ALIGN (4K);
    boot_page_directory = .;
    . += 0x1000;
    boot_page_table = .;
    . += 0x1000;

    bss_end = .;
  }

  kernel_end = .;
}
