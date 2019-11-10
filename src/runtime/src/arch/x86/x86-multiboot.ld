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

ENTRY (_start)
SECTIONS {
  . = 0xC0100000;
  kernel_start = .;

  .text ALIGN(4K) : AT (ADDR (.text) - 0xC0000000)
  {
    *(.multiboot)
    *(.text*)
  }

  .rodata ALIGN (4K) : AT (ADDR (.rodata) - 0xC0000000)
  {
    *(.rodata*)
  }

  .data ALIGN (4K) : AT (ADDR (.data) - 0xC0000000)
  {
    *(.data*)
  }

  .bss ALIGN (4K) : AT (ADDR (.bss) - 0xC0000000)
  {
    bss_start = .;
    *(COMMON)
    *(.bss*)

    . = ALIGN (4K);
    boot_page_directory = .;
    . += 0x1000;
    boot_page_table = .;
    . += 0x1000;

    bss_end = .;
  }

  kernel_end = .;
}
