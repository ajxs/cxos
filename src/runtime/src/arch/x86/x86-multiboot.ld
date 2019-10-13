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
  . = 0x00100000;

  .text BLOCK(4K) : ALIGN(4K) {
    *(.multiboot)
    *(.text)
  }

  .rodata ALIGN (0x1000) : {
    *(.rodata)
  }

  .data ALIGN (0x1000) : {
    *(.data)
  }

  .bss : {
    sbss = .;
    *(COMMON)
    *(.bss)
    ebss = .;

    /* Reserve space for page table entries. */
    . = ALIGN(4K);
    page_tables_start = .;
    . += (1024*1024*4);
    page_tables_end = .;
  }
}
