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
  . = 1M;

  .text BLOCK(4K) : ALIGN(4K) {
    kernel_start = .;
    *(.multiboot)
    *(.text)
    kernel_end = .;
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

    /* Reserve space for Page Table entries. */
    . = ALIGN(4K);
    page_tables_start = .;
    . += (1024*1024*4);
    page_tables_end = .;

    /* Reserve space for Page Directory. */
    . = ALIGN(4K);
    page_directory_start = .;
    . += (1024*4);
    page_directory_end = .;
  }
}
