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
  }
}
