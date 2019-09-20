.section .text

.global _gdt_flush
.type _gdt_flush, @function
_gdt_flush:
	lgdt gdt_pointer
	ljmp $0x0008, $_gdt_complete_flush
_gdt_complete_flush:
	mov $0x10, %eax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs
	mov %ax, %ss
	ret
