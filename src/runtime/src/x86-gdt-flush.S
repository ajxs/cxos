.section .text

.global _gdt_flush
.type _gdt_flush, @function
_gdt_flush:
	# Clear interrupts
	cli

	lgdt gdt_pointer
	ljmp $0x0008, $.Lreload_segment
.Lreload_segment:
	mov $0x10, %eax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs
	mov %ax, %ss

	# Re-enable interrupts
	# sti

	ret
