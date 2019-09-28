.section .text

.global _idt_load
.type _idt_load, @function
_idt_load:
	lidt idt_pointer
