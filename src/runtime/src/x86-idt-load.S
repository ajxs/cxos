.section .text

.global __idt_load
.type __idt_load, @function
__idt_load:
	lidt idt_pointer
	ret
