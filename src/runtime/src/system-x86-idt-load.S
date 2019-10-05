.section .text

#####################################################################
#  IDT Load
#
#  Instructs the processor on the location of the Interrupt
#  Descriptor Table.
#####################################################################
.global __idt_load
.type __idt_load, @function
__idt_load:
	lidt idt_pointer
	ret
