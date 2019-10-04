.section .text


#####################################################################
#  IRQ0 Entry
#
#  The entry point for the IRQ0 handler.
#####################################################################
.global __irq0_entry
.type __irq0_entry, @function
__irq0_entry:
	pusha
	call __irq0_handler
	popa
	iret


#####################################################################
#  IRQ1 Entry
#
#  The entry point for the IRQ1 handler.
#####################################################################
.global __irq1_entry
.type __irq1_entry, @function
__irq1_entry:
	pusha
	call __irq1_handler
	popa
	iret
