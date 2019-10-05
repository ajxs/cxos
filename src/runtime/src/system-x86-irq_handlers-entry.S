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


#####################################################################
#  IRQ2 Entry
#
#  The entry point for the IRQ2 handler.
#####################################################################
.global __irq2_entry
.type __irq2_entry, @function
__irq2_entry:
	pusha
	call __irq2_handler
	popa
	iret


#####################################################################
#  IRQ3 Entry
#
#  The entry point for the IRQ3 handler.
#####################################################################
.global __irq3_entry
.type __irq3_entry, @function
__irq3_entry:
	pusha
	call __irq3_handler
	popa
	iret


#####################################################################
#  IRQ4 Entry
#
#  The entry point for the IRQ4 handler.
#####################################################################
.global __irq4_entry
.type __irq4_entry, @function
__irq4_entry:
	pusha
	call __irq4_handler
	popa
	iret


#####################################################################
#  IRQ5 Entry
#
#  The entry point for the IRQ5 handler.
#####################################################################
.global __irq5_entry
.type __irq5_entry, @function
__irq5_entry:
	pusha
	call __irq5_handler
	popa
	iret


#####################################################################
#  IRQ6 Entry
#
#  The entry point for the IRQ6 handler.
#####################################################################
.global __irq6_entry
.type __irq6_entry, @function
__irq6_entry:
	pusha
	call __irq6_handler
	popa
	iret


#####################################################################
#  IRQ7 Entry
#
#  The entry point for the IRQ7 handler.
#####################################################################
.global __irq7_entry
.type __irq7_entry, @function
__irq7_entry:
	pusha
	call __irq7_handler
	popa
	iret


#####################################################################
#  IRQ8 Entry
#
#  The entry point for the IRQ8 handler.
#####################################################################
.global __irq8_entry
.type __irq8_entry, @function
__irq8_entry:
	pusha
	call __irq8_handler
	popa
	iret


#####################################################################
#  IRQ9 Entry
#
#  The entry point for the IRQ9 handler.
#####################################################################
.global __irq9_entry
.type __irq9_entry, @function
__irq9_entry:
	pusha
	call __irq9_handler
	popa
	iret


#####################################################################
#  IRQ10 Entry
#
#  The entry point for the IRQ10 handler.
#####################################################################
.global __irq10_entry
.type __irq10_entry, @function
__irq10_entry:
	pusha
	call __irq10_handler
	popa
	iret


#####################################################################
#  IRQ11 Entry
#
#  The entry point for the IRQ11 handler.
#####################################################################
.global __irq11_entry
.type __irq11_entry, @function
__irq11_entry:
	pusha
	call __irq11_handler
	popa
	iret


#####################################################################
#  IRQ12 Entry
#
#  The entry point for the IRQ12 handler.
#####################################################################
.global __irq12_entry
.type __irq12_entry, @function
__irq12_entry:
	pusha
	call __irq12_handler
	popa
	iret


#####################################################################
#  IRQ13 Entry
#
#  The entry point for the IRQ13 handler.
#####################################################################
.global __irq13_entry
.type __irq13_entry, @function
__irq13_entry:
	pusha
	call __irq13_handler
	popa
	iret


#####################################################################
#  IRQ14 Entry
#
#  The entry point for the IRQ14 handler.
#####################################################################
.global __irq14_entry
.type __irq14_entry, @function
__irq14_entry:
	pusha
	call __irq14_handler
	popa
	iret


#####################################################################
#  IRQ15 Entry
#
#  The entry point for the IRQ15 handler.
#####################################################################
.global __irq15_entry
.type __irq15_entry, @function
__irq15_entry:
	pusha
	call __irq15_handler
	popa
	iret
