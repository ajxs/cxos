#####################################################################
#  Copyright (c) 2019, CXOS.
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  Authors:
#     Anthony <ajxs [at] panoptic.online>
#####################################################################

.section .text


#####################################################################
#  Macro for creating an ISR handler for an exception where no error
#  code is passed onto the stack automatically.
#  Accepts the exception number as a param.
#####################################################################
.macro ISR_WITHOUT_ERROR_CODE EXCODE
	.global __exception\EXCODE\()_entry
	.type __exception\EXCODE\()_entry, @function
	__exception\EXCODE\()_entry:
		cli
		# Push blank error code and ISR number.
		push $0
		push $\EXCODE
		jmp .Linterrupt_service_routine_entry
.endm


#####################################################################
#  Macro for creating an ISR handler for an exception where an error
#  code is passed onto the stack automatically.
#  Accepts the exception number as a param.
#####################################################################
.macro ISR_WITH_ERROR_CODE EXCODE
	.global __exception\EXCODE\()_entry
	.type __exception\EXCODE\()_entry, @function
	__exception\EXCODE\()_entry:
		cli
		# Error code is has already been pushed onto the stack.
		# Push ISR number.
		push $\EXCODE
		jmp .Linterrupt_service_routine_entry
.endm


#####################################################################
#  Common Interrupt Service Routine entry.
#
#  This is the common entry point for all interrupt service routine
#  entries. This will call the common exception handler function.
#  Additional values are automatically pushed onto the stack by the 
#  CPU: SS, USERESP, EFLAGS, CS, EIP.
#####################################################################
.Linterrupt_service_routine_entry:
	# Push all registers onto our exception handler stack frame.
	pushal
	push %ds
	push %es
	push %fs
	push %gs

	# Call the common exception handler procedure.
	call __exception_handler

	# Clear the stack.
	pop %gs
	pop %fs
	pop %es
	pop %ds
	popal

	# Clear the error code and ISR number from the stack.
	add $8, %esp
	iret


#####################################################################
#  Exception 0 Entry
#
#  The entry point for the Exception 0 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 0


#####################################################################
#  Exception 1 Entry
#
#  The entry point for the Exception 1 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 1


#####################################################################
#  Exception 2 Entry
#
#  The entry point for the Exception 2 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 2


#####################################################################
#  Exception 3 Entry
#
#  The entry point for the Exception 3 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 3


#####################################################################
#  Exception 4 Entry
#
#  The entry point for the Exception 4 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 4


#####################################################################
#  Exception 5 Entry
#
#  The entry point for the Exception 5 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 5


#####################################################################
#  Exception 6 Entry
#
#  The entry point for the Exception 6 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 6


#####################################################################
#  Exception 7 Entry
#
#  The entry point for the Exception 7 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 7


#####################################################################
#  Exception 8 Entry
#
#  The entry point for the Exception 8 handler.
#####################################################################
ISR_WITH_ERROR_CODE 8


#####################################################################
#  Exception 9 Entry
#
#  The entry point for the Exception 9 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 9


#####################################################################
#  Exception 10 Entry
#
#  The entry point for the Exception 10 handler.
#####################################################################
ISR_WITH_ERROR_CODE 10


#####################################################################
#  Exception 11 Entry
#
#  The entry point for the Exception 11 handler.
#####################################################################
ISR_WITH_ERROR_CODE 11


#####################################################################
#  Exception 12 Entry
#
#  The entry point for the Exception 12 handler.
#####################################################################
ISR_WITH_ERROR_CODE 12


#####################################################################
#  Exception 13 Entry
#
#  The entry point for the Exception 13 handler.
#####################################################################
ISR_WITH_ERROR_CODE 13


#####################################################################
#  Exception 14 Entry
#
#  The entry point for the Exception 14 handler.
#####################################################################
ISR_WITH_ERROR_CODE 14


#####################################################################
#  Exception 15 Entry
#
#  The entry point for the Exception 15 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 15

#####################################################################
#  Exception 16 Entry
#
#  The entry point for the Exception 16 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 16


#####################################################################
#  Exception 17 Entry
#
#  The entry point for the Exception 17 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 17


#####################################################################
#  Exception 18 Entry
#
#  The entry point for the Exception 18 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 18


#####################################################################
#  Exception 19 Entry
#
#  The entry point for the Exception 19 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 19


#####################################################################
#  Exception 20 Entry
#
#  The entry point for the Exception 20 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 20

#####################################################################
#  Exception 21 Entry
#
#  The entry point for the Exception 21 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 21


#####################################################################
#  Exception 22 Entry
#
#  The entry point for the Exception 22 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 22


#####################################################################
#  Exception 23 Entry
#
#  The entry point for the Exception 23 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 23


#####################################################################
#  Exception 24 Entry
#
#  The entry point for the Exception 24 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 24


#####################################################################
#  Exception 25 Entry
#
#  The entry point for the Exception 25 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 25


#####################################################################
#  Exception 26 Entry
#
#  The entry point for the Exception 26 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 26


#####################################################################
#  Exception 27 Entry
#
#  The entry point for the Exception 27 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 27


#####################################################################
#  Exception 28 Entry
#
#  The entry point for the Exception 28 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 28


#####################################################################
#  Exception 29 Entry
#
#  The entry point for the Exception 29 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 29


#####################################################################
#  Exception 30 Entry
#
#  The entry point for the Exception 30 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 30


#####################################################################
#  Exception 31 Entry
#
#  The entry point for the Exception 31 handler.
#####################################################################
ISR_WITHOUT_ERROR_CODE 31


