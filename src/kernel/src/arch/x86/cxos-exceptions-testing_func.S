#####################################################################
#  Test DIV0 exception
#
#  Triggers a divide-by-0 processor exception.
#####################################################################
.global __test_div0
.type __test_div0, @function
__test_div0:
	movl $0, %eax
	movl $0, %ecx
	idiv %ecx
	ret
