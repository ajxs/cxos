.section .text

#####################################################################
#  GDT Load
#
#  Instructs the processor on the location of the Global Descriptor 
#  Table.
#####################################################################
.global __gdt_load
.type __gdt_load, @function
__gdt_load:
	lgdt gdt_pointer
	ret
