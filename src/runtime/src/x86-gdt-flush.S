.section .text

#####################################################################
#  GDT Flush
#
#  Instructs the processor on the location of the Global Descriptor 
#  Table and reloads the segment registers.
#####################################################################
.global __gdt_flush
.type __gdt_flush, @function
__gdt_flush:
	lgdt gdt_pointer

	# This far-jump instruction loads the value of '1' into the CS register. JMP is used 
	# here since we can't modify CS with the MOV instruction. The JMP instruction used with 
	# the format 'CS:OFFSET' will reload the CS register to the target of the jump.
	# Refer to: https://c9x.me/x86/html/file_module_x86_id_147.html
	# This refers to GDT entry 1 being the code segment.
	# This is because bit 1 << 3 is the index bit of the segment structure, the 0x8 here 
	# will set that bit to '1'.
	# Refer to 3.4.2 of the Intel SDM on page 95 for a description of the segment selector
	# structure. 
	# For a simple explanation, refer to: https://stackoverflow.com/a/23979175/5931673
	ljmp $0x0008, $.Lgdt_flush_reload_segments

.Lgdt_flush_reload_segments:
	mov $0x10, %ax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs
	mov %ax, %ss

	ret
