.set ALIGN,      1<<0
.set MEMINFO,    1<<1
.set FLAGS,      ALIGN | MEMINFO
.set MAGIC,      0x1BADB002
.set CHECKSUM,   -(MAGIC + FLAGS)
.set STACK_SIZE, 0x4000


.section .multiboot
.align 4
.long MAGIC
.long FLAGS
.long CHECKSUM

.align 4
.section .bss, "aw", @nobits
stack_bottom:
.skip STACK_SIZE
stack_top:

.section .text

.global _start
.type _start, @function
_start:
	cli

	movl $stack_top, %esp

	# Push Magic number and boot info address onto the stack as per Multiboot spec.
	# See: https://www.gnu.org/software/grub/manual/multiboot/multiboot.html#Machine-state
	pushl %ebx
	pushl %eax

	# Call Multiboot entry.
	call __multiboot_init

	# Initialise Ada runtime.
	call adainit

	# Initialise the system.
	call __system_init

	# Jump to the kernel entry.
	call _ada_kernel_entry

	cli
	hlt
.Lhang:
	jmp .Lhang
