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

# Store multiboot variables.
multiboot_magic:
	.long 0
multiboot_struct:
	.long 0

.section .text

.global _start
.type _start, @function
_start:
	# Disable interrupts prior to system initialisation.
	cli

	movl $stack_top, %esp

	# Save multiboot variables, since we want to pass these to the multiboot
	# initialisation function after having initialised the runtime.
	movl %eax, multiboot_magic
	movl %ebx, multiboot_struct

	# Initialise Ada runtime.
	call adainit

	# Retrieve the magic number and boot info struct address then push onto the stack as per 
	# the Multiboot spec.
	# See: https://www.gnu.org/software/grub/manual/multiboot/multiboot.html#Machine-state
	movl multiboot_magic, %eax
	movl multiboot_struct, %ebx

	pushl %ebx
	pushl %eax

	# Call Multiboot entry.
	call __multiboot_init

	# Initialise the system.
	call __system_init

	# Jump to the kernel entry.
	call _ada_kernel_entry

	cli
	hlt
.Lhang:
	jmp .Lhang
