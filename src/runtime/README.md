# CXOS Kernel Ada Runtime

This directory contains the source for the Kernel's customised Ada runtime.
This runtime has been specifically constructed to target 'bareboard' x86 development. Unlike an Ada runtime for a hosted environment, this runtime is unable to provide most of the facilities outlined in the Ada Language Reference. The runtime contains the functions necessary for initialising the underlying x86 platform and launching the kernel.

This runtime also contains the linker script responsible for linking the the kernel. This linker script can be found in: `src/x86-multiboot.ld`. This strategy of outlining the memory map in the runtime and using it to link the kernel executable was modeled after the Adacore bareboard runtimes for ARM provided by Adacore.

Execution of the OS begins in the `x86-multiboot-start.S` file. This file is responsible for bootstrapping the x86 system, taking control from the bootloader and initialising all of the necessary platform functionality needed by the operating system. Once the initialisation is complete, execution is handed over to the main kernel.
