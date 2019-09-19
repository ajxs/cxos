# CXOS Kernel

The sources for the main Kernel are contained within this directory. The main entry point for the kernel is contained within the `kernel_entry.adb` file.
Assembly code for bootstrapping the system and handing control over to the kernel is contained within the `runtime/x86-multiboot-start.S` file.

For additional details regarding bootstrapping the system refer to the documentation and sources contained within the `runtime` directory.
