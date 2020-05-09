# CXOS

## ISO
Contains the configuration files necessary for creating an ISO image from the built OS that can either be written to removable media or loaded directly by `qemu`. The `make debug` target will automatically load the generated ISO. The OS uses GRUB as a bootloader, the config files for which can be found in this directory.

## Kernel
The main kernel source code. The source files here are built together with the OS' custom Ada runtime to produce the final OS executable. Refer to the `README` file in this directory for more information.

## Runtime
Contains the OS customised Ada runtime targeting a bare-metal x86 system. The functionality for bootstrapping the system is contained inside this directory. Refer to the `README` file in this directory for more information.
