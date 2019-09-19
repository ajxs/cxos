# CXOS

A toy example operating-system written in Ada. It contains only extremely minimal functionality to serve as an example for setting up bare-metal systems development in Ada.

The operating system is built for the x86 platform and requires an Ada2012 compiler targeting the `i686-elf` platform. A recipe for compiling a GCC cross-compiler from `x86-64-linux-elf` to `i686-elf` from source can be found within the `src/util/build_cross_gnat` script.
