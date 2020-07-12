# CXOS Kernel Ada Runtime

This directory contains the source for the Kernel's customised Ada runtime.
This runtime has been specifically constructed to target 'bareboard' x86 development. Unlike an Ada runtime for a hosted environment, this runtime is unable to provide most of the facilities outlined in the Ada Language Reference. The runtime contains the functions necessary for initialising the underlying x86 platform and launching the kernel.

