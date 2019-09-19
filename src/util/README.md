# Utils

This folder contains various scripts and utilities to assist in setup and development.

## build_cross_gnat

This script contains a working recipe for building an Ada capable GCC cross-compiler. This script was developed and has been tested on Ubuntu 18.08. A Linux distribution will be required to use this script, results may vary by distro but the recipe shouldn't differ too much.

## debugrc

This script is loaded and executed automatically by GDB when debugging the kernel. This will automatically connect to the QEMU GDB stub launched when using the `/debug` script in the main repo directory. Commands to aid in debugging can be placed here.
