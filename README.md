# CXOS

## Background
CXOS is a personal research project operating-system implemented in Ada. The current aims of the project are to learn the fundamentals of operating system development and research the benefits provided by the Ada programming language for engineering safety-critical bare-metal software.

The operating system currently exclusively targets the x86 platform. An ARM/MIPS implementation is being considered for the long-term.

## Current status
CXOS currently contains only an extremely minimal implementation, offering no interactivity. Currently CXOS has a working higher-half kernel, physical memory manager and several peripheral drivers. Currently development is focused on reaching a working tasking implementation and userland.

## Project goals
The long-term goals of the project as of time-of-writing are to implement a minimal non-graphical user-interface and basic user applications. After this has been achieved, planning will begin on a more robust and planned implementation more focused on security and efficiency. Future porting to RISC architectures is being considered.

## Name
The name `cxos` was chosen at random, based upon the precedent set by my previous operating-system development research projects. All of which were given a working title of `_xos`, where `_` is a random letter. The original research project was called`jxos`, named by a stylised amalgamation of `ajxs` and `os`. Once CXOS reaches a point of development where more specific, purposeful architectual design choices become relevant and development of userland applications begins a more fitting, long-term name will be selected.

## Development
CXOS requires an Ada2012 compiler targeting the `i686-elf` platform. A recipe for compiling a GCC cross-compiler from `x86-64-linux-elf` to `i686-elf` from source can be found within the `src/util/build_cross_gnat` script. Contributions are not presently being solicited from the community, however any advice would definitely be appreciated.

While I would welcome any help on the project, work has not yet reached a point where development is following a structured plan or can be easily subdivided amongst multiple developers. I would however absolutely welcome anyone submitting pull requests to resolve any issues they encounter.

