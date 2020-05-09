#####################################################################
#  Copyright (c) 2020, CXOS.
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  Authors:
#     Anthony <ajxs [at] panoptic.online>
#####################################################################

.POSIX:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

BUILD_DIR     := build
KERNEL_BINARY := ${BUILD_DIR}/cxos.elf
KERNEL_PROJ   := kernel

.PHONY: all clean

all: ${KERNEL_BINARY}

clean:
	gprclean -P${KERNEL_PROJ}

${KERNEL_BINARY}:
	gprbuild -P${KERNEL_PROJ}
