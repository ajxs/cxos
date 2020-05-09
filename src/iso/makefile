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

KERNEL_DIR    := ../kernel
KERNEL_BINARY := ${KERNEL_DIR}/build/cxos.elf

BUILD_DIR     := build
ISO           := ${BUILD_DIR}/cxos.img

.PHONY: all clean

all: ${ISO}

clean:
	rm -rf ${BUILD_DIR}

${ISO}: ${KERNEL_BINARY} ${BUILD_DIR}
	cp "${KERNEL_BINARY}" ./boot
	grub-mkrescue -o ${ISO} .

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}
