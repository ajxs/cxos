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

ARCH           := x86

BUILD_DIR      := build
LIB_DIR        := ${BUILD_DIR}/adalib
INCLUDE_DIR    := ${BUILD_DIR}/adainclude

RUNTIME_BINARY := ${LIB_DIR}/libgnat.a
RUNTIME_PROJ   := runtime

ARCH_SRC_DIR   := src/arch/${ARCH}
COMMON_SRC_DIR := src/arch/common

.PHONY: all clean

all: ${RUNTIME_BINARY}

clean:
	gprclean -P${RUNTIME_PROJ}
	rm -rf ${BUILD_DIR}

${RUNTIME_BINARY}: ${INCLUDE_DIR}/*.ad[sb] ${LIB_DIR}
	gprbuild -P${RUNTIME_PROJ}

${INCLUDE_DIR}/*.ad[sb]: ${ARCH_SRC_DIR}/*.ad[sb] ${COMMON_SRC_DIR}/*.ad[sb] ${INCLUDE_DIR}
	cp -a ${ARCH_SRC_DIR}/*.ad[sb] ${INCLUDE_DIR}
	cp -a ${COMMON_SRC_DIR}/*.ad[sb] ${INCLUDE_DIR}

${INCLUDE_DIR}:
	mkdir -p ${INCLUDE_DIR}

${LIB_DIR}:
	mkdir -p ${LIB_DIR}
