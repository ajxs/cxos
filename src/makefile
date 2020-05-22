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

.PHONY: all clean debug emu runtime kernel

KERNEL_DIR      := kernel
KERNEL_BINARY   := ${KERNEL_DIR}/build/cxos.elf

RUNTIME_DIR     := runtime
RUNTIME_BINARY  := ${RUNTIME_DIR}/build/adalib/libgnat.a

ISO_DIR         := iso
ISO             := ${ISO_DIR}/build/cxos.img

SERIAL_FILENAME := serial.log

TEST_DISK_IMG      := ../test.fat
TEST_DISK_IMG_SIZE := 2880

TEST_DRV_IMG       := ../test2.fat

QEMU_FLAGS :=                                  \
	-accel tcg,thread=single                     \
	-boot order=dc                               \
	-cpu core2duo                                \
	-drive format=raw,media=cdrom,file=${ISO}    \
	-drive format=raw,media=disk,file=${TEST_DISK_IMG} \
	-m 128                                       \
	-machine pc                                  \
	-no-reboot                                   \
	-smp 1                                       \
	-usb                                         \
	-vga std

all: ${ISO} ${TEST_DISK_IMG} ${TEST_DRV_IMG}

${ISO}: ${KERNEL_BINARY} ${BUILD_DIR}
	make -C "${ISO_DIR}"

clean:
	# Ensure the kernel directory is cleaned before the rts dir.
	# gprclean needs to see that there is a valid rts to perform any operations.
	make clean -C "${ISO_DIR}"
	make clean -C "${KERNEL_DIR}"
	make clean -C "${RUNTIME_DIR}"
	rm -f ${TEST_DISK_IMG}
	rm -f ${TEST_DRV_IMG}

debug: ${ISO}
	qemu-system-i386                               \
		${QEMU_FLAGS}                                \
		-nographic                                   \
		-d cpu_reset,int,guest_errors                \
		-gdb tcp::1234                               \
		-S                                           \
		-serial file:${SERIAL_FILENAME}

emu: ${ISO}
	qemu-system-i386                               \
		${QEMU_FLAGS}                                \
		-serial stdio

bochs: ${ISO}
	bochs -f ./src/util/bochsrc

kernel: ${KERNEL_BINARY}

runtime: ${RUNTIME_BINARY}

iso: ${ISO}

${RUNTIME_BINARY}:
	make -C "${RUNTIME_DIR}"

${KERNEL_BINARY}: ${RUNTIME_BINARY}
	make -C "${KERNEL_DIR}"

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}

${TEST_DISK_IMG}:
	dd if=/dev/zero of=${TEST_DISK_IMG} bs=1k count=${TEST_DISK_IMG_SIZE}
	mformat -i ${TEST_DISK_IMG} -f ${TEST_DISK_IMG_SIZE} ::
	mcopy -i ${TEST_DISK_IMG} README.md ::/README.md

${TEST_DRV_IMG}:
	dd if=/dev/zero of=${TEST_DRV_IMG} count=1 bs=1M
	mkfs.fat -F 16 ${TEST_DRV_IMG}
	mcopy -i ${TEST_DRV_IMG} README.md ::/README.md
