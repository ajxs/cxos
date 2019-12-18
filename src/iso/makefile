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
