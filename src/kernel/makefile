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
