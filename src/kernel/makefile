KERNEL_BINARY := ${KERNEL_DIR}/build/cxos.elf
KERNEL_PROJ   := kernel

.PHONY: clean

all: ${KERNEL_BINARY}

clean:
	gprclean -P${KERNEL_PROJ}

${KERNEL_BINARY}:
	gprbuild -P${KERNEL_PROJ}
