KERNEL_DIR      := kernel
KERNEL_BINARY   := ${KERNEL_DIR}/build/cxos.elf

RUNTIME_DIR     := runtime
RUNTIME_BINARY  := ${RUNTIME_DIR}/build/adalib/libgnat.a

BUILD_DIR       := ../build
ISO             := ${BUILD_DIR}/cxos.img

SERIAL_FILENAME := serial.log

QEMU_FLAGS :=                 \
	-accel tcg,thread=single    \
	-cpu core2duo               \
	-m 128                      \
	-no-reboot                  \
	-no-shutdown                \
	-smp 1                      \
	-usb                        \
	-vga std

.POSIX:
.PHONY: clean debug emu runtime kernel

all: ${ISO}

${ISO}: ${KERNEL_BINARY} ${BUILD_DIR}
	cp "${KERNEL_BINARY}" iso/boot
	grub-mkrescue -o ${ISO} iso

clean:
	# Ensure the kernel directory is cleaned before the rts dir.
	# gprclean needs to see that there is a valid rts to perform any operations.
	make clean -C "${KERNEL_DIR}"
	make clean -C "${RUNTIME_DIR}"

debug: ${ISO}
	qemu-system-i386                   \
		${QEMU_FLAGS}                    \
		-drive format=raw,file=${ISO}    \
		-d cpu_reset,int,guest_errors    \
		-gdb tcp::1234                   \
		-S                               \
		-serial file:${SERIAL_FILENAME}

emu: ${ISO}
	qemu-system-i386                   \
		${QEMU_FLAGS}                    \
		-drive format=raw,file=${ISO}    \
		-serial stdio                    \

kernel: ${KERNEL_BINARY}

runtime: ${RUNTIME_BINARY}

${RUNTIME_BINARY}:
	make -C "${RUNTIME_DIR}"

${KERNEL_BINARY}: ${RUNTIME_BINARY}
	make -C "${KERNEL_DIR}"

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}
