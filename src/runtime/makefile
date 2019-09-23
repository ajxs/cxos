RUNTIME_BINARY := ${LIB_DIR}/libgnat.a
RUNTIME_PROJ   := runtime

BUILD_DIR      := build
LIB_DIR        := ${BUILD_DIR}/adalib
INCLUDE_DIR    := ${BUILD_DIR}/adainclude

LINKER_SCRIPT  := x86-multiboot.ld
RUNTIME_CONFIG := runtime.xml

.PHONY: clean

all: ${RUNTIME_BINARY}

clean:
	gprclean -P${RUNTIME_PROJ}
	rm -rf ${BUILD_DIR}

${RUNTIME_BINARY}: ${INCLUDE_DIR}/*.ad[sb] ${INCLUDE_DIR}/*.S \
	${LIB_DIR} ${BUILD_DIR}/${LINKER_SCRIPT} ${BUILD_DIR}/${RUNTIME_CONFIG}
	gprbuild -P${RUNTIME_PROJ}

${INCLUDE_DIR}/*.ad[sb]: src/*.ad[sb] ${INCLUDE_DIR}
	cp -a src/*.ad[sb] ${INCLUDE_DIR}

${INCLUDE_DIR}/*.S: src/*.S ${INCLUDE_DIR}
	cp -a src/*.S ${INCLUDE_DIR}

${BUILD_DIR}/${LINKER_SCRIPT}:
	cp -a src/${LINKER_SCRIPT} ${BUILD_DIR}/${LINKER_SCRIPT}

${BUILD_DIR}/${RUNTIME_CONFIG}:
	cp -a src/${RUNTIME_CONFIG} ${BUILD_DIR}/${RUNTIME_CONFIG}

${INCLUDE_DIR}:
	mkdir -p ${INCLUDE_DIR}

${LIB_DIR}:
	mkdir -p ${LIB_DIR}
