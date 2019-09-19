RUNTIME_BINARY := ${LIB_DIR}/libgnat.a
RUNTIME_PROJ   := runtime

OBJ_DIR     :=  obj
LIB_DIR     := ${OBJ_DIR}/adalib
INCLUDE_DIR := ${OBJ_DIR}/adainclude

LINKER_SCRIPT  := x86-multiboot.ld
RUNTIME_CONFIG := runtime.xml

.PHONY: clean

all: ${RUNTIME_BINARY}

clean:
	rm -f ${OBJ_DIR}/${LINKER_SCRIPT}
	rm -f ${OBJ_DIR}/${RUNTIME_CONFIG}
	gprclean -P${RUNTIME_PROJ}
	rm -rf ${INCLUDE_DIR}
	rm -rf ${LIB_DIR}

${RUNTIME_BINARY}: ${INCLUDE_DIR}/*.ad[sb] ${INCLUDE_DIR}/*.S \
	${LIB_DIR} ${OBJ_DIR}/${LINKER_SCRIPT} ${OBJ_DIR}/${RUNTIME_CONFIG}
	gprbuild -P${RUNTIME_PROJ}

${INCLUDE_DIR}/*.ad[sb]: src/*.ad[sb] ${INCLUDE_DIR}
	cp -a src/* ${OBJ_DIR}/adainclude

${INCLUDE_DIR}/*.S: src/*.S ${INCLUDE_DIR}
	cp -a src/*.S ${OBJ_DIR}/adainclude

${OBJ_DIR}/${LINKER_SCRIPT}:
	cp -a src/${LINKER_SCRIPT} ${OBJ_DIR}/${LINKER_SCRIPT}

${OBJ_DIR}/${RUNTIME_CONFIG}:
	cp -a src/${RUNTIME_CONFIG} ${OBJ_DIR}/${RUNTIME_CONFIG}

${INCLUDE_DIR}:
	mkdir -p ${INCLUDE_DIR}

${LIB_DIR}:
	mkdir -p ${LIB_DIR}
