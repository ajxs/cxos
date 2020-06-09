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

.PHONY: all clean

TEST_DISK_FAT12_IMG      := ./test_fat12.img
TEST_DISK_FAT12_IMG_SIZE := 2880

TEST_DISK_FAT16_IMG      := ./test_fat16.img

TEST_DISK_CONTENTS := $(shell ls -d ./test_disk_files/*)

all: ${TEST_DISK_FAT12_IMG} ${TEST_DISK_FAT16_IMG}

clean:
	rm -f ${TEST_DISK_FAT12_IMG} ${TEST_DISK_FAT16_IMG}

${TEST_DISK_FAT12_IMG}:
	dd if=/dev/zero of=${TEST_DISK_FAT12_IMG} bs=1k count=${TEST_DISK_FAT12_IMG_SIZE}
	mformat -i ${TEST_DISK_FAT12_IMG} -f ${TEST_DISK_FAT12_IMG_SIZE} ::
	mcopy -i ${TEST_DISK_FAT12_IMG} ${TEST_DISK_CONTENTS} ::

${TEST_DISK_FAT16_IMG}:
	dd if=/dev/zero of=${TEST_DISK_FAT16_IMG} count=16 bs=1M
	mkfs.fat -F 16 ${TEST_DISK_FAT16_IMG}
	mcopy -i ${TEST_DISK_FAT16_IMG} ${TEST_DISK_CONTENTS} ::
