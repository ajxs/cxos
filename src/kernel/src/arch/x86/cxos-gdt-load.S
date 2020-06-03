#####################################################################
#  Copyright (c) 2019, CXOS.
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  Authors:
#     Anthony <ajxs [at] panoptic.online>
#####################################################################

.section .text

#####################################################################
#  GDT Load
#
#  Instructs the processor on the location of the Global Descriptor 
#  Table.
#####################################################################
.global cxos_gdt_load
.type cxos_gdt_load, @function
cxos_gdt_load:
	lgdt gdt_pointer
	ret
