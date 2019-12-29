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
#  Switch to process
#
#  Switches to the provided process.
#####################################################################
.type switch_to_process, @function
.global switch_to_process
switch_to_process:
	cli
	hlt
