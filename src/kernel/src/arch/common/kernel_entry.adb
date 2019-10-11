-------------------------------------------------------------------------------
--  Copyright (c) 2019, CXOS.
--  This program is free software; you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3 of the License, or
--  (at your option) any later version.
--
--  Authors:
--     Anthony <ajxs [at] panoptic.online>
-------------------------------------------------------------------------------

with Cxos;

-------------------------------------------------------------------------------
--  Kernel_Entry
--
--  Purpose:
--    The Kernel_Entry procedure is the main entry point for the kernel.
--    This procedure is called from the boot code contained within the
--    kernel's Ada runtime, and serves as the transition between the boot
--    code and the kernel functionality.
--
-------------------------------------------------------------------------------
procedure Kernel_Entry is
begin
   --  Call the main kernel function.
   Cxos.Main;
end Kernel_Entry;
