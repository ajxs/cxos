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

with Interfaces;
with System;

-------------------------------------------------------------------------------
--  CXOS.VFS
--
--  Purpose:
--    This package contains functionality for interacting with the virtual
--    file system.
-------------------------------------------------------------------------------
package Cxos.VFS is
   pragma Preelaborate (Cxos.VFS);

   use Interfaces;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This function initialises the kernel's virtual file system.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Initialise return Kernel_Process_Result;

private
   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Drive_Map
   --
   --  Purpose:
   --    This function parses the Multiboot information structure's drive map.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Parse_Multiboot_Drive_Map (
     Drive_Map_Addr   : System.Address;
     Drive_Map_Length : Unsigned_32
   ) return Kernel_Process_Result;
end Cxos.VFS;
