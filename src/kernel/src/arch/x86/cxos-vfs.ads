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

-------------------------------------------------------------------------------
--  CXOS.VFS
--
--  Purpose:
--    This package contains functionality for interacting with the virtual
--    file system.
-------------------------------------------------------------------------------
package Cxos.VFS is
   pragma Preelaborate (Cxos.VFS);

   function Initialise return Kernel_Process_Result;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Drive_Map
   --
   --  Purpose:
   --    This function parses the Multiboot information structure's drive map.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Parse_Multiboot_Drive_Map (
     Memory_Map_Addr   : System.Address;
     Memory_Map_Length : Unsigned_32
   ) return Kernel_Process_Result;
end Cxos.VFS;

