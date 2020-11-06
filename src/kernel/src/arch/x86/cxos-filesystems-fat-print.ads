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
--  CXOS.FILESYSTEMS.FAT.PRINT
--
--  Purpose:
--    This package contains functionality for printing information about
--    a particular FAT filesystem.
-------------------------------------------------------------------------------
package Cxos.Filesystems.FAT.Print is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Print_Filesystem_Info
   --
   --  Purpose:
   --    Prints information about a FAT formatted device.
   ----------------------------------------------------------------------------
   procedure Print_Filesystem_Info (Boot_Sec : Boot_Sector_T);

end Cxos.Filesystems.FAT.Print;
