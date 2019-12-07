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
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Process Result type.
   --  Used to track the result of package processes.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Success,
     Unhandled_Exception
   );

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This function initialises the kernel's virtual file system.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Initialise return Process_Result;

end Cxos.VFS;
