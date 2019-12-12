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

with Cxos.Memory.Map;
with System.Storage_Elements; use System.Storage_Elements;

package body Cxos.Memory.Paging is
   ----------------------------------------------------------------------------
   --  Create_New_Page_Directory
   --
   --  Purpose:
   --    Allocates and initialises a new page directory, placing the address
   --    of the newly allocated directory in the provided parameter.
   ----------------------------------------------------------------------------
   function Create_New_Page_Directory (
     Page_Directory_Addr : out System.Address;
   ) return Process_Result is
   begin
      Page_Directory_Addr := To_Address (0);
      return Success;
   end Create_New_Page_Directory;
end Cxos.Memory.Paging;
