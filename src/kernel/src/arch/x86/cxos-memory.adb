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

with System.Machine_Code;

package body Cxos.Memory is
   ----------------------------------------------------------------------------
   --  Current_Page_Dir_Ptr
   ----------------------------------------------------------------------------
   function Current_Page_Dir_Ptr return System.Address is
      CR3 : System.Address;
   begin
      System.Machine_Code.Asm (
        Template => "movl %%cr3, %0",
        Outputs => (
          System.Address'Asm_Output ("=a", CR3)
        ),
        Volatile => True);

      --  Return the final entry in the currently loaded page directory.
      return CR3;
   end Current_Page_Dir_Ptr;

end Cxos.Memory;
