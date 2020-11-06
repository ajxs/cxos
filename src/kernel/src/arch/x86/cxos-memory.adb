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

package body Cxos.Memory is
   ----------------------------------------------------------------------------
   --  Allocate_Kernel_Memory
   ----------------------------------------------------------------------------
   procedure Allocate_Kernel_Memory (
     Size   :     Positive;
     Addr   : out System.Address;
     Status : out Process_Result
   ) is
   begin
      Addr := System.Null_Address;

      pragma Unreferenced (Size);

      Status := Success;
   end Allocate_Kernel_Memory;

end Cxos.Memory;
