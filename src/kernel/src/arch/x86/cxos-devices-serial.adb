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

with x86.Serial;

package body Cxos.Devices.Serial is
   ----------------------------------------------------------------------------
   --  Put_String
   ----------------------------------------------------------------------------
   procedure Put_String (
     Data : String
   ) is
   begin
      x86.Serial.Put_String (x86.Serial.COM1, Data);
   end Put_String;
end Cxos.Devices.Serial;
