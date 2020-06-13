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

with Cxos.Devices.Serial;

package body Cxos.Debug is
   ----------------------------------------------------------------------------
   --  Put_String
   --
   --  Implementation Notes:
   --    Prints output to COM1.
   ----------------------------------------------------------------------------
   procedure Put_String (
     Data : String
   ) is
   begin
      Cxos.Devices.Serial.Put_String (Data);
   end Put_String;

   ----------------------------------------------------------------------------
   --  Put_String_Wide
   ----------------------------------------------------------------------------
   procedure Put_String_Wide (
     Data : Wide_String
   ) is
   begin
      pragma Unreferenced (Data);
      null;
   end Put_String_Wide;

end Cxos.Debug;
