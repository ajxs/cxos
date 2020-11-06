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

with Ada.Characters.Latin_1;
with Cxos.Error_Handling;
with Cxos.Devices.Storage.ATA; use Cxos.Devices.Storage.ATA;

package body Cxos.Devices is
   package Chars renames Ada.Characters.Latin_1;

   --  Error handler shorthand.
   procedure Log_Error (Message : String)
     renames Cxos.Error_Handling.Log_Kernel_Error;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
      Status : Cxos.Devices.Storage.ATA.Program_Status;
   begin
      Cxos.Devices.Storage.ATA.Find_ATA_Devices (Status);
      if Status /= Success then
         Log_Error ("Error finding ATA devices" & Chars.LF);
      end if;
   exception
      when Constraint_Error =>
         return;
   end Initialise;

end Cxos.Devices;
