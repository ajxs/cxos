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
with Cxos.Debug;
with Cxos.Devices.ATA;
with Cxos.Devices.PCI;
with x86.ATA;

package body Cxos.Devices is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
      use x86.ATA;

      --  The result of internal functionality.
      Result : Process_Result;

      Read_Buf    : Cxos.Devices.ATA.ATA_Read_Buffer (0 .. 255);
   begin
      Result := Cxos.Devices.PCI.Find_Pci_Devices;
      if Result /= Success then
         return;
      end if;

      Cxos.Devices.ATA.Find_ATA_Devices;

      Result := Cxos.Devices.ATA.Read_ATA_Device (Primary, Slave,
        1, 0, Read_Buf);
      if Result /= Success then
         Cxos.Debug.Put_String ("Read Error." & Ada.Characters.Latin_1.LF);
         return;
      end if;

   exception
      when Constraint_Error =>
         return;
   end Initialise;

end Cxos.Devices;
