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

      Disk_Buffer : Cxos.Devices.ATA.ATA_Buffer;
      Buf_Str     : Cxos.Devices.ATA.ATA_String;
   begin
      Result := Cxos.Devices.PCI.Find_Pci_Devices;
      if Result /= Success then
         return;
      end if;

      Cxos.Devices.ATA.Find_ATA_Devices;

      Result := Cxos.Devices.ATA.Read_ATA_Device (Primary, Slave,
        0, 512, Disk_Buffer);
      if Result /= Success then
         return;
      end if;

      Buf_Str := Cxos.Devices.ATA.ATA_Buffer_To_String (Disk_Buffer);

      for I in Natural range 0 .. 511 loop
         Cxos.Debug.Put_String ("" & Buf_Str (I));
      end loop;

   exception
      when Constraint_Error =>
         return;
   end Initialise;

end Cxos.Devices;
